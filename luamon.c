#include <stdbool.h>
#include <stdlib.h>

#include <readline/history.h>
#include <readline/readline.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "deux.h"

//TODO
// display block
//   show values for non-pointers
//   show brief target info for pointers
// allocate new block
// set value in block
// emit code into block
//   implement all assembler instructions
// initiate gc
//   how to make lua values be roots?
// save image
// load image
// invoke activation record/code block

extern struct heap * heap0;
extern void (* pre_collect_hook)();
extern void (* add_roots_hook)(struct block_header **);
extern void (* update_roots_hook)();

int do_print_block(lua_State * lua);
void new_block(lua_State * lua, struct block_header * header);
struct block_header * get_addr_from_ledger(lua_State * lua, int n);

lua_State * lua_state_for_gc_hooks;

void run_lua_gc() {
    lua_gc(lua_state_for_gc_hooks, LUA_GCCOLLECT, 0);
}

void get_roots_from_lua(struct block_header ** mark_list_head) {
    lua_State * lua = lua_state_for_gc_hooks;

    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    // iterate over ledger
    lua_pushnil(lua);
    while (lua_next(lua, -2)) {
        struct block_header * header = lua_touserdata(lua, -1);
        // mark blocks and add to mark list if not already present
        if (! header->marked) {
            header->link_ptr = * mark_list_head;
            header->marked = true;
            //printf("hook mark %x\n", header); //XXX
            * mark_list_head = header;
        }
        //else printf("hook skip (mark) %x\n", header); //XXX

        lua_pop(lua, 1);
    }

    lua_pop(lua, 1);
}

void update_roots_in_lua() {
    lua_State * lua = lua_state_for_gc_hooks;

    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    // iterate over ledger
    lua_pushnil(lua);
    while (lua_next(lua, -2)) {
        // copy the ledger key (need to use twice)
        lua_pushvalue(lua, -2);
        lua_insert(lua, -2);

        // update address in ledger
        struct block_header * header = lua_touserdata(lua, -1);
        lua_pop(lua, 1);
        lua_pushlightuserdata(lua, header->link_ptr);
        lua_settable(lua, -4);
    }

    lua_pop(lua, 1);
}

void setup_gc_hooks(lua_State * lua) {
    lua_state_for_gc_hooks = lua;
    pre_collect_hook = run_lua_gc;
    add_roots_hook = get_roots_from_lua;
    update_roots_hook = update_roots_in_lua;
}

int block_tostring(lua_State * lua) {
    void * obj = luaL_checkudata(lua, 1, "block");

    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    lua_pushvalue(lua, 1);   // ledger key is block argument
    lua_gettable(lua, -2);   // look up block header address in ledger

    struct block_header * header = lua_touserdata(lua, -1);

    char * note = (char *) (& header->note);
    lua_pushfstring(lua, "%p: layout=%s size=%dx%d=%d note=%c%c%c%c",
                    header, layout_str(header->layout),
                    header->size/sizeof(void *), sizeof(void *), header->size,
                    note[0], note[1], note[2], note[3]);
    return 1;
}

int block_index(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");

    if (lua_type(lua, 2) == LUA_TSTRING) {
        lua_pushliteral(lua, "print");
        if (!lua_rawequal(lua, -1, -2)) luaL_argerror(lua, 2, "no such method");

        lua_pop(lua, 2);
        lua_pushcfunction(lua, do_print_block);
        return 1;
    }

    int ix = luaL_checkinteger(lua, 2);

    struct block_header * header = get_addr_from_ledger(lua, 1);
    if (0 <= ix && ix < header->size / sizeof(void *)) {
        void * value = header->data[ix];

        switch (header->layout) {
        case no_ptr_layout:
            lua_pushinteger(lua, (int) value);
            break;

        case all_ptr_layout:
            switch ((uint32_t) value & 0b11) {
            case 0b00:   // pointer
                if (in_heap(value)) new_block(lua, get_header(value));
                else lua_pushlightuserdata(lua, value);
                break;
            case 0b11:   // uint32_t
                lua_pushinteger(lua, untag(value));
                break;
            default:
                die("unhandled value tag");
            }
            break;

        default:
            die("unhandled block layout");
        }
    }
    else luaL_argerror(lua, 2, "out of bounds");

    return 1;
}

int block_newindex(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    int ix = luaL_checkinteger(lua, 2);

    struct block_header * header = get_addr_from_ledger(lua, 1);
    if (0 <= ix && ix < header->size / sizeof(void *)) {
        switch (header->layout) {
        case no_ptr_layout:
            switch (lua_type(lua, 3)) {
            case LUA_TNUMBER:
                header->data[ix] = (void *) ((int) lua_tointeger(lua, 3));
                break;
            case LUA_TLIGHTUSERDATA:
                header->data[ix] = lua_touserdata(lua, 3);
                break;
            case LUA_TUSERDATA:
                header->data[ix] = get_addr_from_ledger(lua, 3)->data;
                break;
            default:
                luaL_argerror(lua, 3, "bad argument");
            }
            break;

        case all_ptr_layout:
            switch (lua_type(lua, 3)) {
            case LUA_TNUMBER:
                header->data[ix] = tagint(lua_tointeger(lua, 3));
                break;
            case LUA_TLIGHTUSERDATA:
                header->data[ix] = lua_touserdata(lua, 3);
                break;
            case LUA_TUSERDATA:
                header->data[ix] = get_addr_from_ledger(lua, 3)->data;
                break;
            default:
                luaL_argerror(lua, 3, "bad argument");
            }
            break;

        default:
            die("unhandled block layout");
        }
    }
    else luaL_argerror(lua, 2, "out of bounds");

    return 0;
}

int block_same(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    luaL_checkudata(lua, 2, "block");

    bool result = get_addr_from_ledger(lua, 1) == get_addr_from_ledger(lua, 2);
    lua_pushboolean(lua, result);
    return 1;
}

void setup_block_metatable(lua_State * lua) {
    luaL_newmetatable(lua, "block");

    lua_pushcfunction(lua, block_tostring);
    lua_setfield(lua, -2, "__tostring");

    lua_pushcfunction(lua, block_index);
    lua_setfield(lua, -2, "__index");

    lua_pushcfunction(lua, block_newindex);
    lua_setfield(lua, -2, "__newindex");

    /*
    lua_pushcfunction(lua, block_same);
    lua_setfield(lua, -2, "__eq");
    */

    lua_pop(lua, 1);
}

int do_hexdump(lua_State * lua) {
    int addr = luaL_checkinteger(lua, 1);
    int length = luaL_checkinteger(lua, 2);
    //TODO catch segfault and throw lua error
    print_hexdump((void *) addr, length);
    return 0;
}

int do_get_heap_addr(lua_State * lua) {
    lua_pushinteger(lua, (int) heap0);
    return 1;
}

int do_print_block_at(lua_State * lua) {
    int addr = luaL_checkinteger(lua, 1);
    //TODO catch segfault and throw lua error
    print_block((struct block_header *) addr);
    return 0;
}

struct block_header * get_addr_from_ledger(lua_State * lua, int n) {
    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    lua_pushvalue(lua, n);   // ledger key is block object
    lua_gettable(lua, -2);   // look up block header address in ledger

    return lua_touserdata(lua, -1);
}

int do_print_block(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    struct block_header * header = get_addr_from_ledger(lua, 1);
    //TODO catch segfault and throw lua error
    print_block(header);
    return 0;
}

int do_hex(lua_State * lua) {
    int n = luaL_checkinteger(lua, 1);
    char s[19];
    snprintf(s, 19, "0x%x", n);
    lua_pushstring(lua, s);
    return 1;
}

void new_block(lua_State * lua, struct block_header * header) {
    // new block object
    lua_newuserdata(lua, 1);
    luaL_setmetatable(lua, "block");

    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    lua_pushvalue(lua, -2);   // ledger key is new block object
    lua_pushlightuserdata(lua, header);  // ledger value is block header address
    lua_settable(lua, -3);   // enter new block in ledger

    lua_pop(lua, 1);   // remove ledger, leave new block object on stack
}

int get_root_block(lua_State * lua) {
    new_block(lua, heap0->roots[0]);
    return 1;
}

int do_alloc_data(lua_State * lua) {
    int size = luaL_checkinteger(lua, 1);
    void * block = allocate_noptr(size);
    if (! block) luaL_argerror(lua, 1, "can't allocate");
    new_block(lua, get_header(block));
    return 1;
}

int do_alloc_ptr(lua_State * lua) {
    int size = luaL_checkinteger(lua, 1);
    void * block = allocate_allptr(size);
    if (! block) luaL_argerror(lua, 1, "can't allocate");
    new_block(lua, get_header(block));
    return 1;
}

int do_collect(lua_State * lua) {
    collect();
    return 0;
}

struct luaL_Reg monitor_lib[] = {
    {"hexdump", do_hexdump},
    {"get_heap_addr", do_get_heap_addr},
    {"print_block_at", do_print_block_at},
    {"print_block", do_print_block},
    {"get_root", get_root_block},
    {"collect", do_collect},
    {NULL, NULL}
};

void setup_globals(lua_State * lua) {
    luaL_newlib(lua, monitor_lib);
    lua_setglobal(lua, "mon");

    lua_pushinteger(lua, (int) heap0);
    lua_setglobal(lua, "heap");

    lua_pushcfunction(lua, do_hex);
    lua_setglobal(lua, "hex");

    get_root_block(lua);
    lua_setglobal(lua, "root");

    lua_pushcfunction(lua, do_alloc_data);
    lua_setglobal(lua, "data");

    lua_pushcfunction(lua, do_alloc_ptr);
    lua_setglobal(lua, "ptr");
}

void setup_ledger(lua_State * lua) {
    // create weak-key ledger to track lua-held heap blocks
    //   each block lua holds is tracked in ledger
    //   key is lua object, value is heap address
    //   if lua object is collected, its ledger entry goes away
    //   before heap gc, call lua gc to free unreachable lua objects
    //   at heap gc, find which blocks are lua-held and treat as roots
    //     then update values in ledger to reflect new addresses
    //   all lua access to heap blocks goes through ledger
    lua_pushlightuserdata(lua, heap0);   // ledger's key in registry is heap0
    lua_newtable(lua);   // ledger table
    lua_newtable(lua);   // ledger's metatable (to set weak key mode)
    lua_pushstring(lua, "k");   // keys are weak
    lua_setfield(lua, -2, "__mode");   // set weak key mode
    lua_setmetatable(lua, -2);  // set ledger's metatable
    lua_settable(lua, LUA_REGISTRYINDEX);   // set ledger into registry
    lua_pop(lua, 1);
}

void luamon() {
    using_history();

    lua_State * lua = luaL_newstate();
    luaL_openlibs(lua);

    make_heap(& heap0, HEAP_SIZE);
    setup_gc_hooks(lua);
    setup_ledger(lua);

    setup_block_metatable(lua);
    setup_globals(lua);

    char * line;
    while(true) {
        lua_settop(lua, 0);

        line = readline("luamon> ");
        if (! line) break;
        if (* line) add_history(line);

        const char * retline = lua_pushfstring(lua, "return %s;", line);
        int status = luaL_loadbuffer(lua, retline, strlen(retline), "=stdin");
        lua_remove(lua, 1);
        if (status != LUA_OK) {
            lua_pop(lua, 1);
            status = luaL_loadbuffer(lua, line, strlen(line), "stdin");
        }
        if (status != LUA_OK) {
            printf("error: %s\n", luaL_checkstring(lua, 1));
            lua_pop(lua, 1);
        }
        else {
            status = lua_pcall(lua, 0, LUA_MULTRET, 0);
            if (status == LUA_OK) {
                int n = lua_gettop(lua);
                if (n > 0) {
                    lua_getglobal(lua, "print");
                    lua_insert(lua, 1);
                    lua_pcall(lua, n, 0, 0);
                }
            }
        }
        free(line);
    }
    putchar('\n');
}

int main(int argc, char * argv[]) {
    luamon();
}
