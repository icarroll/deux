#include <stdbool.h>
#include <stdlib.h>

#include <readline/history.h>
#include <readline/readline.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "deux.h"

//TODO
// emit code into block
// invoke activation record/code block

extern struct heap * heap0;
extern void (* pre_collect_hook)();
extern void (* add_roots_hook)(struct block_header **);
extern void (* update_roots_hook)();
extern struct registers regs;

int do_print_block(lua_State * lua);
int do_write_string(lua_State * lua);
int do_read_string(lua_State * lua);
void new_block(lua_State * lua, struct block_header * header);
struct block_header * get_addr_from_ledger(lua_State * lua, int n);
void setup_reverse_ledger(lua_State * lua);
void setup_ledger(lua_State * lua);

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

    setup_reverse_ledger(lua);   // clear out reverse ledger
    // get reverse ledger from registry
    lua_pushlightuserdata(lua, heap0->start);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    // iterate over ledger
    lua_pushnil(lua);
    while (lua_next(lua, -2)) {
        // copy the block object reference (need to use thrice)
        lua_pushvalue(lua, -2);
        lua_insert(lua, -2);
        lua_pushvalue(lua, -2);
        lua_insert(lua, -2);

        // update address in ledger
        struct block_header * header = lua_touserdata(lua, -1);
        lua_pop(lua, 1);
        lua_pushlightuserdata(lua, header->link_ptr);
        lua_settable(lua, -5);

        // add entry to reverse ledger
        lua_pushlightuserdata(lua, header->link_ptr);
        lua_insert(lua, -2);
        lua_settable(lua, -5);
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

    struct block_header * header = get_addr_from_ledger(lua, 1);

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
        if (lua_rawequal(lua, -1, -2)) {
            lua_pop(lua, 2);
            lua_pushcfunction(lua, do_print_block);
            return 1;
        }
        else lua_pop(lua, 1);

        lua_pushliteral(lua, "note");
        if (lua_rawequal(lua, -1, -2)) {
            lua_pop(lua, 2);
            struct block_header * header = get_addr_from_ledger(lua, 1);
            lua_pushlstring(lua, (char *) (& header->note), 4);
            return 1;
        }
        else lua_pop(lua, 1);

        lua_pushliteral(lua, "write_string");
        if (lua_rawequal(lua, -1, -2)) {
            lua_pop(lua, 2);
            lua_pushcfunction(lua, do_write_string);
            return 1;
        }
        else lua_pop(lua, 1);

        lua_pushliteral(lua, "read_string");
        if (lua_rawequal(lua, -1, -2)) {
            lua_pop(lua, 2);
            lua_pushcfunction(lua, do_read_string);
            return 1;
        }
        else lua_pop(lua, 1);

        luaL_argerror(lua, 2, "no such method");
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
            case 0b11:   // uint32_t
                lua_pushinteger(lua, untagint(value));
                break;
            default:   // pointer
                if (in_heap(value)) new_block(lua, get_header(untagptr(value)));
                else lua_pushlightuserdata(lua, value);
                break;
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

    if (lua_type(lua, 2) == LUA_TSTRING) {
        lua_pushliteral(lua, "note");
        if (lua_rawequal(lua, -1, 2)) {
            lua_pop(lua, 1);
            char * new_note = luaL_checkstring(lua, 3);
            struct block_header * header = get_addr_from_ledger(lua, 1);
            header->note = make_note(new_note);
            return 0;
        }
        else lua_pop(lua, 1);

        luaL_argerror(lua, 2, "no such method");
    }

    int ix = luaL_checkinteger(lua, 2);

    struct block_header * header = get_addr_from_ledger(lua, 1);
    if (0 <= ix && ix < header->size / sizeof(void *)) {
        switch (header->layout) {
        case no_ptr_layout:
            switch (lua_type(lua, 3)) {
            case LUA_TNIL:
                header->data[ix] = NULL;
                break;
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
            case LUA_TNIL:
                header->data[ix] = NULL;
                break;
            case LUA_TNUMBER:
                header->data[ix] = tagint(lua_tointeger(lua, 3));
                break;
            case LUA_TLIGHTUSERDATA:
                header->data[ix] = lua_touserdata(lua, 3);
                break;
            case LUA_TUSERDATA:
                {
                    struct block_header * pheader = get_addr_from_ledger(lua, 3);
                    void * block = pheader->data;
                    switch (pheader->note) {
                    case CONS_NOTE:
                        block = tagconsptr(block);
                        break;
                    case SYMB_NOTE:
                        block = tagsymptr(block);
                        break;
                    default:
                        block = tagblockptr(block);
                        break;
                    }
                    header->data[ix] = block;
                }
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
    void * block1 = luaL_checkudata(lua, 1, "block");
    void * block2 = luaL_checkudata(lua, 2, "block");

    bool result = get_addr_from_ledger(lua, 1) == get_addr_from_ledger(lua, 2);
    //if (result && (block1 != block2)) printf("problem: different lua objects, same heap block\n");
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

    lua_pushcfunction(lua, block_same);
    lua_setfield(lua, -2, "__eq");

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

    void * temp = lua_touserdata(lua, -1);
    if (temp == NULL) luaL_argerror(lua, n, "invalidated block");
    return temp;
}

int do_print_block(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    struct block_header * header = get_addr_from_ledger(lua, 1);

    print_block(header);
    return 0;
}

int do_write_string(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    struct block_header * header = get_addr_from_ledger(lua, 1);
    char * s = luaL_checkstring(lua, 2);

    memset(header->data, 0, header->size);
    strncpy((char *) header->data, s, header->size - 1);
    return 0;
}

int do_read_string(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    struct block_header * header = get_addr_from_ledger(lua, 1);

    lua_pushstring(lua, (char *) header->data);
    return 1;
}

int do_hex(lua_State * lua) {
    int n = luaL_checkinteger(lua, 1);
    char s[19];
    snprintf(s, 19, "0x%x", n);
    lua_pushstring(lua, s);
    return 1;
}

int do_raw(lua_State * lua) {
    int n = luaL_checkinteger(lua, 1);
    lua_pushlightuserdata(lua, (void *) n);
    return 1;
}

void new_block(lua_State * lua, struct block_header * header) {
    // get reverse ledger from registry
    lua_pushlightuserdata(lua, heap0->start);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    // first check for address in reverse ledger
    lua_pushlightuserdata(lua, header);   // reverse ledger key is header address
    lua_gettable(lua, -2);   // is there existing block object for this address?
    if (! lua_isnil(lua, -1)) {   // if there is,
        lua_remove(lua, -2);   // remove reverse ledger from stack
        return;   // return block object
    }
    else lua_pop(lua, 1);   // remove nil, leave reverse ledger on stack

    // new block object
    lua_newuserdata(lua, 1);
    luaL_setmetatable(lua, "block");

    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    // enter block in ledger
    lua_pushvalue(lua, -2);   // ledger key is new block object
    lua_pushlightuserdata(lua, header);   // ledger value is block header address
    lua_settable(lua, -3);   // enter new block in ledger

    lua_pop(lua, 1);   // remove ledger, leave new block object on stack

    // enter block in reverse ledger
    lua_pushlightuserdata(lua, header);   // reverse ledger key is address
    lua_pushvalue(lua, -2);   // copy block object reference as value
    lua_settable(lua, -4);   // enter block in reverse ledger

    lua_remove(lua, -2);   // remove reverse ledger
}

int get_root_block(lua_State * lua) {
    new_block(lua, heap0->roots[0]);
    return 1;
}

int do_alloc_code(lua_State * lua) {
    int size = luaL_checkinteger(lua, 1);
    void * block = allocate_noptr(size * sizeof(void *));
    if (! block) luaL_argerror(lua, 1, "can't allocate");
    struct block_header * header = get_header(block);
    header->note = make_note("code");
    new_block(lua, header);
    return 1;
}

int do_alloc_noptr_bytes(lua_State * lua) {
    int size = luaL_checkinteger(lua, 1);
    void * block = allocate_noptr(size);
    if (! block) luaL_argerror(lua, 1, "can't allocate");
    new_block(lua, get_header(block));
    return 1;
}

int do_alloc_ptr(lua_State * lua) {
    int size = luaL_checkinteger(lua, 1);
    void * block = allocate_allptr(size * sizeof(void *));
    if (! block) luaL_argerror(lua, 1, "can't allocate");
    new_block(lua, get_header(block));
    return 1;
}

int do_disassemble(lua_State * lua) {
    struct block_header * header;
    void ** code_block;

    switch (lua_type(lua, 1)) {
    case LUA_TNUMBER:
        code_block = (void *) ((int) lua_tointeger(lua, 1));
        header = get_header(code_block);
        break;
    case LUA_TLIGHTUSERDATA:
        code_block = lua_touserdata(lua, 1);
        header = get_header(code_block);
        break;
    case LUA_TUSERDATA:
        header = get_addr_from_ledger(lua, 1);
        code_block = header->data;
        break;
    default:
        luaL_argerror(lua, 1, "bad argument");
    }

    if (header->layout != no_ptr_layout) luaL_argerror(lua, 1, "block type");

    print_disassembly(code_block, header->size);
    return 0;
}

int do_collect(lua_State * lua) {
    collect();
    return 0;
}

int do_save(lua_State * lua) {
    save_heap();
    return 0;
}

int do_load(lua_State * lua) {
    //TODO free existing heap first
    load_heap(& heap0, HEAP_SIZE);
    setup_ledger(lua);   // invalidate lua-held blocks

    get_root_block(lua);
    lua_setglobal(lua, "root");

    lua_pushinteger(lua, (int) heap0);
    lua_setglobal(lua, "heap_addr");

    return 0;
}

int do_readline(lua_State * lua) {
    char * prompt = luaL_checkstring(lua, -1);
    char * line = readline(prompt);
    if (! line) {
        lua_pushnil(lua);
        return 1;
    }

    if (* line) add_history(line);
    lua_pushstring(lua, line);
    free(line);
    return 1;
}

int do_execute(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    struct block_header * header = get_addr_from_ledger(lua, 1);
    if (header->layout != all_ptr_layout) luaL_argerror(lua, 1, "bad execute");

    void * argument;
    switch (lua_type(lua, 2)) {
    case LUA_TNIL:
        argument = NULL;
        break;
    case LUA_TNUMBER:
        argument = tagint(lua_tointeger(lua, 2));
        break;
    case LUA_TLIGHTUSERDATA:
        argument = lua_touserdata(lua, 2);
        break;
    case LUA_TUSERDATA:
        {
            struct block_header * header = get_addr_from_ledger(lua, 2);
            switch (header->note) {
            case CONS_NOTE:
                argument = tagconsptr(header->data);
                break;
            case SYMB_NOTE:
                argument = tagsymptr(header->data);
                break;
            default:
                argument = tagblockptr(header->data);
                break;
            }
        }
        break;
    default:
        luaL_argerror(lua, 2, "bad argument");
    }

    regs.code_block = header->data[0];
    regs.icount = 0;
    regs.arec_block = header->data;
    regs.link_data = argument;

    run();

    void * value = regs.link_data;
    switch ((uint32_t) value & 0b11) {
    case 0b11:   // uint32_t
        lua_pushinteger(lua, untagint(value));
        break;
    default:   // pointer
        if (in_heap(value)) new_block(lua, get_header(untagptr(value)));
        else lua_pushlightuserdata(lua, value);
        break;
    }

    return 1;
}

int do_print_heap(lua_State * lua) {
    print_heap();
    return 0;
}

struct luaL_Reg monitor_lib[] = {
    {"hexdump", do_hexdump},
    {"get_heap_addr", do_get_heap_addr},
    {"print_block_at", do_print_block_at},
    {"print_block", do_print_block},
    {"get_root", get_root_block},
    {"collect", do_collect},
    {"save", do_save},
    {"load", do_load},
    {"disasm", do_disassemble},
    {"print_heap", do_print_heap},
    {NULL, NULL}
};

void setup_globals(lua_State * lua) {
    luaL_newlib(lua, monitor_lib);
    lua_setglobal(lua, "mon");

    lua_pushinteger(lua, (int) heap0);
    lua_setglobal(lua, "heap_addr");

    lua_pushcfunction(lua, do_hex);
    lua_setglobal(lua, "hex");

    lua_pushcfunction(lua, do_raw);
    lua_setglobal(lua, "raw");

    get_root_block(lua);
    lua_setglobal(lua, "root");

    lua_pushcfunction(lua, do_alloc_code);
    lua_setglobal(lua, "alloc_code");

    lua_pushcfunction(lua, do_alloc_noptr_bytes);
    lua_setglobal(lua, "alloc_data");

    lua_pushcfunction(lua, do_alloc_ptr);
    lua_setglobal(lua, "alloc_ptr");

    lua_pushcfunction(lua, do_readline);
    lua_setglobal(lua, "readline");

    lua_pushcfunction(lua, do_execute);
    lua_setglobal(lua, "execute");

    int status = luaL_dofile(lua, "mnemonics.lua");
    if (status != LUA_OK) lua_error(lua);

    status = luaL_dofile(lua, "lisp.lua");
    if (status != LUA_OK) lua_error(lua);
}

void setup_reverse_ledger(lua_State * lua) {
    // reverse ledger mapping addresses to block objects
    lua_pushlightuserdata(lua, heap0->start);   // registry key is heap start
    lua_newtable(lua);   // reverse ledger table
    lua_newtable(lua);   // reverse ledger's metatable (to set weak value mode)
    lua_pushstring(lua, "v");   // values are weak
    lua_setfield(lua, -2, "__mode");   // set weak value mode
    lua_setmetatable(lua, -2);  // set ledger's metatable
    lua_settable(lua, LUA_REGISTRYINDEX);   // set reverse ledger into registry
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

    setup_reverse_ledger(lua);
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
            printf("error: %s\n", luaL_checkstring(lua, -1));
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
            else {
                printf("error: %s\n", luaL_checkstring(lua, -1));
                lua_pop(lua, 1);
            }
        }
        free(line);
    }
    putchar('\n');
}

int main(int argc, char * argv[]) {
    luamon();
}
