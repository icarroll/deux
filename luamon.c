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

int block_tostring(lua_State * lua) {
    void * obj = luaL_checkudata(lua, 1, "block");

    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    lua_pushvalue(lua, 1);   // ledger key is block argument
    lua_gettable(lua, -2);   // look up block address in ledger

    int addr = luaL_checkinteger(lua, -1);
    struct block_header * header = (struct block_header *) addr;

    char * note = (char *) (& header->note);
    lua_pushfstring(lua, "%p: layout=%s size=%dx%d=%d note=%c%c%c%c",
                    header, layout_str(header->layout),
                    header->size/sizeof(void *), sizeof(void *), header->size,
                    note[0], note[1], note[2], note[3]);
    return 1;
}

void setup_block_metatable(lua_State * lua) {
    luaL_newmetatable(lua, "block");
    lua_pushcfunction(lua, block_tostring);
    lua_setfield(lua, -2, "__tostring");
    lua_pop(lua, 1);
}

int do_hexdump(lua_State * lua) {
    int addr = luaL_checkinteger(lua, 1);
    int length = luaL_checkinteger(lua, 2);
    //TODO catch segfault and throw lua error
    print_hexdump((void *) addr, length);
    return 0;
}

int do_get_heap(lua_State * lua) {
    lua_pushinteger(lua, (int) heap0);
    return 1;
}

int do_print_block_at(lua_State * lua) {
    int addr = luaL_checkinteger(lua, 1);
    //TODO catch segfault and throw lua error
    print_block((struct block_header *) addr);
    return 0;
}

int get_addr_from_ledger(lua_State * lua, int n) {
    // get ledger from registry
    lua_pushlightuserdata(lua, heap0);
    lua_gettable(lua, LUA_REGISTRYINDEX);

    lua_pushvalue(lua, n);   // ledger key is block argument
    lua_gettable(lua, -2);   // look up block address in ledger

    return luaL_checkinteger(lua, -1);
}

int do_print_block(lua_State * lua) {
    luaL_checkudata(lua, 1, "block");
    int addr = get_addr_from_ledger(lua, 1);
    //TODO catch segfault and throw lua error
    print_block((struct block_header *) addr);
    return 0;
}

struct luaL_Reg monitor_lib[] = {
    {"hexdump", do_hexdump},
    {"get_heap", do_get_heap},
    {"print_block_at", do_print_block_at},
    {"print_block", do_print_block},
    {NULL, NULL}
};

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
    lua_pushinteger(lua, (int) header);  // ledger value is heap address
    lua_settable(lua, -3);   // enter new block in registry

    lua_pop(lua, 1);   // remove ledger, leave new block object on stack
}

int get_root_block(lua_State * lua) {
    new_block(lua, heap0->roots[0]);
    return 1;
}

void setup_globals(lua_State * lua) {
    luaL_newlib(lua, monitor_lib);
    lua_setglobal(lua, "mon");

    lua_pushinteger(lua, (int) heap0);
    lua_setglobal(lua, "heap");

    lua_pushcfunction(lua, do_hex);
    lua_setglobal(lua, "hex");

    lua_pushcfunction(lua, get_root_block);
    lua_setglobal(lua, "root");
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

    make_heap(& heap0, HEAP_SIZE);

    lua_State * lua = luaL_newstate();
    luaL_openlibs(lua);

    setup_block_metatable(lua);
    setup_globals(lua);
    setup_ledger(lua);

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
