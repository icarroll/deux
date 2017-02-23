#include <stdbool.h>
#include <stdlib.h>

#include <readline/history.h>
#include <readline/readline.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "deux.h"

extern struct heap * heap0;

int do_lol(lua_State * lua) {
    int times = luaL_checkinteger(lua, 1);
    for (int ix=0 ; ix < times ; ix+=1) {
        printf("lol ");
    }
    putchar('\n');
    return 0;
}

int do_whut(lua_State * lua) {
    lua_pushstring(lua, "lol");
    lua_pushstring(lua, "whut");
    return 2;
}

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

struct luaL_Reg monitor_lib[] = {
    {"lol", do_lol},
    {"whut", do_whut},
    {NULL, NULL}
};

void luamon() {
    using_history();

    make_heap(& heap0, HEAP_SIZE);

    lua_State * lua = luaL_newstate();
    luaL_openlibs(lua);

    luaL_newlib(lua, monitor_lib);
    lua_setglobal(lua, "mon");

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
