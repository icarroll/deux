mnemonics = [("ABORT",(0,)),
             ("HALT",(0,)),
             ("ALLOCATE_NOPTR",(11,)),
             ("ALLOCATE_NOPTR_imm16",(12,)),
             ("ALLOCATE_ALLPTR",(11,)),
             ("ALLOCATE_ALLPTR_imm16",(12,)),
             ("ALLOCATE_CONS",(1,)),
             ("COPY_BLOCK", (11,)),
             ("SET_NOTE",(11,)),
             ("GET_CBLK",(1,)),
             ("GET_INST",(1,)),
             ("GET_AREC",(1,)),
             ("GET_AREC_FAR",(11,)),
             ("GET_LINK_PTR",(1,)),
             ("GET_LINK_PTR_FAR",(11,)),
             ("SET_LINK_PTR",(1,)),
             ("GET_LINK_DATA",(1,)),
             ("SET_LINK_DATA",(1,)),
             ("SET_LINK_DATA_imm24",(3,)),
             ("READ_FAR",(111,)),
             ("WRITE_FAR",(111,)),
             ("WRITE_FAR_imm8",(111,)),
             ("JUMP_imm24", (3,)),
             ("JUMP_IF_imm16", (12,)),
             ("JUMP_IF_raw_imm16", (12,)),
             ("JUMP_REL_imm24", (3,)),
             ("JUMP_REL_IF_imm16", (12,)),
             ("JUMP_REL_IF_raw_imm16", (12,)),
             ("JUMP_UNLS_imm16", (12,)),
             ("JUMP_FAR",(111,)),
             ("JUMP_FAR_imm8",(111,)),
             ("JUMP_AREC",(1,)),
             ("RESET_JUMP_AREC",(1,)),
             ("IS_ZERO",(11,)),
             ("IS_ZERO_raw",(11,)),
             ("CONST_imm16",(12,)),
             ("CONST_FAR_imm8",(111,)),
             ("CONST_imm16_raw",(12,)),
             ("SET_16l",(12,)),
             ("SET_16l_raw",(12,)),
             ("SET_14h",(12,)),
             ("SET_16h_raw",(12,)),
             ("COPY", (11,)),
             ("ADD",(111,)),
             ("ADD_imm8",(111,)),
             ("ADD_raw",(111,)),
             ("SUB",(111,)),
             ("SUB_raw",(111,)),
             ("SUB_imm8",(111,)),
             ("SUB_imm8_raw",(111,)),
             ("OR",(111,)),
             ("OR_raw",(111,)),
             ("OR_imm8_raw",(111,)),
             ("MUL",(111,)),
             ("AND",(111,)),
             ("AND_imm8_raw",(111,)),
             ("XOR",(111,)),
             ("XOR_imm8",(111,)),
             ("LSHIFT_imm8",(111,)),
             ("LSHIFT_imm8_raw",(111,)),
             ("RSHIFT_imm8",(111,)),
             ("DEBUG_WRITEHEX_raw",(12,)),
             ("DEBUG_WRITEHEX_int",(12,)),
             ]

if len(mnemonics) > 256:
    print("too many mnemonics")
    import sys
    sys.exit(1)

h_start = """
#ifndef MNEMONICS_H
#define MNEMONICS_H
"""
 
enum_code = """
enum opcodes {{
{0}
num_opcodes,
}};
""".format(str.join("\n", ["{0},".format(m) for m,_ in mnemonics]))

h_end = """
char * opcode_to_mnemonic(enum opcodes op);
int arg_pattern(enum opcodes op);
enum opcodes mnemonic_to_opcode(char * mn);

#endif // MNEMONICS_H
"""

c_start = """
#include <string.h>

#include "mnemonics.h"
"""

from_enum_to_string_code = ("""
char * conversion[] = {{
{0}
}};
""".format(str.join(",\n", ['[{0}]="{0}"'.format(m) for m,_ in mnemonics]))
+ """
char * opcode_to_mnemonic(enum opcodes op) {
    if (op < 0 || op >= num_opcodes) return NULL;
    return conversion[op];
}
""")

patterns_code = ("""
int arg_patterns[] = {{
{0}
}};
""".format(str.join(",\n", ["[{0}]={1}".format(m,pat)
                            for m,(pat,) in mnemonics]))
+ """
int arg_pattern(enum opcodes op) {
    if (op < 0 || op >= num_opcodes) return -1;
    return arg_patterns[op];
}
""")

from_string_to_enum_code = str.join("", [
"""
enum opcodes mnemonic_to_opcode(char * mn) {
"""]
+ ['if (strcmp(mn, conversion[{0}]) == 0) return {0};\n'.format(m)
   for m,_ in mnemonics]
+ ["""
return -1;
}
"""]
)

lua_start = "calc_func = {\n"

def lua_items(stuff):
    for ix, (name, (argtype,)) in enumerate(stuff):
        nargs = {0:0, 3:1, 1:1, 11:2, 12:2, 111:3}[argtype]
        args = str.join(",", ["a{0}".format(n) for n in range(nargs)])
        calculation = {
                0:"",
                3:"|a0",
                1:"|a0<<16",
                11:"|a0<<16|a1<<8",
                12:"|a0<<16|a1",
                111:"|a0<<16|a1<<8|a2",
                }[argtype]

        yield name, args, ix, calculation

lua_body = str.join("", ["  {0}=function ({1}) return {2} << 24 {3} end,\n"
                         .format(*items) for items in lua_items(mnemonics)])

lua_end = ("""}

function map(func, array)
  local new_array = {}
  for i,v in ipairs(array) do
    new_array[i] = func(v)
  end
  return new_array
end

for opcode,calc in pairs(calc_func) do
  _G[opcode] = function(...)
    return {opcode, ...}
  end
end

function calc_op(name)
  return calc_func[name]
end

argtypes = {
"""
+ str.join("", ["  {0}={1!r},\n".format(name,argtype)
                for name,(argtype,) in mnemonics])
+ "}\n"
)

def writefiles():
    with open("mnemonics.h", "w") as f:
        f.write(h_start)
        f.write(enum_code)
        f.write(h_end)

    with open("mnemonics.c", "w") as f:
        f.write(c_start)
        f.write(from_enum_to_string_code)
        f.write(patterns_code)
        f.write(from_string_to_enum_code)

    with open("mnemonics.lua", "w") as f:
        f.write(lua_start)
        f.write(lua_body)
        f.write(lua_end)

if __name__ == "__main__":
    writefiles()
