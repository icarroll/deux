mnemonics = [("ABORT",(0,)),
             ("HALT",(0,)),
             ("ALLOCATE_NOPTR",(11,)),
             ("ALLOCATE_NOPTR_imm16",(12,)),
             ("ALLOCATE_ALLPTR",(11,)),
             ("ALLOCATE_ALLPTR_imm16",(12,)),
             ("GET_CBLK",(1,)),
             ("GET_INST",(1,)),
             ("GET_DBLK",(1,)),
             ("READ_FAR",(111,)),
             ("WRITE_FAR",(111,)),
             ("JUMP_FAR",(111,)),
             ("JUMP_FAR_imm8",(111,)),
             ("JUMP_AREC",(1,)),
             ("CONST_imm16",(12,)),
             ("CONST_FAR_imm8",(111,)),
             ("CONST_imm16_raw",(12,)),
             ("SET_16l",(12,)),
             ("SET_16l_raw",(12,)),
             ("SET_14h",(12,)),
             ("SET_16h_raw",(12,)),
             ("ADD",(111,)),
             ("ADD_imm8",(111,)),
             ("ADD_raw",(111,)),
             ("OR",(111,)),
             ("OR_raw",(111,)),
             ("MUL",(111,)),
             ("AND",(111,)),
             ("XOR",(111,)),
             ("LSHIFT_imm8",(111,)),
             ("LSHIFT_imm8_raw",(111,)),
             ("RSHIFT_imm8",(111,)),
             ("DEBUG_WRITEHEX_raw",(12,)),
             ("DEBUG_WRITEHEX_int",(12,)),
             ]

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

if __name__ == "__main__":
    with open("mnemonics.h", "w") as f:
        f.write(h_start)
        f.write(enum_code)
        f.write(h_end)

    with open("mnemonics.c", "w") as f:
        f.write(c_start)
        f.write(from_enum_to_string_code)
        f.write(patterns_code)
        f.write(from_string_to_enum_code)
