mnemonics = [("ABORT",()),
             ("HALT",()),
             ("ALLOCATE_NOPTR",(8,8)),
             ("ALLOCATE_NOPTR_imm16",(8,16)),
             ("ALLOCATE_ALLPTR",(8,8)),
             ("ALLOCATE_ALLPTR_imm16",(8,16)),
             ("ALLOCATE_CONS",(8,)),
             ("GET_CBLK",(8,)),
             ("GET_INST",(8,)),
             ("GET_DBLK",(8,)),
             ("READ_FAR",(8,8,8)),
             ("WRITE_FAR",(8,8,8)),
             ("JUMP_FAR",(8,8,8)),
             ("JUMP_FAR_imm8",(8,8,8)),
             ("JUMP_AREC",(8,)),
             ("CONST_imm16",(8,16)),
             ("CONST_FAR_imm8",(8,8,8)),
             ("CONST_imm16_raw",(8,16)),
             ("SET_16l",(8,16)),
             ("SET_16l_raw",(8,16)),
             ("SET_14h",(8,16)),
             ("SET_16h_raw",(8,16)),
             ("ADD",(8,8,8)),
             ("ADD_imm8",(8,8,8)),
             ("ADD_raw",(8,8,8)),
             ("OR",(8,8,8)),
             ("OR_raw",(8,8,8)),
             ("MUL",(8,8,8)),
             ("AND",(8,8,8)),
             ("XOR",(8,8,8)),
             ("LSHIFT_imm8",(8,8,8)),
             ("LSHIFT_imm8_raw",(8,8,8)),
             ("RSHIFT_imm8",(8,8,8)),
             ("DEBUG_WRITEHEX_raw",(8,16)),
             ("DEBUG_WRITEHEX_int",(8,16)),
             ]

h_start = """
#ifndef MNEMONICS_H
#define MNEMONICS_H
"""
 
enum_code = """
enum opcodes {{
{0}
}};
""".format(str.join(",\n", [m for m,_ in mnemonics]))

h_end = """
char * opcode_to_mnemonic(enum opcodes op);
enum opcodes mnemonic_to_opcode(char * mn);

#endif // MNEMONICS_H
"""

c_start = """
#include "mnemonics.h"
"""

from_enum_to_string_code = ("""
char * conversion[] = {{
{0}
}};
""".format(str.join(",\n", ['[{0}]="{0}"'.format(m) for m,_ in mnemonics]))
+ """
char * opcode_to_mnemonic(enum opcodes op) {
    return conversion[op];
}
""")

from_string_to_enum_code = str.join("", [
"""
enum opcodes mnemonic_to_opcode(char * mn) {
"""]
+ [""]
+ ["""
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
        f.write(from_string_to_enum_code)
