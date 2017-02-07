mnemonics = ["ABORT", "HALT", "ALLOCATE_NOPTR", "ALLOCATE_NOPTR_imm16",
             "ALLOCATE_ALLPTR", "ALLOCATE_ALLPTR_imm16", "ALLOCATE_CONS",
             "GET_CBLK", "GET_INST", "GET_DBLK", "READ_FAR", "WRITE_FAR",
             "JUMP_FAR", "JUMP_FAR_imm8", "JUMP_AREC", "CONST_imm16",
             "CONST_FAR_imm8", "CONST_imm16_raw", "SET_16l", "SET_16l_raw",
             "SET_14h", "SET_16h_raw", "ADD", "ADD_imm8", "ADD_raw", "OR",
             "OR_raw", "MUL", "AND", "XOR", "LSHIFT_imm8", "LSHIFT_imm8_raw",
             "RSHIFT_imm8", "DEBUG_WRITEHEX_raw", "DEBUG_WRITEHEX_int"]

enum_code = """
enum opcodes {{
{0}
}};
""".format(str.join(",\n", mnemonics))

from_enum_to_string_code = ""

from_string_to_enum_code = ""

if __name__ == "__main__":
    with open("mnemonics.h", "w") as f:
        f.write(enum_code)
