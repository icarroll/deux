set code_block 0
set icount 1
set link_value 2
set dyn_parent 3
set stat_parent 4
set descriptor 5
set new_arec 6
set known_arec_size 7
set known_code_block 8

.code_block create_subprogram_arec
ALLOCATE_ALLPTR new_arec known_arec_size        # allocate activation record
WRITE_FAR new_arec code_block known_code_block  # set code block
WRITE_FAR_imm8 new_arec icount 0                # set icount to 0
WRITE_FAR new_arec link_value link_value        # copy in argument
WRITE_FAR new_arec dyn_parent dyn_parent        # copy in dynamic parent
WRITE_FAR new_arec stat_parent stat_parent      # set static parent
GET_DBLK_FAR new_arec descriptor                # set descriptor
RESET_JUMP_AREC new_arec                        # transfer to subprogram proper
.end

.code_block create_coroutine_arec
ALLOCATE_ALLPTR new_arec known_arec_size        # allocate activation record
WRITE_FAR new_arec code_block known_code_block  # set code block
WRITE_FAR_imm8 new_arec icount 0                # set icount to 0
WRITE_FAR new_arec link_value link_value        # copy in argument
WRITE_FAR new_arec dyn_parent dyn_parent        # copy in dynamic parent
WRITE_FAR new_arec stat_parent stat_parent      # set static parent
GET_DBLK_FAR new_arec descriptor                # set descriptor
WRITE_FAR dyn_parent link_value new_arec        # tell caller new arec
RESET_JUMP_AREC dyn_parent                      # return to caller
.end

.code_snippet create_subprogram_descriptor
ALLOCATE_ALLPTR_imm16 new_desc DESC_SIZE
# somehow look up create_subprogram_arec
WRITE_FAR new_desc code_block create_subprogram_arec
WRITE_FAR_imm8 new_desc icount 0
# create code block
WRITE_FAR new_desc known_arec_size calculated_arec_size
WRITE_FAR new_desc known_code_block new_code_block
WRITE_FAR new_desc symbol_table new_symbol_table
GET_DBLK_FAR new_desc stat_parent
.end

# vim: ft=asm
