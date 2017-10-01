Dependencies:

  * `glibc-devel.i686`
  * `readline-devel.i686`
  * `lua-devel.i686`

---

`luamon` builtins:

  * mon
    * hexdump
    * get_heap_addr
    * print_block_at
    * print_block
    * get_root
    * collect
    * save
    * load
    * disasm
    * print_heap
  * heap_addr
  * hex
  * raw
  * root
  * alloc_code
  * alloc_data
  * alloc_ptr
  * readline
  * execute

---

`lisp.lua` functions:

  * pcomp
  * scall
  * run