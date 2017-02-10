#include <errno.h>
#include <inttypes.h>
#include <readline/history.h>
#include <readline/readline.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "deux.h"
#include "mnemonics.h"

// memory management

struct registers regs;

struct block_header * get_header(void * block_ptr) {
    return (struct block_header *) (block_ptr - hdr_sz);
}

void print_block_header(struct block_header * header) {
    printf("%x: link=%x marked=%s layout=%s size=%d\n",
           header->data, header->link_ptr, bool_str(header->marked),
           layout_str(header->layout), header->size);
}

void print_mark_list(struct block_header * header) {
    while (header) {
        print_block_header(header);
        header = header->link_ptr;
    }
}

void mark_in(struct heap * heap) {
    // mark roots and add to list
    struct block_header * next_header = heap->roots[SYMBOLS];
    heap->roots[0]->marked = true;
    struct block_header * last_header = heap->roots[SYMBOLS];

    for (int ix=1 ; ix < NUM_ROOTS ; ix+=1) {
        last_header->link_ptr = heap->roots[ix];
        last_header->marked = true;
        last_header = last_header->link_ptr;
    }

    if (regs.code_block) {
        last_header->link_ptr = regs.code_block;
        last_header->marked = true;
        last_header = last_header->link_ptr;
    }

    if (regs.data_block) {
        last_header->link_ptr = regs.data_block;
        last_header->marked = true;
        last_header = last_header->link_ptr;
    }

    last_header->link_ptr = NULL;

    while (next_header) {
        struct block_header * cur_header = next_header;

        switch (cur_header->layout) {
        case no_ptr_layout:
            break;
        case all_ptr_layout:
            {
                void ** end = cur_header->data
                              + cur_header->size / sizeof(* cur_header->data);
                for (void ** candidate = cur_header->data
                     ; candidate < end
                     ; candidate += 1) {
                    // don't follow null or unaligned pointers
                    if (* candidate && ! ((int) * candidate & 0b11)) {
                        struct block_header * header = get_header(* candidate);
                        if (! header->marked) {
                            header->link_ptr = NULL;
                            header->marked = true;
                            last_header->link_ptr = header;
                            last_header = header;
                        }
                    }
                }
            }
            break;
        case cons_layout:
            {
                struct cons * cell = (struct cons *) cur_header->data;
                if (is_ptr_tag(cell->head.tag) && cell->head.ptr) {
                    struct block_header * header = get_header(cell->head.ptr);
                    if (! header->marked) {
                        header->link_ptr = NULL;
                        header->marked = true;
                        last_header->link_ptr = header;
                        last_header = header;
                    }
                }
                if (is_ptr_tag(cell->tail.tag) && cell->tail.ptr) {
                    struct block_header * header = get_header(cell->tail.ptr);
                    if (! header->marked) {
                        header->link_ptr = NULL;
                        header->marked = true;
                        last_header->link_ptr = header;
                        last_header = header;
                    }
                }
            }
            break;
        default:
            die("unhandled block layout");
        }

        next_header = next_header->link_ptr;
    }
}

void * following_block(void * block) {
    return block + get_header(block)->size + hdr_sz;
}

void * compute_forward_addrs_in(struct heap * heap) {
    void * next_free_addr = heap->memory;
    for (void * block=heap->memory + hdr_sz
         ; block < heap->next
         ; block = following_block(block)) {
        struct block_header * header = get_header(block);
        if (header->marked) {
            header->link_ptr = next_free_addr;
            next_free_addr += hdr_sz + header->size;
        }
    }

    return next_free_addr;
}

void update_pointers_in(struct heap * heap) {
    // scan over heap
    for (void * block=heap->memory + hdr_sz
         ; block < heap->next
         ; block = following_block(block)) {
        struct block_header * header = get_header(block);
        if (! header->marked) continue;   // skip unmarked blocks

        switch (header->layout) {
        case no_ptr_layout:
            break;
        case all_ptr_layout:
            // scan over pointers in block
            for (void ** candidate = (void **) block
                 ; candidate < (void **) (block + header->size)
                 ; candidate += 1) {
                // forward any non-zero aligned pointers
                if (* candidate && ! ((int) * candidate & 0b11)) {
                    * candidate = get_header(* candidate)->link_ptr + hdr_sz;
                }
            }
            break;
        case cons_layout:
            {
                // in head and tail, forward non-zero pointers
                struct cons * cell = (struct cons *) block;
                if (is_ptr_tag(cell->head.tag) && cell->head.ptr) {
                    cell->head.ptr = get_header(cell->head.ptr)->link_ptr
                                     + hdr_sz;
                }
                if (is_ptr_tag(cell->tail.tag) && cell->tail.ptr) {
                    cell->tail.ptr = get_header(cell->tail.ptr)->link_ptr
                                     + hdr_sz;
                }
            }
            break;
        default:
            die("unhandled block layout");
        }
    }
}

void compact_in(struct heap * heap) {
    for (void * block=heap->memory + hdr_sz
         ; block < heap->next
         ; block = following_block(block)) {
        struct block_header * header = get_header(block);
        if (header->marked) {
            struct block_header * dest_header;
            dest_header = (struct block_header *) (header->link_ptr);
            memmove(dest_header, header, hdr_sz + header->size);

            dest_header->link_ptr = NULL;
            dest_header->marked = false;
        }
    }
}

//TODO
// check heap for errors
// test allocation and collection

void collect_in(struct heap * heap) {
    mark_in(heap);
    void * new_next = compute_forward_addrs_in(heap);

    // record new root locations
    for (int ix=0 ; ix < NUM_ROOTS ; ix+=1) {
        if (heap->roots[ix]) heap->new_roots[ix] = heap->roots[ix]->link_ptr;
        else heap->new_roots[ix] = NULL;
    }

    struct block_header * new_code_block;
    if (regs.code_block) {
        new_code_block = get_header(regs.code_block)->link_ptr;
    }

    struct block_header * new_data_block;
    if (regs.data_block) {
        new_data_block = get_header(regs.data_block)->link_ptr;
    }

    update_pointers_in(heap);
    compact_in(heap);

    heap->next = new_next;

    // record new root locations
    for (int ix=0 ; ix < NUM_ROOTS ; ix+=1) {
        if (heap->new_roots[ix]) heap->roots[ix] = heap->new_roots[ix];
    }

    if (regs.code_block) regs.code_block = new_code_block->data;
    if (regs.data_block) regs.data_block = new_data_block->data;
}

int round_to(int unit, int amount) {
    return ((amount - 1) / unit + 1) * unit;
}

void * allocate_in(struct heap * heap, int size, enum layout layout) {
    if (size < 1) return NULL;

    size = round_to(ALIGNMENT, size);
    int entire_size = round_to(ALIGNMENT, hdr_sz + size);

    void * new_next = heap->next + entire_size;
    if (new_next > heap->end) {
        collect_in(heap);
        new_next = heap->next + entire_size;
        if (new_next > heap->end) {
            return NULL;
        }
    }

    struct block_header * header = (struct block_header *) heap->next;
    header->link_ptr = NULL;
    header->marked = false;
    header->layout = layout;
    header->size = size;

    int data_size = entire_size - hdr_sz;
    memset(header->data, 0, data_size);

    heap->next = new_next;
    return header->data;
}

/*
void add_root_in(struct heap * heap, void * new_root_ptr) {
    struct block_header * root_block = heap->root_block;

    void ** block_end = root_block->data + root_block->size;

    void ** candidate;
    for (candidate=root_block->data ; candidate < block_end ; candidate += 1) {
        if (* candidate == NULL) {
            * candidate = new_root_ptr;
            return;
        }
    }

    void ** new_block = allocate_in(heap, ROOT_BLOCK_SIZE, all_ptr_layout);
    if (! new_block) die("can't allocate root block");

    new_block[0] = root_block->data;
    new_block[1] = new_root_ptr;
    heap->root_block = get_header(new_block);
}
*/

void make_heap(struct heap * heap, int size) {
    int rounded_size = round_to(getpagesize(), size);

    void * memory = mmap(NULL, rounded_size, PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (! memory) die("can't create heap");

    heap->memory = memory;
    heap->end = memory + rounded_size;
    heap->next = memory;

    heap->roots[SYMBOLS] = get_header(allocate_symbol_intern_node());
}

struct heap heap0;

void collect() {
    collect_in(& heap0);
}

void * allocate(int size, enum layout layout) {
    return allocate_in(& heap0, size, layout);
}

void * allocate_noptr(int size) {
    return allocate(size, no_ptr_layout);
}

void * allocate_allptr(int size) {
    return allocate(size, all_ptr_layout);
}

void init_heap() {
    make_heap(& heap0, HEAP_SIZE);
}

/*
void add_root(void * new_root) {
    add_root_in(& heap0, new_root);
}
*/

// end memory management

// heap display

char * bool_str(bool val) {
    return val ? "true" : "false";
}

char * layout_str(enum layout val) {
    switch (val) {
    case no_ptr_layout:
        return "noptr";
    case all_ptr_layout:
        return "allptr";
    case cons_layout:
        return "cons";
    default:
        return "unhandled";
    }
}

char * cons_tag_str(enum cons_tag val) {
    switch (val) {
    case cons_tag:
        return "cons";
    case sym_tag:
        return "sym";
    case char_tag:
        return "char";
    case int_tag:
        return "int";
    case obj_tag:
        return "obj";
    default:
        return "unhandled";
    }
}

//TODO iterate over headers instead of datas
void print_heap_in(struct heap * heap) {
    /*
    printf("memory=%x end=%x next=%x root_block=%x\n",
           heap->memory, heap->end, heap->next, heap->root_block);
    */
    printf("memory=%x end=%x next=%x\n",
           heap->memory, heap->end, heap->next);
    for (void * block=heap->memory + hdr_sz
         ; block < heap->next
         ; block = following_block(block)) {
        struct block_header * header = get_header(block);
        print_block_header(header);

        if (header->size <= 0) {
            printf("size error\n");
            break;
        }

        switch (header->layout) {
        case no_ptr_layout:
            break;
        case all_ptr_layout:
            // scan over pointers in block
            for (void ** candidate = (void **) block
                 ; candidate < (void **) (block + header->size)
                 ; candidate += 1) {
                // print any non-zero pointers
                if (* candidate) {
                    printf("    %x\n", * candidate);
                }
            }
            break;
        case cons_layout:
            {
                // print head and tail contents
                struct cons * cell = (struct cons *) block;
                printf("    %s=%x : %s=%x\n    ",
                       cons_tag_str(cell->head.tag), cell->head.ptr,
                       cons_tag_str(cell->tail.tag), cell->tail.ptr);
                print_cons_item((struct item) {cons_tag, cell});
                putchar('\n');
            }
            break;
        default:
            break;
        }
    }
}

void print_heap() {
    print_heap_in(& heap0);
}

// end heap display

// virtual machine

jmp_buf abort_run;

void throw_run_error(char * message) {
    printf("%s\n", message);
    longjmp(abort_run, 1);
}

void * tagint(uint32_t val) {
    return (void *) ((val << 2) | 0b11);
}

uint32_t untag(void * val) {
    return (uint32_t) val >> 2;
}

struct do_next {
    enum {
        abort_code,
        halt_code,
        jump_c,
    } action;
    struct registers regs;
};

struct do_next do_abort = {abort_code};
struct do_next do_halt = {halt_code};

void unimpl() {
    die("unimplemented instruction");
}

struct do_next run() {
    if (setjmp(abort_run)) {
        printf("run error\n");
        return do_abort;
    }

    unsigned int end = get_header(regs.code_block)->size / sizeof(void *);
    while (regs.icount < end) {
        unsigned int instruction = (unsigned int) regs.code_block[regs.icount];
        regs.icount += 1;

        enum opcodes op = instruction >> 24;
        unsigned int arg24 = instruction & 0xffffff;
        unsigned int arg8_1 = arg24 >> 16;
        unsigned int arg16 = arg24 & 0xffff;
        unsigned int arg8_2 = (arg24 >> 8) & 0xff;
        unsigned int arg8_3 = arg24 & 0xff;

        switch (op) {
        case ABORT:
            throw_run_error("hit ABORT code");
        case HALT:
            return do_halt;
        case ALLOCATE_NOPTR:
            {
                int size = untag(regs.data_block[arg8_2]);
                regs.data_block[arg8_1] = allocate_noptr(size);
            }
            break;
        case ALLOCATE_NOPTR_imm16:
            regs.data_block[arg8_1] = allocate_noptr(arg16);
            break;
        case ALLOCATE_ALLPTR:
            {
                int size = untag(regs.data_block[arg8_2]);
                regs.data_block[arg8_1] = allocate_allptr(size);
            }
            break;
        case ALLOCATE_ALLPTR_imm16:
            regs.data_block[arg8_1] = allocate_allptr(arg16);
            break;
        case ALLOCATE_CONS:
            regs.data_block[arg8_1] = allocate_cons();
            break;
        case GET_CBLK:
            regs.data_block[arg8_1] = regs.code_block;
            break;
        case GET_INST:
            regs.data_block[arg8_1] = tagint(regs.icount);
            break;
        case GET_DBLK:
            regs.data_block[arg8_1] = regs.data_block;
            break;
        case READ_FAR:
            regs.data_block[arg8_1]
                = ((void **) regs.data_block[arg8_2])[arg8_3];
            break;
        case WRITE_FAR:
            ((void **) regs.data_block[arg8_1])[arg8_2]
                = regs.data_block[arg8_3];
            break;
        //TODO for out-of-heap jumps, transfer out of vm
        case JUMP_FAR:
            regs.code_block = regs.data_block[arg8_1];
            regs.icount = untag(regs.data_block[arg8_2]);
            regs.data_block = regs.data_block[arg8_3];
            end = get_header(regs.code_block)->size / sizeof(void *);
            break;
        case JUMP_FAR_imm8:
            regs.code_block = regs.data_block[arg8_1];
            regs.icount = arg8_2;
            regs.data_block = regs.data_block[arg8_3];
            end = get_header(regs.code_block)->size / sizeof(void *);
            break;
        case JUMP_AREC:
            regs.data_block[0] = regs.code_block;
            regs.data_block[1] = tagint(regs.icount);

            regs.data_block = regs.data_block[arg8_1];
            regs.code_block = regs.data_block[0];
            regs.icount = untag(regs.data_block[1]);

            end = get_header(regs.code_block)->size / sizeof(void *);
            break;
        case CONST_imm16:
            regs.data_block[arg8_1] = tagint(arg16);
            break;
        case CONST_FAR_imm8:
            ((void **) regs.data_block[arg8_1])[arg8_2] = tagint(arg8_3);
            break;
        case CONST_imm16_raw:
            regs.data_block[arg8_1] = (void *) arg16;
            break;
        case SET_16l:
            {
                int low16 = arg16 << 2;
                int high14 = (uint32_t) regs.data_block[arg8_1] & 0xfffc0000;
                regs.data_block[arg8_1] = (void *) (high14 | low16 | 0b11);
            }
            break;
        case SET_16l_raw:
            {
                int low16 = arg16 & 0xffff;
                int high16 = (uint32_t) regs.data_block[arg8_1] & 0xffff0000;
                regs.data_block[arg8_1] = (void *) (high16 | low16);
            }
            break;
        case SET_14h:
            {
                int low16 = (uint32_t) regs.data_block[arg8_1] & 0xffff;
                int high14 = (arg16 & 0x3fff) << 18;
                regs.data_block[arg8_1] = (void *) (high14 | low16);
            }
            break;
        case SET_16h_raw:
            {
                int low16 = (uint32_t) regs.data_block[arg8_1] & 0xffff;
                int high16 = arg16 << 16;
                regs.data_block[arg8_1] = (void *) (high16 | low16);
            }
            break;
        case ADD:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                uint32_t raw3 = untag(regs.data_block[arg8_3]);
                regs.data_block[arg8_1] = tagint(raw2 + raw3);
            }
            break;
        case ADD_imm8:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                regs.data_block[arg8_1] = tagint(raw2 + arg8_3);
            }
            break;
        case ADD_raw:
            {
                int raw2 = (int) regs.data_block[arg8_2];
                int raw3 = (int) regs.data_block[arg8_3];
                regs.data_block[arg8_1] = (void *) (raw2 + raw3);
            }
            break;
        case MUL:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                uint32_t raw3 = untag(regs.data_block[arg8_3]);
                regs.data_block[arg8_1] = tagint(raw2 * raw3);
            }
            break;
        case AND:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                uint32_t raw3 = untag(regs.data_block[arg8_3]);
                regs.data_block[arg8_1] = tagint(raw2 & raw3);
            }
            break;
        case OR:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                uint32_t raw3 = untag(regs.data_block[arg8_3]);
                regs.data_block[arg8_1] = tagint(raw2 | raw3);
            }
            break;
        case OR_raw:
            {
                int raw2 = (int) regs.data_block[arg8_2];
                int raw3 = (int) regs.data_block[arg8_3];
                regs.data_block[arg8_1] = (void *) (raw2 | raw3);
            }
            break;
        case XOR:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                uint32_t raw3 = untag(regs.data_block[arg8_3]);
                regs.data_block[arg8_1] = tagint(raw2 ^ raw3);
            }
            break;
        case LSHIFT_imm8:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                regs.data_block[arg8_1] = tagint(raw2 << arg8_3);
            }
            break;
        case LSHIFT_imm8_raw:
            {
                int raw = (int) regs.data_block[arg8_2];
                regs.data_block[arg8_1] = (void *) (raw << arg8_3);
            }
            break;
        case RSHIFT_imm8:
            {
                uint32_t raw2 = untag(regs.data_block[arg8_2]);
                regs.data_block[arg8_1] = tagint(raw2 >> arg8_3);
            }
            break;
        case DEBUG_WRITEHEX_raw:
            printf("0x%08x%c", regs.data_block[arg8_1], arg8_3);
            break;
        case DEBUG_WRITEHEX_int:
            printf("0x%08x%c", untag(regs.data_block[arg8_1]), arg8_3);
            break;
        }
    }

    throw_run_error("ran off code block end");
}

uint32_t op111(enum opcodes op, uint8_t arg8_1, uint8_t arg8_2, uint8_t arg8_3)
{
    return op << 24 | arg8_1 << 16 | arg8_2 << 8 | arg8_3;
}

uint32_t op12(enum opcodes op, uint8_t arg8_1, uint16_t arg16) {
    return op << 24 | arg8_1 << 16 | arg16;
}

uint32_t op1(enum opcodes op, uint8_t arg8_1) {
    return op << 24 | arg8_1 << 16;
}

uint32_t op(enum opcodes op) {
    return op << 24;
}

void create_test_sub() {
    enum {
        cblk=0,
        icount=1,
        link_value=2,
        dyn_parent=3,
        stat_parent=4,
        sub_descr=5,
        sub_arec=6,
        sub_cblk=7,
        temp=8,
    };
    enum opcodes source[] = {
        // subprogram activation record
        op12(ALLOCATE_ALLPTR_imm16, sub_arec, 6*sizeof(void *)),
        // subprogram code block
        op12(ALLOCATE_NOPTR_imm16, sub_cblk, 3*sizeof(void *)),

        // set up subprogram's activation record
        // code block and instruction counter
        op111(WRITE_FAR, sub_arec, cblk, sub_cblk),
        op111(CONST_FAR_imm8, sub_arec, icount, 0),
        // our data block (activation record) is subprogram's dynamic parent
        op1(GET_DBLK, temp),
        op111(WRITE_FAR, sub_arec, dyn_parent, temp),

        // add 0x55 to argument
        op12(SET_16h_raw, temp, (ADD_imm8 << 8) + link_value),
        op12(SET_16l_raw, temp, (link_value << 8) + 0x55),
        op111(WRITE_FAR, sub_cblk, 0, temp),

        // store result into dynamic parent
        op12(SET_16h_raw, temp, (WRITE_FAR << 8) + dyn_parent),
        op12(SET_16l_raw, temp, (link_value << 8) + link_value),
        op111(WRITE_FAR, sub_cblk, 1, temp),

        // return to dynamic parent
        op12(SET_16h_raw, temp, (JUMP_AREC << 8) + dyn_parent),
        op12(SET_16l_raw, temp, 0),
        op111(WRITE_FAR, sub_cblk, 2, temp),

        op111(CONST_FAR_imm8, sub_arec, link_value, 0xaa), // argument is 0xaa
        op1(JUMP_AREC, sub_arec),  // jump to subprogram

        op12(DEBUG_WRITEHEX_int, link_value, '\n'),
        op(HALT),
    };

    regs.data_block = allocate_allptr(10 * sizeof(void *));
    regs.code_block = allocate_noptr(sizeof(source));
    memmove(regs.code_block, source, sizeof(source));
    regs.icount = 0;
}

// call a virtual machine subprogram from c
void * call_virtual(int which, void * arg) {
    //TODO something about setting up the context to return to
}

// call c subprogram from virtual machine
struct registers handle_jump_c(struct registers WHUT) {
    //TODO replace with specific opcodes
}

// end virtual machine

// begin symbol trie
//TODO reduce fanout to 16 by adding intermediate nodes

void * allocate_symbol_intern_node() {
    void * temp = allocate_allptr(sizeof(struct symbol_intern_node));
    if (! temp) die("can't allocate symbol node");
    return temp;
}

struct symbol_intern_node * symbols;

void * get_symbol_ref_n_in(struct heap * heap, char * name, int length) {
    struct symbol_intern_node * node;
    node = (struct symbol_intern_node *) heap->roots[SYMBOLS]->data;

    for (int n=0 ; n < length ; n += 1) {
        struct symbol_intern_node * next_node = node->child[name[n]];
        if (! next_node) {
            next_node = allocate_symbol_intern_node();
            next_node->name = allocate_noptr(n+2);
            memmove(next_node->name, name, n+1);
            node->child[name[n]] = next_node;
        }
        node = next_node;
    }

    return (void *) node->name;
}

void * get_symbol_ref_n(char * name, int length) {
    return get_symbol_ref_n_in(& heap0, name, length);
}

void * get_symbol_ref(char * name) {
    return get_symbol_ref_n(name, strlen(name));
}

// end symbol trie

// begin cons tree

struct maybe_item nothing = {false};
struct maybe_item just(struct item value) {
    return (struct maybe_item) {true, value};
}

void * allocate_cons() {
    return allocate(sizeof(struct cons), cons_layout);
}

struct item nil = {cons_tag, NULL};
struct item ch(int c) {
    return (struct item) {char_tag, (void *) c};
}
struct item num(int n) {
    return (struct item) {int_tag, (void *) n};
}
struct item sym(char * name) {
    return (struct item) {sym_tag, (void *) get_symbol_ref(name)};
}
struct item sym_n(char * name, int length) {
    return (struct item) {sym_tag, (void *) get_symbol_ref_n(name, length)};
}
struct item cons(struct item head, struct item tail) {
    struct cons * cell = allocate_cons();
    cell->head = head;
    cell->tail = tail;
    return (struct item) {cons_tag, cell};
}

void print_cons_item(struct item item) {
    struct cons * cell = item.ptr;

    switch (item.tag) {
        case char_tag:
            fprintf(stdout, "#\\%c", (int) item.ptr);
            break;
        case int_tag:
            fprintf(stdout, "%u", (unsigned int) item.ptr);
            break;
        case sym_tag:
            fprintf(stdout, "%s", (char *) item.ptr);
            break;
        case cons_tag:
            fputc('(', stdout);
            if (cell) print_tail_cons(cell);
            fputc(')', stdout);
            break;
        case obj_tag:
            fprintf(stdout, "<obj>");
            break;
    }
}

void print_tail_cons(struct cons * cell) {
    print_cons_item(cell->head);

    switch (cell->tail.tag) {
        case char_tag:
        case int_tag:
        case sym_tag:
        case obj_tag:
            fputs(" . ", stdout);
            print_cons_item(cell->tail);
            break;
        case cons_tag:
            if (cell->tail.ptr) {
                fputc(' ', stdout);
                print_tail_cons(cell->tail.ptr);
            }
            break;
    }
}

// end cons tree

// begin parse sexps

jmp_buf abort_parse;

void throw_parse_error(char * message) {
    printf("%s\n", message);
    longjmp(abort_parse, 1);
}

void skip_space(char ** text_ptr) {
    while (isspace(** text_ptr)) * text_ptr += 1;
}

bool symbol_char(char c) {
    switch (c) {
        case '\0':
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case '"':
        case ',':
        case '\'':
        case '`':
        case ';':
        case '|':
        case '\\':
            return false;

        default:
            return ! isspace(c);
    }
}

struct item parse_tail_cons(char ** text_ptr) ;

struct maybe_item parse_cons_item(char ** text_ptr) {
    char * text = * text_ptr;

    switch (text[0]) {
        case '\0':
            return nothing;

        case '#':
            if (text[1] == '\0' || text[2] == '\0') {
                throw_parse_error("end of text in character literal");
            }
            if (text[1] != '\\') throw_parse_error("bad character literal");
            * text_ptr += 3;
            return just(ch(text[2]));

        case '0' ... '9':
            {
                int num_digits = 0;
                int temp = 0;
                do temp = temp*10 + text[0] - '0';
                while (num_digits+=1, isdigit(* ++text));
                * text_ptr += num_digits;
                return just(num(temp));
            }

        case '(':
            {
                * text_ptr += 1;

                skip_space(text_ptr);
                if (** text_ptr == ')') {
                    * text_ptr += 1;
                    return just(nil);
                }

                struct maybe_item maybe_head = parse_cons_item(text_ptr);
                if (! maybe_head.present) throw_parse_error("expected item");

                skip_space(text_ptr);
                struct item tail = parse_tail_cons(text_ptr);

                skip_space(text_ptr);
                if (** text_ptr != ')') throw_parse_error("expected )");
                else * text_ptr += 1;

                return just(cons(maybe_head.v, tail));
            }

        case ')':
            return nothing;

        case '.':
            return nothing;

        default:   // anything else is a symbol
            if (symbol_char(* text)) {
                char * cur = text;
                for (char c = * cur ; symbol_char(c) ; c = * cur++) {}
                int length = cur - text - 1;
                * text_ptr += length;
                return just(sym_n(text, length));
            }
            else return nothing;
    }
}

struct item parse_tail_cons(char ** text_ptr) {
    if (** text_ptr == '.') {
        * text_ptr += 1;

        skip_space(text_ptr);
        struct maybe_item temp = parse_cons_item(text_ptr);
        if (! temp.present) throw_parse_error("expected item");
        else return temp.v;
    }

    //skip_space(text_ptr);
    struct maybe_item maybe_head = parse_cons_item(text_ptr);
    if (! maybe_head.present) return nil;

    skip_space(text_ptr);
    struct item tail = parse_tail_cons(text_ptr);

    return cons(maybe_head.v, tail);
}

struct maybe_item parse(char * line) {
    if (setjmp(abort_parse)) {
        printf("parse error\n");
        return nothing;
    }
    else {
        skip_space(& line);
        struct maybe_item temp = parse_cons_item(& line);

        skip_space(& line);
        if (* line) {
            printf("unexpected %c\nparse error\n", * line);
            return nothing;
        }

        return temp;
    }
}

// end parse

void die(char * message) {
    printf("%s\n", message);
    exit(1);
}

/*
struct item lisp_eval_step(struct to_eval exp) {
}
*/

/*
//TODO figure out return values
void * read_sexp() {
    char * line = readline("~> ");
    if (! line) return NULL;
    if (* line) add_history(line);
    struct maybe_item maybe_tree = parse(line);
    if (maybe_tree.present) return maybe_tree.v;
    else return nil;
}
*/

/*
   HOW TO REPL
struct maybe_item maybe_tree = parse(line);
if (maybe_tree.present) {
    print_cons_item(maybe_tree.v); putchar('\n');
}
*/

void print_regs() {
    printf("data=0x%08x code=0x%08x instruction=%u\n",
           regs.data_block, regs.code_block, regs.icount);
}

void print_disassembly(void ** code, int size) {
    for (int ix=0 ; ix < size/sizeof(void *) ; ix+=1) {
        uint32_t instruction = (uint32_t) code[ix];

        enum opcodes op = instruction >> 24;
        unsigned int arg24 = instruction & 0xffffff;
        unsigned int arg8_1 = arg24 >> 16;
        unsigned int arg16 = arg24 & 0xffff;
        unsigned int arg8_2 = (arg24 >> 8) & 0xff;
        unsigned int arg8_3 = arg24 & 0xff;

        printf("0x%08x - %s ", instruction, opcode_to_mnemonic(op));
        switch (arg_pattern(op)) {
        case 0:
            break;
        case 1:
            printf("0x%02x", arg8_1);
            break;
        case 11:
            printf("0x%02x 0x%02x", arg8_1, arg8_2);
            break;
        case 12:
            printf("0x%02x 0x%04x", arg8_1, arg16);
            break;
        case 111:
            printf("0x%02x 0x%02x 0x%02x", arg8_1, arg8_2, arg8_3);
            break;
        default:
            die("bad arg pattern");
        }

        putchar('\n');
    }
}

void monitor() {
    char * line;
    while (line = readline("mon> ")) {
        if (! line) break;
        if (* line) add_history(line);

        char * text_ptr = line;

        skip_space(& text_ptr);
        char command = * text_ptr;
        text_ptr += 1;

        if (command == 'q') break;

        char * end_ptr = text_ptr;
        unsigned int argument = strtoumax(text_ptr, & end_ptr, 0);
        bool has_argument = (end_ptr != text_ptr);

        switch (command) {
        case 'r':
            print_regs();
            break;
        case 'C':
            if (has_argument) {
                regs.code_block = (void *) argument;
                print_regs();
            }
            else printf("need value\n");
            break;
        case 'I':
            if (has_argument) {
                regs.icount = (unsigned int) argument;
                print_regs();
            }
            else printf("need value\n");
            break;
        case 'D':
            if (has_argument) {
                regs.data_block = (void *) argument;
                print_regs();
            }
            else printf("need value\n");
            break;
        case 'g':
            //TODO start vm
            break;
        case 'd':
            if (has_argument) {
                void ** code_block = (void **) argument;
                print_disassembly(code_block, get_header(code_block)->size);
            }
            else printf("need value\n");
            break;
        case 'b':
            //TODO display block
            break;
        case 'h':
            //TODO display header
            break;
        default:
            printf("unknown command '%c'\n", command);
            break;
        }

        free(line);
    }

quit:
    putchar('\n');
}

int main(int argc, char * argv[]) {
    using_history();

    init_heap();

    create_test_sub();

    /*
    struct do_next do_next;
do_run:
    do_next = run();

    switch (do_next.action) {
    case abort_code:
        die("ABORT");
    case halt_code:
        puts("halted");
        break;
    case jump_c:
        regs = handle_jump_c(do_next.regs);
        goto do_run;
    default:
        die("unknown do_next.action");
    }
    */

    monitor();
}
