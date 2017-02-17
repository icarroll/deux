#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>

#include "deux.h"
#include "mnemonics.h"

void hurl() {
    printf("not ok\n");
    fflush(0);
    raise(SIGSEGV);
}

// memory management

struct registers regs;

bool in_heap_in(struct heap * heap, void * addr) {
    return ((void *) heap->start <= addr && addr < heap->end);
}

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
    void * next_block = block + get_header(block)->size + hdr_sz;
    //if (! in_heap(next_block)) hurl();
    return next_block;
}

void * compute_forward_addrs_in(struct heap * heap) {
    void * next_free_addr = heap->start;
    for (void * block=((void *) heap->start) + hdr_sz
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
    for (void * block=((void *) heap->start) + hdr_sz
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
    for (void * block=((void *) heap->start) + hdr_sz
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

    int entire_size = round_to(ALIGNMENT, hdr_sz + size);
    int data_size = entire_size - hdr_sz;

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
    header->size = data_size;

    memset(header->data, 0, data_size);

    heap->next = new_next;
    return header->data;
}

void make_heap(struct heap ** heap_ptr, int size) {
    int rounded_size = round_to(getpagesize(), size);

    void * memory = mmap(NULL, rounded_size, PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (! memory) die("can't create heap");

    * heap_ptr = (struct heap *) memory;
    struct heap * heap = * heap_ptr;


    heap->memory = memory;
    heap->end = memory + rounded_size;
    heap->next = heap->start;

    heap->roots[SYMBOLS] = get_header(allocate_symbol_intern_node());
}

void load_heap(struct heap ** heap_ptr) {
    //open("system_image.bin", WHUT);
    die("can't load heap");
}

struct heap * heap0;

bool in_heap(void * addr) {
    return in_heap_in(heap0, addr);
}

void collect() {
    collect_in(heap0);
}

void * allocate(int size, enum layout layout) {
    return allocate_in(heap0, size, layout);
}

void * allocate_noptr(int size) {
    return allocate(size, no_ptr_layout);
}

void * allocate_allptr(int size) {
    return allocate(size, all_ptr_layout);
}

struct block_header * following_header(struct block_header * header) {
    return header + hdr_sz + header->size;
}

bool heap_ok_in(struct heap * heap) {
    for (struct block_header * header=(struct block_header *) heap->start
         ; header < (struct block_header *) heap->next
         ; header = following_header(header)) {
        if (header->size <= 0) {
            return false;
        }
        if (! in_heap_in(heap, header)) {
            return false;
        }
        if ((int) header & 0b11) {
            return false;
        }
    }

    return true;
}

bool heap_ok() {
    return heap_ok_in(heap0);
}

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

void print_hexdump(void * addr, int length) {
    for (int ix=0 ; ix < length ; ix+=16) {
        printf("%08x: ", addr+ix);
        for (int seg=0 ; seg < 4 ; seg+=1) {
            for (int ix2=0 ; ix2 < 4 ; ix2+=1) {
                printf("%02x ", ((uint8_t *) addr)[ix+seg*4+ix2]);
            }
            putchar(' ');
        }

        for (int jx=0 ; jx < 16 ; jx+=1) {
            char c = ((char *) addr)[ix+jx];
            putchar(isprint(c) ? c : '.');
        }
        putchar('\n');
    }
}

void print_heap_in(struct heap * heap) {
    printf("memory=%x end=%x next=%x start=%x\n",
           heap->memory, heap->end, heap->next, heap->start);

    for (int ix=0 ; ix < NUM_ROOTS ; ix+=1) {
        printf("heap->roots[%d]=%x\n", ix, heap->roots[ix]);
    }

    for (void * block=((void *) heap->start) + hdr_sz
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
    printf("end\n");
}

void print_heap() {
    print_heap_in(heap0);
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
        //TODO check for out-of-heap jumps
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
    return get_symbol_ref_n_in(heap0, name, length);
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

struct cons * conscell(struct item head, struct item tail) {
    struct cons * cell = allocate_cons();
    cell->head = head;
    cell->tail = tail;
    return cell;
}
struct item cons_to_item(struct cons * cell) {
    return (struct item) {cons_tag, cell};
}
struct item cons(struct item head, struct item tail) {
    return cons_to_item(conscell(head, tail));
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
            if (item.ptr) fprintf(stdout, "%s", (char *) item.ptr);
            else fprintf(stdout, "-0-");
            break;
        case cons_tag:
            if (cell && cell->head.tag == sym_tag
                && cell->head.ptr
                && ((char *) cell->head.ptr)[0] == '#') {
                fprintf(stdout, cell->head.ptr);
                break;
            }
            fputc('(', stdout);
            if (cell) print_tail_cons(cell);
            fputc(')', stdout);
            break;
        case obj_tag:
            fprintf(stdout, "<obj>");
            break;
        default:
            die("unknown tag in print cons item");
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
        default:
            die("unknown tag in print tail cons");
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

        case '\'':
            {
                * text_ptr += 1;
                skip_space(text_ptr);
                struct maybe_item quoted = parse_cons_item(text_ptr);
                if (! quoted.present) throw_parse_error("expected quoted item");
                return just(cons(sym("quote"), cons(quoted.v, nil)));
            }

        case '`':
            {
                * text_ptr += 1;
                skip_space(text_ptr);
                struct maybe_item quoted = parse_cons_item(text_ptr);
                if (! quoted.present) throw_parse_error("expected quasiquoted item");
                return just(cons(sym("quasiquote"), cons(quoted.v, nil)));
            }

        case ',':
            {
                * text_ptr += 1;
                if (** text_ptr == '@') {
                    * text_ptr += 1;
                    skip_space(text_ptr);
                    struct maybe_item quoted = parse_cons_item(text_ptr);
                    if (! quoted.present) throw_parse_error("expected unquoted item");
                    return just(cons(sym("unquote-splicing"), cons(quoted.v, nil)));
                }
                else {
                    skip_space(text_ptr);
                    struct maybe_item quoted = parse_cons_item(text_ptr);
                    if (! quoted.present) throw_parse_error("expected unquoted item");
                    return just(cons(sym("unquote"), cons(quoted.v, nil)));
                }
            }

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

    // skip_space() has already been done at this point
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

// lisp interpreter

jmp_buf abort_eval;

void throw_eval_error(char * message) {
    printf("%s\n", message);
    longjmp(abort_eval, 1);
}

struct maybe_item assoc_get(struct cons * haystack, void * needle) {
    while (haystack) {
        //TODO check that head is a cons
        struct cons * candidate = (struct cons *) haystack->head.ptr;
        if (candidate->head.ptr == needle) return just(candidate->tail);
        //TODO check that tail is a cons
        else haystack = (struct cons *) haystack->tail.ptr;
    }
    return nothing;
}

void assoc_set(struct cons * haystack, void * needle, struct item value) {
    while (haystack) {
        //TODO check that head is a cons
        struct cons * candidate = (struct cons *) haystack->head.ptr;
        if (candidate->head.ptr == needle) {
            candidate->tail = value;
            return;
        }
        //TODO check that tail is a cons
        else haystack = (struct cons *) haystack->tail.ptr;
    }
    throw_eval_error("unbound symbol");
}

struct cons * get_cell(struct item cons_item) {
    return (struct cons *) cons_item.ptr;
}
struct cons * head_cons(struct cons * cell) {
    return (struct cons *) cell->head.ptr;
}
struct cons * tail_cons(struct cons * cell) {
    return (struct cons *) cell->tail.ptr;
}

struct item head_item(struct item cons_item) {
    return cons_to_item(head_cons(get_cell(cons_item)));
}

bool is_nil(struct item item) {
    return item.tag == cons_tag && item.ptr == NULL;
}
bool is_cons(struct item item) {
    return item.tag == cons_tag;
}

bool match_sym(struct item item, char * str) {
    return item.tag == sym_tag && item.ptr == get_symbol_ref(str);
}

struct item lisp_eval_rec(struct item exp, struct cons * env) {
    switch (exp.tag) {
    case char_tag:
    case int_tag:
    case obj_tag:   // objs should probably self-evaluate
        return exp;
    case sym_tag:
        {
            struct maybe_item value = assoc_get(env, exp.ptr);
            if (value.present) return value.v;
            else throw_eval_error("unbound symbol");
        }
    case cons_tag:
        // check for special form
        // if special form, invoke
        // if not special form, eval each item and invoke
        {
            struct cons * cell = get_cell(exp);
            if (! cell) return nil;
            if (cell->head.tag == sym_tag) {
                void * symbol = cell->head.ptr;
                // "quote" special form
                if (symbol == get_symbol_ref("quote")) {
                    if (is_nil(cell->tail)) throw_eval_error("bad quote");
                    if (! is_cons(cell->tail)) throw_eval_error("bad quote");
                    struct cons * tail = tail_cons(cell);
                    if (! is_cons(tail->tail)) throw_eval_error("bad quote");
                    if (! is_nil(tail->tail)) throw_eval_error("bad quote");
                    return tail->head;
                }
                // "if" special form
                if (symbol == get_symbol_ref("if")) {
                    if (! is_cons(cell->tail)) throw_eval_error("bad if");
                    struct cons * clause = tail_cons(cell);
do_if:
                    // no clauses -> no op
                    if (! clause) return nil;
                    // one clause -> eval and return
                    if (is_nil(clause->tail)) {
                        return lisp_eval_rec(clause->head, env);
                    }
                    // two or more clauses ->
                    // if first evals true then return eval of second
                    // otherwise skip to next clause(s)
                    struct item condition = lisp_eval_rec(clause->head, env);
                    if (condition.ptr) {
                        struct item action = tail_cons(clause)->head;
                        return lisp_eval_rec(action, env);
                    }
                    clause = tail_cons(tail_cons(clause));
                    goto do_if;
                }
                // "fn" special form
                if (symbol == get_symbol_ref("fn")) {
                    if (! is_cons(cell->tail)) throw_eval_error("bad fn");
                    if (is_nil(cell->tail)) throw_eval_error("bad fn");
                    struct cons * tail = tail_cons(cell);
                    struct item argspec = tail->head;
                    struct item body = tail->tail;
                    return cons(sym("#<subprogram>"),
                           cons(argspec,
                           cons(body,
                           cons(cons_to_item(env),
                                nil))));
                }
                // "rewrite" special form
                if (symbol == get_symbol_ref("rewrite")) {
                    if (! is_cons(cell->tail)) throw_eval_error("bad rewrite");
                    if (is_nil(cell->tail)) throw_eval_error("bad rewrite");
                    struct cons * tail = tail_cons(cell);
                    struct item argspec = tail->head;
                    struct item body = tail->tail;
                    return cons(sym("#<rewrite>"),
                           cons(argspec,
                           cons(body,
                           cons(cons_to_item(env),
                                nil))));
                }
                // "new" special form
                if (symbol == get_symbol_ref("new")) {
                    //TODO actually check for errors
                    struct cons * current = tail_cons(cell);
                    struct item var = current->head;
                    current = tail_cons(current);
                    struct item val = lisp_eval_rec(current->head, env);
                    struct item snd = env->tail;
                    struct item ins = cons(cons(var, val), snd);
                    env->tail = ins;
                    return val;
                }
                // "set" special form
                if (symbol == get_symbol_ref("set")) {
                    //TODO actually check for errors
                    struct cons * current = tail_cons(cell);
                    struct item var = current->head;
                    current = tail_cons(current);
                    struct item val = lisp_eval_rec(current->head, env);
                    assoc_set(env, var.ptr, val);
                    return val;
                }
            }

            // not special form so evaluate and invoke
            {
                struct item to_invoke = lisp_eval_rec(cell->head, env);
                if (is_cons(to_invoke)) {
                   if (match_sym(get_cell(to_invoke)->head, "#<subprogram>")) {
                       struct cons * evaluated = conscell(nil, nil);

                       struct cons * current_read = cell;
                       struct cons * current_write = evaluated;
                       while (is_cons(current_read->tail)
                              && current_read->tail.ptr) {
                           current_write->head
                               = lisp_eval_rec(current_read->head, env);
                           current_write->tail = cons(nil, nil);

                           current_read = tail_cons(current_read);
                           current_write = tail_cons(current_write);
                       }

                       current_write->head = lisp_eval_rec(current_read->head, env);
                       if (current_read->tail.ptr) {
                           current_write->tail
                               = lisp_eval_rec(current_read->tail, env);
                       }
                       else current_write->tail = nil;

                       return lisp_apply_rec(to_invoke, evaluated->tail);
                   }

                   if (match_sym(get_cell(to_invoke)->head, "#<rewrite>")) {
                       return lisp_eval_rec(lisp_apply_rec(to_invoke, cell->tail), env);
                   }
                }
            }

            throw_eval_error("bad invoke");
        }
    default:
        die("unknown tag in eval");
    }

    die("internal error in eval");
}

struct cons * extend_env(struct cons * env, struct item argspec,
                         struct item args) {
    if (! is_cons(argspec)) {
        if (argspec.tag == sym_tag) {
            return conscell(cons(nil, nil),
                   cons(cons(argspec, args),
                        cons_to_item(env)));
        }

        throw_eval_error("bad argspec");
    }

    if (! is_cons(args)) throw_eval_error("bad args");

    struct cons * new_env = env;
    struct cons * vars = get_cell(argspec);
    struct cons * vals = get_cell(args);

    while (vars || vals) {
        if (! vars || ! vals) throw_eval_error("arg mismatch");

        struct item var = vars->head;
        //TODO allow destructuring bind
        if (var.tag != sym_tag) throw_eval_error("bad argspec");
        struct item val = vals->head;
        new_env = conscell(cons(var, val), cons_to_item(new_env));

        vars = tail_cons(vars);
        if (! is_cons(vals->tail)) throw_eval_error("bad args");
        vals = tail_cons(vals);
    }
    return conscell(cons(nil, nil), cons_to_item(new_env));
}

struct item builtin_cons(struct item args) {
    if (args.tag != cons_tag) throw_eval_error("bad cons arg");
    if (! args.ptr) throw_eval_error("bad cons arg");
    struct item hd = get_cell(args)->head;
    if (get_cell(args)->tail.tag != cons_tag) throw_eval_error("bad cons arg");
    if (! get_cell(args)->tail.ptr) throw_eval_error("bad cons arg");
    struct item tl = tail_cons(get_cell(args))->head;
    if (! is_nil(tail_cons(get_cell(args))->tail)) {
        throw_eval_error("bad cons arg");
    }
    return cons(hd, tl);
}

struct item builtin_add(struct item args) {
    uint32_t n = 0;
    while (args.ptr) {
        if (args.tag != cons_tag) throw_eval_error("bad add arg");

        struct cons * cell = get_cell(args);

        if (cell->head.tag == int_tag) n += (uint32_t) cell->head.ptr;
        else throw_eval_error("add non-number");

        args = cell->tail;
    }
    return num(n);
}

char gensym_string[] = "#gensym";
unsigned int gensym_counter = 1;
struct item builtin_gensym(struct item args) {
    if (! is_nil(args)) throw_eval_error("bad gensym arg");
    int counter_digits = snprintf(0, 0, "%u", gensym_counter);
    void * str = allocate_noptr(sizeof(gensym_string) + counter_digits);
    if (! str) die("can't allocate gensym");
    snprintf(str, sizeof(gensym_string) + counter_digits,
             "%s%u", gensym_string, gensym_counter);
    gensym_counter += 1;
    return (struct item) {sym_tag, str};
}

bool do_gc = false;
struct item builtin_gc(struct item args) {
    if (! is_nil(args)) throw_eval_error("bad gc arg");
    do_gc = true;
    return nil;
}

struct item lisp_apply_rec(struct item to_invoke, struct item args) {
    struct cons * current = get_cell(to_invoke);

    current = tail_cons(current);
    struct item argspec = current->head;
    current = tail_cons(current);
    // at this point, current == tail_cons(tail_cons(get_cell(to_invoke)))
    if (current->head.tag == sym_tag) {
        void * builtin = current->head.ptr;
        if (builtin == get_symbol_ref("#<cons>")) return builtin_cons(args);
        if (builtin == get_symbol_ref("#<+>")) return builtin_add(args);
        if (builtin == get_symbol_ref("#<gensym>")) return builtin_gensym(args);
        if (builtin == get_symbol_ref("#<gc>")) return builtin_gc(args);
        throw_eval_error("bad builtin");
    }
    struct cons * body = head_cons(current);
    current = tail_cons(current);
    struct cons * env = head_cons(current);

    struct cons * new_env = extend_env(env, argspec, args);
    struct item result = nil;
    while (body) {
        result = lisp_eval_rec(body->head, new_env);
        body = tail_cons(body);
    }

    return result;
}

// end lisp interpreter

// monitor

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

// end monitor

struct item builtin(char * str) {
    return cons(sym("#<subprogram>"),
           cons(sym("unused"),
           cons(sym(str),
           cons(nil,
                nil))));
}

void al() {
    using_history();

    make_heap(& heap0, HEAP_SIZE);

    struct cons * initial_env
        = conscell(cons(nil, nil),
                   cons(cons(sym("cons"), builtin("#<cons>")),
                   cons(cons(sym("+"), builtin("#<+>")),
                   cons(cons(sym("gensym"), builtin("#<gensym>")),
                   cons(cons(sym("gc"), builtin("#<gc>")),
                        nil)))));
    heap0->roots[LISP_ENV] = get_header(initial_env);

    char * line;
    while(line = readline("al> ")) {
        if (* line) add_history(line);
        struct maybe_item maybe_sexp = parse(line);
        free(line);
        if (maybe_sexp.present) {
            if (setjmp(abort_eval)) {
                printf("eval error\n");
            }
            else {
                struct item result = lisp_eval_rec(
                        maybe_sexp.v,
                        (struct cons *) heap0->roots[LISP_ENV]->data);
                print_cons_item(result);
                putchar('\n');
            }
        }

        if (do_gc) {
            collect();
            do_gc = false;

            if (heap0->roots[LISP_ENV]->data != (void *) initial_env) printf("env moved\n");
        }
    }
    putchar('\n');
}
