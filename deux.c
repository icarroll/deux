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
    return ((void *) heap->start <= addr && addr < heap->next);
}

struct block_header * get_header(void * block_ptr) {
    return (struct block_header *) (block_ptr - hdr_sz);
}

void print_block_header(struct block_header * header) {
    printf("0x%x: link=%x marked=%s layout=%s size=%dx%d=%d note=%.*s next@0x%x\n",
           header, header->link_ptr, bool_str(header->marked),
           layout_str(header->layout), header->size/sizeof(void *),
           sizeof(void *), header->size, 4, & header->note,
           following_header(header));
}

void print_block(struct block_header * header) {
    print_block_header(header);

    switch (header->layout) {
    case no_ptr_layout:
        /*
        for (void ** item = (void **) header->data
             ; item < (void **) ((void *) header->data + header->size)
             ; item += 1) {
        }
        */
        print_hexdump(header->data, header->size);
        break;
    case all_ptr_layout:
        for (int ix=0 ; ix < header->size/sizeof(void *) ; ix+=1) {
            void * value = header->data[ix];
            printf("% 4d: 0x%08x", ix, value);
            switch ((uint32_t) value & 0b11) {
            case 0b00:   // pointer
                {
                    if (in_heap(value)) {
                        struct block_header * pheader = get_header(value);
                        printf(" -> layout=%s size=%d note=%.*s",
                               layout_str(pheader->layout), pheader->size,
                               4, & pheader->note);
                    }
                }
                break;
            case 0b11:   // uint32_t
                printf(" = int:%u", untag(value));
                break;
            default:
                die("unhandled value tag");
            }

            putchar('\n');
        }
        break;
    default:
        die("unhandled block layout");
    }
}

void print_mark_list(struct block_header * header) {
    while (header) {
        print_block_header(header);
        header = header->link_ptr;
    }
}

void (* pre_collect_hook)() = NULL;
void (* add_roots_hook)(struct block_header **) = NULL;
void (* update_roots_hook)() = NULL;

void mark_in(struct heap * heap) {
    // mark roots and add to list
    struct block_header * next_header = heap->roots[0];
    heap->roots[0]->marked = true;
    //printf("root mark %x\n", heap->roots[0]); //XXX
    struct block_header * last_header = next_header;

    for (int ix=1 ; ix < NUM_ROOTS ; ix+=1) {
        last_header->link_ptr = heap->roots[ix];
        last_header->marked = true;
        //printf("root mark %x\n", last_header); //XXX
        last_header = last_header->link_ptr;
    }

    if (regs.code_block) {
        last_header->link_ptr = regs.code_block;
        last_header->marked = true;
        //printf("root mark %x\n", last_header); //XXX
        last_header = last_header->link_ptr;
    }

    if (regs.data_block) {
        last_header->link_ptr = regs.data_block;
        last_header->marked = true;
        //printf("root mark %x\n", last_header); //XXX
        last_header = last_header->link_ptr;
    }

    last_header->link_ptr = NULL;

    if (add_roots_hook) add_roots_hook(& next_header);

    while (next_header) {
        struct block_header * cur_header = next_header;

        switch (cur_header->layout) {
        case no_ptr_layout:
            //printf("skip trace in %x (no_ptr)\n", cur_header); //XXX
            break;
        case all_ptr_layout:
            {
                //printf("trace in %x\n", cur_header); //XXX
                void ** end = cur_header->data
                              + cur_header->size / sizeof(void *);
                for (void ** candidate = cur_header->data
                     ; candidate < end
                     ; candidate += 1) {
                    //printf("    @%x :", candidate); //XXX
                    // don't follow null or unaligned or out of heap pointers
                    if (* candidate && ! ((int) * candidate & 0b11) && in_heap_in(heap, candidate)) {
                        void * block = * candidate;
                        struct block_header * header = get_header(block);
                        if (! header->marked) {
                            header->link_ptr = NULL;
                            header->marked = true;
                            //printf(" mark %x\n", header); //XXX
                            last_header->link_ptr = header;
                            last_header = header;
                        }
                        //else printf(" skip (marked) %x\n", header); //XXX
                    }
                    //else printf(" skip (candidate) %x\n", *candidate); //XXX
                }
            }
            break;
        default:
            die("unhandled block layout");
        }

        next_header = next_header->link_ptr;
    }
}

void * compute_forward_addrs_in(struct heap * heap) {
    void * next_free_addr = heap->start;
    for (struct block_header * header=(struct block_header *) heap->start
         ; header < (struct block_header *) heap->next
         ; header = following_header(header)) {
        if (header->marked) {
            header->link_ptr = next_free_addr;
            //printf("forward %x -> %x\n", header, next_free_addr); //XXX
            next_free_addr += hdr_sz + header->size;
        }
        //else printf("skip (forward) %x\n", header); //XXX
    }

    return next_free_addr;
}

//TODO factor out should_forward() and also check in_heap()
void update_pointers_in(struct heap * heap) {
    // scan over heap
    for (struct block_header * header=(struct block_header *) heap->start
         ; header < (struct block_header *) heap->next
         ; header = following_header(header)) {
        //if (! header->marked) printf("skip (update) %x\n", header); //XXX
        if (! header->marked) continue;   // skip unmarked blocks
        void * block = header->data;

        switch (header->layout) {
        case no_ptr_layout:
            break;
        case all_ptr_layout:
            // scan over pointers in block
            //printf("update in %x\n", header); //XXX
            for (void ** candidate = (void **) block
                 ; candidate < (void **) (block + header->size)
                 ; candidate += 1) {
                // forward any non-zero aligned pointers
                //printf("    @%x :", candidate); //XXX
                if (* candidate && ! ((int) * candidate & 0b11)) {
                    //printf(" %x ->", *candidate); //XXX
                    * candidate = get_header(* candidate)->link_ptr + hdr_sz;
                    //printf(" %x\n", *candidate); //XXX
                }
                //else printf(" skip (candidate) %x\n", *candidate); //XXX
            }
            break;
        default:
            die("unhandled block layout");
        }
    }
}

void compact_in(struct heap * heap) {
    for (struct block_header * header=(struct block_header *) heap->start
         ; header < (struct block_header *) heap->next
         ; header = following_header(header)) {
        if (header->marked) {
            struct block_header * dest_header;
            dest_header = (struct block_header *) (header->link_ptr);
            memmove(dest_header, header, hdr_sz + header->size);

            //printf("compact %x -> %x\n", header, dest_header); //XXX

            dest_header->link_ptr = NULL;
            dest_header->marked = false;
        }
        //else printf("skip (compact) %x\n", header); //XXX
    }
}

void collect_in(struct heap * heap) {
    if (pre_collect_hook) pre_collect_hook();

    mark_in(heap);
    void * new_next = compute_forward_addrs_in(heap);

    if (update_roots_hook) update_roots_hook();

    // save new root locations
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

uint32_t make_note(char * c) {
    if (strnlen(c, 4) != 4) die("block note too short");
    return c[3]<<24 | c[2]<<16 | c[1]<<8 | c[0];
}

void * allocate_in(struct heap * heap, int size, enum layout layout) {
    if (! heap_ok_in(heap)) hurl();
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

    header->note = make_note("shrg");

    if (! heap_ok_in(heap)) hurl();
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

    heap->roots[0] = get_header(
            allocate_in(heap, ROOT_BLOCK_SIZE, all_ptr_layout));
}

void load_heap(struct heap ** heap_ptr) {
    //TODO
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
    return (void *) header + hdr_sz + header->size;
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

    for (struct block_header * header=(struct block_header *) heap->start
         ; header < (struct block_header *) heap->next
         ; header = following_header(header)) {
        print_block_header(header);

        if (header->size <= 0) {
            printf("size error\n");
            break;
        }

        void * block = header->data;

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

void die(char * message) {
    printf("%s\n", message);
    exit(1);
}

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

void skip_space(char ** text_ptr) {
    while (isspace(** text_ptr)) * text_ptr += 1;
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
