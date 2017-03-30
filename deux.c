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

#include "deux.h"
#include "mnemonics.h"

void hurl() {
    printf("not ok\n");
    fflush(0);
    raise(SIGSEGV);
}

// memory management

bool istaggedptr(void * ptr) {
    return ((uint32_t) ptr & 0b11) != 0b11;
}

bool istaggedint(void * ptr) {
    return ((uint32_t) ptr & 0b11) == 0b11;
}

bool istaggedblock(void * ptr) {
    return ((uint32_t) ptr & 0b11) == 0b00;
}

bool istaggedsym(void * ptr) {
    return ((uint32_t) ptr & 0b11) == 0b01;
}

bool istaggedcons(void * ptr) {
    return ((uint32_t) ptr & 0b11) == 0b10;
}

void * tagblockptr(void * ptr) {
    return ptr;
}

void * tagsymptr(void * ptr) {
    return (void *) ((uint32_t) ptr | 0b01);
}

void * tagconsptr(void * ptr) {
    return (void *) ((uint32_t) ptr | 0b10);
}

void * untagptr(void * ptr) {
    return (void *) ((uint32_t) ptr & ~0b11);
}

uint32_t getptrtag(void * ptr) {
    return (uint32_t) ptr & 0b11;
}

void * tagptr(void * ptr, uint32_t tag) {
    return (void *) ((uint32_t) untagptr(ptr) | tag);
}

void * tagint(uint32_t val) {
    return (void *) ((val << 2) | 0b11);
}

uint32_t untagint(void * val) {
    return (uint32_t) val >> 2;
}

struct registers regs;

bool in_heap_in(struct heap * heap, void * addr) {
    return ((void *) heap->start <= addr && addr < heap->next);
}

struct block_header * get_header(void * block_ptr) {
    if (getptrtag(block_ptr) != 0) die("get header of unaligned pointer");

    return (struct block_header *) (block_ptr - hdr_sz);
}

void print_block_header(struct block_header * header) {
    printf("0x%x: link=%x marked=%s layout=%s size=%dx%d=%d note=%.*s data@0x%x\n",
           header, header->link_ptr, bool_str(header->marked),
           layout_str(header->layout), header->size/sizeof(void *),
           sizeof(void *), header->size, 4, & header->note, header->data);
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
            int tag = (uint32_t) value & 0b11;
            switch (tag) {
            case 0b11:   // uint32_t
                printf(" = int:%u", untagint(value));
                break;
            default:   // pointer
                {
                    //TODO print cons and symbol properly
                    void * ptr_value = untagptr(value);
                    if (in_heap(ptr_value)) {
                        struct block_header * pheader = get_header(ptr_value);
                        printf(" -> layout=%s size=%d note=%.*s",
                               layout_str(pheader->layout), pheader->size,
                               4, & pheader->note);
                    }
                }
                break;
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

    //TODO check regs.link_data for a root
    if (regs.code_block) {
        last_header->link_ptr = regs.code_block;
        last_header->marked = true;
        //printf("root mark %x\n", last_header); //XXX
        last_header = last_header->link_ptr;
    }

    if (regs.arec_block) {
        last_header->link_ptr = regs.arec_block;
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
                    // don't follow null or integers or out of heap pointers
                    if (* candidate && istaggedptr(* candidate) && in_heap_in(heap, * candidate)) {
                        void * block = untagptr(* candidate);
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
                // forward any aligned pointers
                //printf("    @%x :", candidate); //XXX
                if (* candidate && istaggedptr(* candidate) && in_heap_in(heap, * candidate)) {
                    //printf(" %x ->", *candidate); //XXX
                    uint32_t tag = getptrtag(* candidate);
                    void * other_block = untagptr(* candidate);
                    other_block = get_header(other_block)->link_ptr + hdr_sz;
                    * candidate = tagptr(other_block, tag);
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
    if (regs.arec_block) {
        new_data_block = get_header(regs.arec_block)->link_ptr;
    }

    update_pointers_in(heap);
    compact_in(heap);

    heap->next = new_next;

    // record new root locations
    for (int ix=0 ; ix < NUM_ROOTS ; ix+=1) {
        if (heap->new_roots[ix]) heap->roots[ix] = heap->new_roots[ix];
    }

    if (regs.code_block) regs.code_block = new_code_block->data;
    if (regs.arec_block) regs.arec_block = new_data_block->data;
}

int round_to(int unit, int amount) {
    return ((amount - 1) / unit + 1) * unit;
}

//TODO needs to not die on error
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

    header->note = make_note("____");

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

int min(int a, int b) {
    return a < b ? a : b;
}

void print_hexdump(void * addr, int length) {
    for (void * end=addr+length ; addr < end ; addr+=16) {
        int howmany = min(end - addr, 16);

        printf("%08x:", addr);
        for (int n=0 ; n < howmany ; n+=1) {
            if (n%4 == 0) putchar(' ');
            printf("%02x ", ((uint8_t *) addr)[n]);
        }

        for (int n=0 ; n < 16-howmany ; n+=1) {
            if (n%4 == 0) putchar(' ');
            printf("   ");
        }

        putchar(' ');
        for (int n=0 ; n < howmany ; n+=1) {
            char c = ((char *) addr)[n];
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

void print_disassembly(void ** code, int size) {
    for (int ix=0 ; ix < size/sizeof(void *) ; ix+=1) {
        uint32_t instruction = (uint32_t) code[ix];

        enum opcodes op = instruction >> 24;
        unsigned int arg24 = instruction & 0xffffff;
        unsigned int arg8_1 = arg24 >> 16;
        unsigned int arg16 = arg24 & 0xffff;
        unsigned int arg8_2 = (arg24 >> 8) & 0xff;
        unsigned int arg8_3 = arg24 & 0xff;

        printf("%x : 0x%08x - %s ", code+ix, instruction, opcode_to_mnemonic(op));
        switch (arg_pattern(op)) {
        case 0:
            break;
        case 3:
            printf("0x%06x", arg24);
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

// end heap display

// virtual machine

jmp_buf abort_run;

void throw_run_error(char * message) {
    printf("%s\n", message);
    longjmp(abort_run, 1);
}

struct do_next do_abort = {abort_code};
struct do_next do_halt = {halt_code};

void unimpl() {
    die("unimplemented instruction");
}

void ** untagptrptr(void * ptr) {
    return (void **) untagptr(ptr);
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
                int size = untagint(regs.arec_block[arg8_2]);
                regs.arec_block[arg8_1] = allocate_noptr(size);
            }
            break;
        case ALLOCATE_NOPTR_imm16:
            regs.arec_block[arg8_1] = allocate_noptr(arg16);
            break;
        case ALLOCATE_ALLPTR:
            {
                int size = untagint(regs.arec_block[arg8_2]);
                regs.arec_block[arg8_1] = allocate_allptr(size);
            }
            break;
        case ALLOCATE_ALLPTR_imm16:
            regs.arec_block[arg8_1] = allocate_allptr(arg16);
            break;
        case GET_CBLK:
            regs.arec_block[arg8_1] = regs.code_block;
            break;
        case GET_INST:
            regs.arec_block[arg8_1] = tagint(regs.icount);
            break;
        case GET_AREC:
            regs.arec_block[arg8_1] = regs.arec_block;
            break;
        case GET_AREC_FAR:
            untagptrptr(regs.arec_block[arg8_1])[arg8_2] = regs.arec_block;
            break;
        case GET_LINK:
            regs.arec_block[arg8_1] = regs.link_data;
            break;
        case SET_LINK:
            regs.link_data = regs.arec_block[arg8_1];
            break;
        case SET_LINK_imm24:
            regs.link_data = tagint(arg24);
            break;
        case READ_FAR:
            regs.arec_block[arg8_1]
                = untagptrptr(regs.arec_block[arg8_2])[arg8_3];
            break;
        case WRITE_FAR:
            untagptrptr(regs.arec_block[arg8_1])[arg8_2]
                = regs.arec_block[arg8_3];
            break;
        case WRITE_FAR_imm8:
            untagptrptr(regs.arec_block[arg8_1])[arg8_2] = (void *) arg8_3;
            break;
        //TODO check for out-of-block jumps
        case JUMP_imm24:
            regs.icount = arg24;
            break;
        case JUMP_IF_imm16:
            if (untagint(regs.arec_block[arg8_1])) {
                regs.icount = arg16;
            }
            break;
        case JUMP_IF_raw_imm16:
            if ((uint32_t) regs.arec_block[arg8_1]) {
                regs.icount = arg16;
            }
            break;
        //TODO check for out-of-heap jumps
        case JUMP_FAR:
            //TODO check for jump to nonzerotag pointer
            regs.code_block = regs.arec_block[arg8_1];
            regs.icount = untagint(regs.arec_block[arg8_2]);
            regs.arec_block = regs.arec_block[arg8_3];
            end = get_header(regs.code_block)->size / sizeof(void *);
            break;
        case JUMP_FAR_imm8:
            //TODO check for jump to nonzerotag pointer
            regs.code_block = regs.arec_block[arg8_1];
            regs.icount = arg8_2;
            regs.arec_block = regs.arec_block[arg8_3];
            end = get_header(regs.code_block)->size / sizeof(void *);
            break;
        case JUMP_AREC:
            //TODO check for jump to nonzerotag pointer
            regs.arec_block[0] = regs.code_block;
            regs.arec_block[1] = tagint(regs.icount);

            regs.arec_block = regs.arec_block[arg8_1];
            regs.code_block = regs.arec_block[0];
            regs.icount = untagint(regs.arec_block[1]);

            end = get_header(regs.code_block)->size / sizeof(void *);
            break;
        case RESET_JUMP_AREC:
            //TODO check for jump to nonzerotag pointer
            regs.arec_block[0] = regs.code_block;
            regs.arec_block[1] = tagint(0);

            regs.arec_block = regs.arec_block[arg8_1];
            regs.code_block = regs.arec_block[0];
            regs.icount = untagint(regs.arec_block[1]);

            end = get_header(regs.code_block)->size / sizeof(void *);
            break;
        case CONST_imm16:
            regs.arec_block[arg8_1] = tagint(arg16);
            break;
        case CONST_FAR_imm8:
            untagptrptr(regs.arec_block[arg8_1])[arg8_2] = tagint(arg8_3);
            break;
        case CONST_imm16_raw:
            regs.arec_block[arg8_1] = (void *) arg16;
            break;
        case SET_16l:
            {
                int low16 = arg16 << 2;
                int high14 = (uint32_t) regs.arec_block[arg8_1] & 0xfffc0000;
                regs.arec_block[arg8_1] = (void *) (high14 | low16 | 0b11);
            }
            break;
        case SET_16l_raw:
            {
                int low16 = arg16 & 0xffff;
                int high16 = (uint32_t) regs.arec_block[arg8_1] & 0xffff0000;
                regs.arec_block[arg8_1] = (void *) (high16 | low16);
            }
            break;
        case SET_14h:
            {
                int low16 = (uint32_t) regs.arec_block[arg8_1] & 0xffff;
                int high14 = (arg16 & 0x3fff) << 18;
                regs.arec_block[arg8_1] = (void *) (high14 | low16);
            }
            break;
        case SET_16h_raw:
            {
                int low16 = (uint32_t) regs.arec_block[arg8_1] & 0xffff;
                int high16 = arg16 << 16;
                regs.arec_block[arg8_1] = (void *) (high16 | low16);
            }
            break;
        case ADD:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                uint32_t raw3 = untagint(regs.arec_block[arg8_3]);
                regs.arec_block[arg8_1] = tagint(raw2 + raw3);
            }
            break;
        case ADD_imm8:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                regs.arec_block[arg8_1] = tagint(raw2 + arg8_3);
            }
            break;
        case ADD_raw:
            {
                int raw2 = (int) regs.arec_block[arg8_2];
                int raw3 = (int) regs.arec_block[arg8_3];
                regs.arec_block[arg8_1] = (void *) (raw2 + raw3);
            }
            break;
        case MUL:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                uint32_t raw3 = untagint(regs.arec_block[arg8_3]);
                regs.arec_block[arg8_1] = tagint(raw2 * raw3);
            }
            break;
        case AND:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                uint32_t raw3 = untagint(regs.arec_block[arg8_3]);
                regs.arec_block[arg8_1] = tagint(raw2 & raw3);
            }
            break;
        case OR:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                uint32_t raw3 = untagint(regs.arec_block[arg8_3]);
                regs.arec_block[arg8_1] = tagint(raw2 | raw3);
            }
            break;
        case OR_raw:
            {
                int raw2 = (int) regs.arec_block[arg8_2];
                int raw3 = (int) regs.arec_block[arg8_3];
                regs.arec_block[arg8_1] = (void *) (raw2 | raw3);
            }
            break;
        case XOR:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                uint32_t raw3 = untagint(regs.arec_block[arg8_3]);
                regs.arec_block[arg8_1] = tagint(raw2 ^ raw3);
            }
            break;
        case LSHIFT_imm8:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                regs.arec_block[arg8_1] = tagint(raw2 << arg8_3);
            }
            break;
        case LSHIFT_imm8_raw:
            {
                int raw = (int) regs.arec_block[arg8_2];
                regs.arec_block[arg8_1] = (void *) (raw << arg8_3);
            }
            break;
        case RSHIFT_imm8:
            {
                uint32_t raw2 = untagint(regs.arec_block[arg8_2]);
                regs.arec_block[arg8_1] = tagint(raw2 >> arg8_3);
            }
            break;
        case DEBUG_WRITEHEX_raw:
            printf("0x%08x%c", regs.arec_block[arg8_1], arg8_3);
            break;
        case DEBUG_WRITEHEX_int:
            printf("0x%08x%c", untagint(regs.arec_block[arg8_1]), arg8_3);
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
        op1(GET_AREC, temp),
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

    regs.arec_block = allocate_allptr(10 * sizeof(void *));
    regs.code_block = allocate_noptr(sizeof(source));
    memmove(regs.code_block, source, sizeof(source));
    regs.icount = 0;
}

// end virtual machine

// save and load

void save_heap() {
    collect();

    FILE * f = fopen(SYSTEM_IMAGE_BIN, "wb");
    fwrite(heap0->memory, 1, heap0->next - heap0->memory, f);
    fclose(f);
}

void load_heap(struct heap ** heap_ptr, int size) {
    // calculate heap size
    FILE * f = fopen(SYSTEM_IMAGE_BIN, "rb");
    fseek(f, 0, SEEK_END);
    int length = ftell(f);

    int rounded_size = round_to(getpagesize(), size);
    if (length > rounded_size) rounded_size = round_to(getpagesize(), length);

    // allocate memory for heap
    void * memory = mmap(NULL, rounded_size, PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (! memory) die("can't create heap");

    * heap_ptr = (struct heap *) memory;
    struct heap * heap = * heap_ptr;

    // load heap from file
    rewind(f);
    fread(memory, 1, length, f);
    fclose(f);

    // fix up heap header
    void * old_start = heap->memory + sizeof(struct heap);
    void * old_next = heap->next;
    int delta = heap->memory - memory;
    heap->memory -= delta;
    heap->end = memory + rounded_size;
    heap->next -= delta;

    for (int ix=0 ; ix < NUM_ROOTS ; ix+=1) {
        if (heap->roots[ix]) {
            heap->roots[ix] = ((void *) heap->roots[ix]) - delta;
        }
    }

    // fix up heap pointers
    for (struct block_header * header=(struct block_header *) heap->start
         ; header < (struct block_header *) heap->next
         ; header = following_header(header)) {
        void * block = header->data;
        switch (header->layout) {
        case no_ptr_layout:
            break;
        case all_ptr_layout:
            // scan over pointers in block
            for (void ** candidate = (void **) block
                 ; candidate < (void **) (block + header->size)
                 ; candidate += 1) {
                // forward any non-zero aligned pointers
                if (* candidate && istaggedptr(* candidate)
                        && old_start <= * candidate && * candidate < old_next) {
                    * candidate -= delta;
                }
            }
            break;
        default:
            die("unhandled block layout");
        }
    }

    // allocate root block if necessary
    if (heap->roots[0] == NULL) {
        heap->roots[0] = get_header(
                allocate_in(heap, ROOT_BLOCK_SIZE, all_ptr_layout));
    }
}

// end save and load

void die(char * message) {
    printf("%s\n", message);
    exit(1);
}
