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

#include "stack.h"

// memory management

struct block_header * get_header(void * block_ptr) {
    return (struct block_header *) (block_ptr - hdr_sz);
}

void mark_in(struct heap * heap) {
    struct block_header * next_header = get_header(heap->root_block);
    struct block_header * last_header = next_header;

    next_header->link_ptr = NULL;
    next_header->marked = true;

    while (next_header) {
        struct block_header * cur_header = next_header;

        switch (cur_header->layout) {
        case no_ptr_layout:
            break;
        case all_ptr_layout:
            for (void ** candidate = cur_header->data
                 ; candidate < (void **) (cur_header->data + cur_header->size)
                 ; candidate += 1) {
                if (* candidate) {
                    struct block_header * header = get_header(* candidate);
                    if (! header->marked) {
                        header->link_ptr = NULL;
                        header->marked = true;
                        last_header->link_ptr = header;
                        last_header = header;
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

void OLD_mark_in(struct heap * heap) {
    void ** block = heap->root_block;
    get_header(block)->marked = true;

    // trace block graph
    while (block) {
        switch (get_header(block)->layout) {
        case no_ptr_layout:
            break;
        case all_ptr_layout:
            // follow non-zero pointers in block
            for (void ** candidate = (void **) block
                 ; candidate < (void **) (block + get_header(block)->size)
                 ; candidate += 1) {
                if (* candidate) {
                    struct block_header * header = get_header(* candidate);
                    if (! header->marked) {
                        header->marked = true;
                        // add block to list
                        header->link_ptr = get_header(block)->link_ptr;
                        get_header(block)->link_ptr = * candidate;
                    }
                }
            }
            break;
        case cons_layout:
            {
                // in head and tail, follow non-zero pointers
                struct cons * cell = (struct cons *) block;
                if (is_ptr_tag(cell->head.tag) && cell->head.ptr) {
                    struct block_header * header = get_header(cell->head.ptr);
                    if (! header->marked) {
                        header->marked = true;
                        // add block to list
                        header->link_ptr = get_header(block)->link_ptr;
                        get_header(block)->link_ptr = cell->head.ptr;
                    }
                }
                if (is_ptr_tag(cell->tail.tag) && cell->tail.ptr) {
                    struct block_header * header = get_header(cell->tail.ptr);
                    if (! header->marked) {
                        header->marked = true;
                        // add block to list
                        header->link_ptr = get_header(block)->link_ptr;
                        get_header(block)->link_ptr = cell->tail.ptr;
                    }
                }
            }
            break;
        default:
            die("unhandled block layout");
        }

        block = get_header(block)->link_ptr;
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
                // forward any non-zero pointers
                if (* candidate) {
                    * candidate = get_header(* candidate)->link_ptr;
                }
            }
            break;
        case cons_layout:
            {
                // in head and tail, forward non-zero pointers
                struct cons * cell = (struct cons *) block;
                if (is_ptr_tag(cell->head.tag) && cell->head.ptr) {
                    cell->head.ptr = get_header(cell->head.ptr)->link_ptr;
                }
                if (is_ptr_tag(cell->tail.tag) && cell->tail.ptr) {
                    cell->tail.ptr = get_header(cell->tail.ptr)->link_ptr;
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
            dest_header = (struct block_header *) (header->link_ptr - hdr_sz);
            memmove(dest_header, header, hdr_sz + header->size);

            dest_header->link_ptr = NULL;
            dest_header->marked = false;
        }
    }
}

//TODO
// display of heap blocks
// check heap for errors
// test allocation and collection

void collect_in(struct heap * heap) {
    mark_in(heap);
    void * new_next = compute_forward_addrs_in(heap);
    void ** new_root = get_header(heap->root_block)->link_ptr;
    update_pointers_in(heap);
    compact_in(heap);

    heap->next = new_next;
    heap->root_block = new_root;
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
    header->link_ptr = 0xdeadbeef;
    header->marked = false;
    header->layout = layout;
    header->size = size;

    int data_size = entire_size - hdr_sz;
    memset(header->data, 0, data_size);

    heap->next = new_next;
    printf("allocating block %x size=%d\n", header->data, data_size);
    return header->data;
}

void add_root_in(struct heap * heap, void * new_root_ptr) {
    void ** root_block = heap->root_block;

    void ** block_end = root_block + get_header(root_block)->size;

    void ** candidate;
    for (candidate = root_block ; candidate < block_end ; candidate += 1) {
        if (* candidate == NULL) {
            * candidate = new_root_ptr;
            return;
        }
    }

    void ** new_block = allocate_in(heap, ROOT_BLOCK_SIZE, all_ptr_layout);
    if (! new_block) die("can't allocate root block");

    new_block[0] = root_block;
    new_block[1] = new_root_ptr;
    heap->root_block = new_block;
}

void make_heap(struct heap * heap, int size) {
    int rounded_size = round_to(getpagesize(), size);

    void * memory = mmap(NULL, rounded_size, PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (! memory) die("can't create heap");

    heap->memory = memory;
    heap->end = memory + rounded_size;
    heap->next = memory;

    heap->root_block = allocate_in(heap, ROOT_BLOCK_SIZE * sizeof(void *), all_ptr_layout);
    if (! heap->root_block) die("can't allocate root block");
}

struct heap heap;

void collect() {
    collect_in(& heap);
}

void * allocate(int size, enum layout layout) {
    return allocate_in(& heap, size, layout);
}

void * allocate_noptr(int size) {
    return allocate(size, no_ptr_layout);
}

void * allocate_allptr(int size) {
    return allocate(size, all_ptr_layout);
}

void init_heap() {
    make_heap(& heap, HEAP_SIZE);
}

void add_root(void * new_root) {
    add_root_in(& heap, new_root);
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
    default:
        return "unhandled";
    }
}

void print_heap_in(struct heap * heap) {
    printf("memory=%x end=%x next=%x root_block=%x\n",
           heap->memory, heap->end, heap->next, heap->root_block);
    for (void * block=heap->memory + hdr_sz
         ; block < heap->next
         ; block = following_block(block)) {
        struct block_header * header = get_header(block);
        printf("%x: link=%x marked=%s layout=%s size=%d\n",
               block, header->link_ptr, bool_str(header->marked),
               layout_str(header->layout), header->size);

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
    print_heap_in(& heap);
}

// end heap display

enum opcodes {
    STOP=0,
    NEXT,
    DUP,
    OVER,
    PICK,
    SWAP,
    ROT,
    DROP,
    SKIPIF,
    FWD,
    REW,
    ZERO,
    ONE,
    NEGONE,
    LIT,
    ADD,
    MUL,
    AND,
    OR,
    XOR,
    RSHIFT,
    LSHIFT,
    DEBUG_WRITEHEX,
    DEBUG_SHOW_STACK,
    num_opcodes
};

uint32_t * data_stack_mem;
uint32_t * data_sp;
void ** call_stack_mem;
void ** call_sp;
//register void ** ip asm("r11"); //TODO does this work?
void ** ip;
void ** program;

void * addr_of[num_opcodes];
void * end_of[num_opcodes];

enum action_type {
    EXECUTE=0,
    ASSEMBLE,
    EXPERIMENT,
};

struct action {
    enum action_type action;
    union {
        enum opcodes * to_assemble;
        void * to_execute;
    };
    int length;
} blank_action;

void print_stack() {
    uint32_t * end = data_stack_mem + STACK_SIZE;
    for (uint32_t * p = data_sp+1 ; p < end ; p += 1) {
        printf("%d ", * p);
    }
    printf("\n");
}

struct action direct_threaded(struct action action) {
    if (action.action) {
        addr_of[STOP] = &&stop;
        addr_of[NEXT] = &&next;
        addr_of[DUP] = &&dup;
        addr_of[OVER] = &&over;
        addr_of[PICK] = &&pick;
        addr_of[SWAP] = &&swap;
        addr_of[ROT] = &&rot;
        addr_of[DROP] = &&drop;
        addr_of[SKIPIF] = &&skipif;
        addr_of[FWD] = &&fwd;
        addr_of[REW] = &&rew;
        addr_of[ZERO] = &&zero;
        addr_of[ONE] = &&one;
        addr_of[NEGONE] = &&negone;
        addr_of[LIT] = &&lit;
        addr_of[ADD] = &&add;
        addr_of[MUL] = &&mul;
        addr_of[AND] = &&and;
        addr_of[OR] = &&or;
        addr_of[XOR] = &&xor;
        addr_of[RSHIFT] = &&rshift;
        addr_of[LSHIFT] = &&lshift;
        addr_of[DEBUG_WRITEHEX] = &&debug_writehex;
        addr_of[DEBUG_SHOW_STACK] = &&debug_show_stack;

        end_of[STOP] = &&stop_end;
        end_of[NEXT] = &&next_end;
        end_of[DUP] = &&dup_end;
        end_of[OVER] = &&over_end;
        end_of[PICK] = &&pick_end;
        end_of[SWAP] = &&swap_end;
        end_of[ROT] = &&rot_end;
        end_of[DROP] = &&drop_end;
        end_of[SKIPIF] = &&skipif_end;
        end_of[FWD] = &&fwd_end;
        end_of[REW] = &&rew_end;
        end_of[ZERO] = &&zero_end;
        end_of[ONE] = &&one_end;
        end_of[NEGONE] = &&negone_end;
        end_of[LIT] = &&lit_end;
        end_of[ADD] = &&add_end;
        end_of[MUL] = &&mul_end;
        end_of[AND] = &&and_end;
        end_of[OR] = &&or_end;
        end_of[XOR] = &&xor_end;
        end_of[RSHIFT] = &&rshift_end;
        end_of[LSHIFT] = &&lshift_end;
        end_of[DEBUG_WRITEHEX] = &&debug_writehex_end;
        end_of[DEBUG_SHOW_STACK] = &&debug_show_stack_end;

        if (action.action == ASSEMBLE) {
            struct action new_action = {
                EXECUTE,
                allocate_noptr(PROGRAM_SIZE),
                PROGRAM_SIZE
            };
            if (! new_action.to_execute) die("can't allocate program");

            for (int ix=0 ; ix < action.length ; ix += 1) {
                enum opcodes op = action.to_assemble[ix];
                program[ix] = addr_of[op];
                if (op == LIT) {
                    ix += 1;
                    program[ix] = (void *) action.to_assemble[ix];
                }
            }

            return new_action;
        }
        else if (action.action == EXPERIMENT) {
            void * subprogram = mmap(NULL, getpagesize(),
                                     PROT_READ | PROT_WRITE,
                                     MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
            void * at = subprogram;

            ptrdiff_t one_length = &&one_end - &&one;
            memmove(at, &&one, one_length); at += one_length;
            memmove(at, &&one, one_length); at += one_length;
            memmove(at, &&one, one_length); at += one_length;
            memmove(at, &&one, one_length); at += one_length;
            ptrdiff_t next_length = &&next_end - &&next;
            memmove(at, &&next, next_length); at += next_length;

            program[0] = subprogram;
            program[1] = &&add;
            program[2] = &&add;
            program[3] = &&add;
            program[4] = &&debug_writehex;
            program[5] = &&stop;

            mprotect(subprogram, getpagesize(), PROT_READ | PROT_EXEC);

            return blank_action;
        }
    }

    uint32_t temp;

    ip = program;

next:
    goto ** (ip++);
next_end:

dup:
    data_sp[0] = data_sp[1];
    data_sp -= 1;
dup_end:
    goto ** (ip++);

over:
    data_sp[0] = data_sp[2];
    data_sp -= 1;
over_end:
    goto ** (ip++);

pick:
    temp = data_sp[2];
    data_sp[1] = data_sp[temp];
pick_end:
    goto ** (ip++);

swap:
    temp = data_sp[1];
    data_sp[1] = data_sp[2];
    data_sp[2] = data_sp[temp];
swap_end:
    goto ** (ip++);

rot:
    temp = data_sp[1];
    data_sp[1] = data_sp[2];
    data_sp[2] = data_sp[3];
    data_sp[3] = data_sp[temp];
rot_end:
    goto ** (ip++);

drop:
    data_sp += 1;
drop_end:
    goto ** (ip++);

skipif:
    temp = data_sp[1];
    data_sp += 1;
    if (temp) ip += 1;
    goto ** (ip++);
skipif_end:

fwd:
    temp = data_sp[1];
    data_sp += 1;
    ip += temp;
    goto ** (ip++);
fwd_end:

rew:
    temp = data_sp[1];
    data_sp += 1;
    ip -= temp;
    goto ** (ip++);
rew_end:

stop:
    return blank_action;
stop_end:

zero:
    data_sp[0] = 0;
    data_sp -= 1;
zero_end:
    goto ** (ip++);

one:
    data_sp[0] = 1;
    data_sp -= 1;
one_end:
    goto ** (ip++);

negone:
    data_sp[0] = -1;
    data_sp -= 1;
negone_end:
    goto ** (ip++);

lit:
    data_sp[0] = (uint32_t) * ip++;
    data_sp -= 1;
lit_end:
    goto ** (ip++);

add:
    data_sp[2] += data_sp[1];
    data_sp += 1;
add_end:
    goto ** (ip++);

mul:
    data_sp[2] *= data_sp[1];
    data_sp += 1;
mul_end:
    goto ** (ip++);

and:
    data_sp[2] &= data_sp[1];
    data_sp += 1;
and_end:
    goto ** (ip++);

or:
    data_sp[2] |= data_sp[1];
    data_sp += 1;
or_end:
    goto ** (ip++);

xor:
    data_sp[2] ^= data_sp[1];
    data_sp += 1;
xor_end:
    goto ** (ip++);

rshift:
    data_sp[2] >>= data_sp[1];
    data_sp += 1;
rshift_end:
    goto ** (ip++);

lshift:
    data_sp[2] <<= data_sp[1];
    data_sp += 1;
lshift_end:
    goto ** (ip++);

debug_writehex:
    printf("%x\n", data_sp[1]);
    data_sp += 1;
debug_writehex_end:
    goto ** (ip++);

debug_show_stack:
    print_stack();
debug_show_stack_end:
    goto ** (ip++);
}

void run() {
    data_stack_mem = allocate_noptr(STACK_SIZE * sizeof(* data_sp));
    if (! data_stack_mem) die("can't allocate data stack");
    data_sp = data_stack_mem + STACK_SIZE-1;

    call_stack_mem = allocate_noptr(STACK_SIZE * sizeof(* call_sp));
    if (! call_stack_mem) die("can't allocate call stack");
    call_sp = call_stack_mem + STACK_SIZE-1;

    program = allocate_noptr(PROGRAM_SIZE);
    if (! program) die("can't allocate program");

    enum opcodes source[] = {
        LIT, 5,
        LIT, 15,
        LIT, 47,
        DEBUG_SHOW_STACK,
        STOP,
    };
    int source_length = sizeof(source) / sizeof(source[0]);

    //direct_threaded(EXPERIMENT);
    //direct_threaded(ASSEMBLE);
    //direct_threaded(EXECUTE);
}

// begin symbol trie

void * allocate_symbol_intern_node() {
    void * temp = allocate_allptr(sizeof(struct symbol_intern_node));
    if (! temp) die("can't allocate symbol node");
    return temp;
}

struct symbol_intern_node * symbols;

void * get_symbol_ref_n(char * name, int length) {
    if (! symbols) {
        symbols = allocate_symbol_intern_node();
        add_root(symbols);
    }
    struct symbol_intern_node * node = symbols;

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

    return (void *) node;
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
            fprintf(stdout, "%s",
                    ((struct symbol_intern_node *) item.ptr)->name);
            break;
        case cons_tag:
            fputc('(', stdout);
            if (cell) print_tail_cons(cell);
            fputc(')', stdout);
            break;
    }
}

void print_tail_cons(struct cons * cell) {
    print_cons_item(cell->head);

    switch (cell->tail.tag) {
        case char_tag:
        case int_tag:
        case sym_tag:
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

// begin parse

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

struct action compile(struct cons * tree) {
//TODO
}

void repl() {
    data_stack_mem = allocate_noptr(STACK_SIZE * sizeof(* data_sp));
    if (! data_stack_mem) die("can't allocate data stack");
    data_sp = data_stack_mem + STACK_SIZE-1;

    call_stack_mem = allocate_noptr(STACK_SIZE * sizeof(* call_sp));
    if (! call_stack_mem) die("can't allocate call stack");
    call_sp = call_stack_mem + STACK_SIZE-1;

    using_history();

    char * line;
    while (line = readline("deux> ")) {
        if (* line) add_history(line);

        struct maybe_item maybe_tree = parse(line);
        if (maybe_tree.present) {
            print_cons_item(maybe_tree.v); putchar('\n');
        }

        //struct action assemble_opcodes = compile(tree);
        //struct action execute_program = direct_threaded(assemble_opcodes);
        //direct_threaded(execute_program);

        free(line);

        print_heap();
    }
    putchar('\n');
}

int main(int argc, char * argv[]) {
    init_heap();
    repl();
    collect();
    print_heap();
}
