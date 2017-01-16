#include <errno.h>
#include <readline/readline.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#ifdef TRACE
#define TR(X) printf("%d: %x\n", __LINE__, X);
#else
#define TR(X)
#endif

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

enum {
    STACK_SIZE=1024*1024,
    PROGRAM_SIZE=1024,
};

uint32_t * data_stack_mem;
uint32_t * data_sp;
void ** call_stack_mem;
void ** call_sp;
register void ** ip asm("r11");
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
                calloc(PROGRAM_SIZE, sizeof(void *)),
                PROGRAM_SIZE
            };
            if (! new_action.to_execute) exit(3);

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
    TR(ip)
    goto ** (ip++);
next_end:

dup:
    data_sp[0] = data_sp[1];
    data_sp -= 1;
dup_end:
    TR(ip)
    goto ** (ip++);

over:
    data_sp[0] = data_sp[2];
    data_sp -= 1;
over_end:
    TR(ip)
    goto ** (ip++);

pick:
    temp = data_sp[2];
    data_sp[1] = data_sp[temp];
pick_end:
    TR(ip)
    goto ** (ip++);

swap:
    temp = data_sp[1];
    data_sp[1] = data_sp[2];
    data_sp[2] = data_sp[temp];
swap_end:
    TR(ip)
    goto ** (ip++);

rot:
    temp = data_sp[1];
    data_sp[1] = data_sp[2];
    data_sp[2] = data_sp[3];
    data_sp[3] = data_sp[temp];
rot_end:
    TR(ip)
    goto ** (ip++);

drop:
    data_sp += 1;
drop_end:
    TR(ip)
    goto ** (ip++);

skipif:
    temp = data_sp[1];
    data_sp += 1;
    if (temp) ip += 1;
    TR(ip)
    goto ** (ip++);
skipif_end:

fwd:
    temp = data_sp[1];
    data_sp += 1;
    ip += temp;
    TR(ip)
    goto ** (ip++);
fwd_end:

rew:
    temp = data_sp[1];
    data_sp += 1;
    ip -= temp;
    TR(ip)
    goto ** (ip++);
rew_end:

stop:
    return blank_action;
stop_end:

zero:
    data_sp[0] = 0;
    data_sp -= 1;
zero_end:
    TR(ip)
    goto ** (ip++);

one:
    data_sp[0] = 1;
    data_sp -= 1;
one_end:
    TR(ip)
    goto ** (ip++);

negone:
    data_sp[0] = -1;
    data_sp -= 1;
negone_end:
    TR(ip)
    goto ** (ip++);

lit:
    data_sp[0] = (uint32_t) * ip++;
    data_sp -= 1;
lit_end:
    TR(ip)
    goto ** (ip++);

add:
    data_sp[2] += data_sp[1];
    data_sp += 1;
add_end:
    TR(ip)
    goto ** (ip++);

mul:
    data_sp[2] *= data_sp[1];
    data_sp += 1;
mul_end:
    TR(ip)
    goto ** (ip++);

and:
    data_sp[2] &= data_sp[1];
    data_sp += 1;
and_end:
    TR(ip)
    goto ** (ip++);

or:
    data_sp[2] |= data_sp[1];
    data_sp += 1;
or_end:
    TR(ip)
    goto ** (ip++);

xor:
    data_sp[2] ^= data_sp[1];
    data_sp += 1;
xor_end:
    TR(ip)
    goto ** (ip++);

rshift:
    data_sp[2] >>= data_sp[1];
    data_sp += 1;
rshift_end:
    TR(ip)
    goto ** (ip++);

lshift:
    data_sp[2] <<= data_sp[1];
    data_sp += 1;
lshift_end:
    TR(ip)
    goto ** (ip++);

debug_writehex:
    printf("%x\n", data_sp[1]);
    data_sp += 1;
debug_writehex_end:
    TR(ip)
    goto ** (ip++);

debug_show_stack:
    print_stack();
debug_show_stack_end:
    TR(ip)
    goto ** (ip++);
}

void run() {
    data_stack_mem = calloc(STACK_SIZE, sizeof(* data_sp));
    if (! data_stack_mem) exit(1);
    data_sp = data_stack_mem + STACK_SIZE-1;

    call_stack_mem = calloc(STACK_SIZE, sizeof(* call_sp));
    if (! call_stack_mem) exit(2);
    call_sp = call_stack_mem + STACK_SIZE-1;

    program = calloc(PROGRAM_SIZE, sizeof(void *));
    if (! program) exit(3);

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

struct astree {
};

void free_action(struct action act) {
    switch (act.action) {
        case EXECUTE:
            free(act.to_execute);
            break;
        case ASSEMBLE:
            free(act.to_assemble);
            break;
        default:
            break;
    }
}

struct astree * parse(char * line) {
    void * item;
    void * next;
}

void free_astree(struct astree * tree) {
}

struct action compile(struct astree * tree) {
}

void rep() {
    data_stack_mem = calloc(STACK_SIZE, sizeof(* data_sp));
    if (! data_stack_mem) exit(1);
    data_sp = data_stack_mem + STACK_SIZE-1;

    call_stack_mem = calloc(STACK_SIZE, sizeof(* call_sp));
    if (! call_stack_mem) exit(2);
    call_sp = call_stack_mem + STACK_SIZE-1;

    char * line = readline("deux> ");
    struct astree * tree = parse(line);
    //struct action assemble_opcodes = compile(tree);
    //struct action execute_program = direct_threaded(assemble_opcodes);
    //direct_threaded(execute_program);

    //free_action(assemble_opcodes);
    //free_action(execute_program);
    free_astree(tree);
    free(line);
    free(call_stack_mem);
    free(data_stack_mem);
}

int main(int argc, char * argv[]) {
    rep();
    print_stack();
}
