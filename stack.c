#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum opcodes {
    STOP=0,
    NOOP,
    DUP,
    OVER,
    PICK,
    SWAP,
    ROT,
    DROP,
    JUMP,
    CALL,
    RET,
    ZERO,
    ONE,
    NEGONE,
    PUSH,
    ADD,
    MUL,
    num_opcodes
};

enum opcodes source[] = {
    PUSH, 2,
    ONE,
    OVER,
    OVER,
    ADD,
    OVER,
    OVER,
    ADD,
    OVER,
    OVER,
    ADD,
    OVER,
    OVER,
    ADD,
    OVER,
    OVER,
    ADD,
    STOP,
};
int source_length = sizeof(source) / sizeof(source[0]);

enum {
    STACK_SIZE=1024*1024,
    PROGRAM_SIZE=1024,
};

uint32_t * data_stack_mem;
uint32_t * data_stack;
void ** call_stack_mem;
void ** call_stack;
void ** ip;
void ** program;

void * addr_of[num_opcodes];

enum action {
    EXECUTE=0,
    COMPILE,
};

void direct_threaded(enum action action) {
    if (action) {
        addr_of[STOP] = &&stop;
        addr_of[NOOP] = &&noop;
        addr_of[DUP] = &&dup;
        addr_of[OVER] = &&over;
        addr_of[PICK] = &&pick;
        addr_of[SWAP] = &&swap;
        addr_of[ROT] = &&rot;
        addr_of[DROP] = &&drop;
        addr_of[JUMP] = &&jump;
        addr_of[CALL] = &&call;
        addr_of[RET] = &&ret;
        addr_of[ZERO] = &&zero;
        addr_of[ONE] = &&one;
        addr_of[NEGONE] = &&negone;
        addr_of[PUSH] = &&push;
        addr_of[ADD] = &&add;
        addr_of[MUL] = &&mul;

        if (action == COMPILE) {
            for (int ix=0 ; ix < source_length ; ix += 1) {
                enum opcodes op = source[ix];
                program[ix] = addr_of[op];
                if (op == PUSH) {
                    ix += 1;
                    program[ix] = (void *) source[ix];
                }
            }
        }
        return;
    }

    uint32_t temp;

    ip = program;

noop:
    goto ** (ip++);
noop_end:

dup:
    data_stack[0] = data_stack[1];
    data_stack -= 1;
dup_end:
    goto ** (ip++);

over:
    data_stack[0] = data_stack[2];
    data_stack -= 1;
over_end:
    goto ** (ip++);

pick:
    temp = data_stack[2];
    data_stack[1] = data_stack[temp];
pick_end:
    goto ** (ip++);

swap:
    temp = data_stack[1];
    data_stack[1] = data_stack[2];
    data_stack[2] = data_stack[temp];
swap_end:
    goto ** (ip++);

rot:
    temp = data_stack[1];
    data_stack[1] = data_stack[2];
    data_stack[2] = data_stack[3];
    data_stack[3] = data_stack[temp];
rot_end:
    goto ** (ip++);

drop:
    data_stack += 1;
drop_end:
    goto ** (ip++);

jump:
    ip = (void **) data_stack[1];
    data_stack += 1;
    goto ** (ip++);
jump_end:

call:
    call_stack[0] = ip;
    call_stack -= 1;
    ip = (void **) data_stack[1];
    data_stack += 1;
    goto ** (ip++);
call_end:

ret:
    ip = call_stack[1];
    call_stack += 1;
    goto ** (ip++);
ret_end:

stop:
    return;
stop_end:

zero:
    data_stack[0] = 0;
    data_stack -= 1;
zero_end:
    goto ** (ip++);

one:
    data_stack[0] = 1;
    data_stack -= 1;
one_end:
    goto ** (ip++);

negone:
    data_stack[0] = -1;
    data_stack -= 1;
negone_end:
    goto ** (ip++);

push:
    data_stack[0] = (uint32_t) * ip++;
    data_stack -= 1;
push_end:
    goto ** (ip++);

add:
    data_stack[2] += data_stack[1];
    data_stack += 1;
add_end:
    goto ** (ip++);

mul:
    data_stack[2] *= data_stack[1];
    data_stack += 1;
mul_end:
    goto ** (ip++);
}

void print_stack() {
    uint32_t * end = data_stack_mem + STACK_SIZE;
    for (uint32_t * p = data_stack+1 ; p < end ; p += 1) {
        printf("%d ", * p);
    }
    printf("\n");
}

int main(int argc, char * argv[]) {
    data_stack_mem = calloc(STACK_SIZE, sizeof(* data_stack));
    if (! data_stack_mem) exit(1);
    data_stack = data_stack_mem + STACK_SIZE-1;

    call_stack_mem = calloc(STACK_SIZE, sizeof(* call_stack));
    if (! call_stack_mem) exit(2);
    call_stack = call_stack_mem + STACK_SIZE-1;

    program = calloc(PROGRAM_SIZE, sizeof(void *));
    if (! program) exit(3);

    direct_threaded(COMPILE);
    direct_threaded(EXECUTE);

    print_stack();
}
