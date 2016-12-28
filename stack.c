#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

enum {STACK_SIZE=1024*1024};

uint32_t * data_stack;
uint32_t * call_stack;
uint32_t ip;
uint32_t * program;

int * source;
int source_length;

enum opcodes {
    NOOP=0,
    DUP,
    OVER,
    PICK,
    SWAP,
    ROT,
    DROP,
    JUMP,
    CALL,
    RET,
    STOP,
};

void run_opcode() {
    uint32_t temp;

    while (true) {
        uint32_t opcode = program[(int) ip];

        switch (opcode) {
            case DUP:
                data_stack[0] = data_stack[1];
                data_stack -= 1;
                ip += 1;
                break;
            case OVER:
                data_stack[0] = data_stack[2];
                data_stack -= 1;
                ip += 1;
                break;
            case PICK:
                temp = data_stack[2];
                data_stack[1] = data_stack[temp];
                ip += 1;
                break;
            case SWAP:
                temp = data_stack[1];
                data_stack[1] = data_stack[2];
                data_stack[2] = data_stack[temp];
                ip += 1;
                break;
            case ROT:
                temp = data_stack[1];
                data_stack[1] = data_stack[2];
                data_stack[2] = data_stack[3];
                data_stack[3] = data_stack[temp];
                ip += 1;
                break;
            case DROP:
                data_stack += 1;
                ip += 1;
                break;
            case JUMP:
                ip = data_stack[1];
                data_stack += 1;
                break;
            case CALL:
                call_stack[0] = (uint32_t) ip;
                call_stack -= 1;
                ip = data_stack[1];
                data_stack += 1;
                break;
            case RET:
                ip = call_stack[1];
                call_stack += 1;
                break;
            case STOP:
                return;
                break;
            default:
                ip += 1;
                break;
        }
    }
}

void indirect_threaded(bool compile) {
    if (compile) {
        void * which[100];

        which[NOOP] = &&noop;
        which[DUP] = &&dup;
        which[OVER] = &&over;
        which[PICK] = &&pick;
        which[SWAP] = &&swap;
        which[ROT] = &&rot;
        which[DROP] = &&drop;
        which[JUMP] = &&jump;
        which[CALL] = &&call;
        which[RET] = &&ret;
        which[STOP] = &&stop;

        for (int ix=0 ; ix < source_length ; ix += 1) {
            program[ix] = (uint32_t) which[source[ix]];
        }

        return;
    }

    uint32_t temp;

noop:
    goto * (ip++);
noop_end:

dup:
    data_stack[0] = data_stack[1];
    data_stack -= 1;
dup_end:
    goto * (ip++);

over:
    data_stack[0] = data_stack[2];
    data_stack -= 1;
over_end:
    goto * (ip++);

pick:
    temp = data_stack[2];
    data_stack[1] = data_stack[temp];
pick_end:
    goto * (ip++);

swap:
    temp = data_stack[1];
    data_stack[1] = data_stack[2];
    data_stack[2] = data_stack[temp];
swap_end:
    goto * (ip++);

rot:
    temp = data_stack[1];
    data_stack[1] = data_stack[2];
    data_stack[2] = data_stack[3];
    data_stack[3] = data_stack[temp];
rot_end:
    goto * (ip++);

drop:
    data_stack += 1;
drop_end:
    goto * (ip++);

jump:
    ip = data_stack[1];
    data_stack += 1;
jump_end:

call:
    call_stack[0] = (uint32_t) ip;
    call_stack -= 1;
    ip = data_stack[1];
    data_stack += 1;
call_end:

ret:
    ip = call_stack[1];
    call_stack += 1;
ret_end:

stop:
    return;
stop_end:

    0;   // to quiet warning about label at end of block
}

int main(int argc, char * argv[]) {
    if (! (data_stack = malloc(STACK_SIZE))) exit(1);
    if (! (call_stack = malloc(STACK_SIZE))) exit(2);
}
