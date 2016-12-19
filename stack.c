#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

uint32_t * data_stack;
uint32_t * call_stack;
uint32_t ip;
uint32_t * program;

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
    RETURN,
    STOP,
};

void run_opcode() {
    uint32_t temp;

    while (true) {
        uint32_t opcode = program[(int) ip];

        switch (opcode) {
            case DUP:
dup_start:
                data_stack[0] = data_stack[1];
                data_stack -= 1;
                ip += 1;
dup_end:
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
            case RETURN:
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

int main(int argc, char * argv[]) {
}
