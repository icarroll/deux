#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

#include "deux.h"
#include "mnemonics.h"
#include "mt19937ar.h"

struct allocate_args {
    int size;
    enum layout layout;
};

struct test_action {
    enum {
        allocate_action,
        zero_ptr_action,
        copy_ptr_action,
        collect_action,
    } action_type;

    union {
        struct allocate_args a_args;
    };
};

uint32_t init[4];

int main(int argc, char * argv[]) {
    FILE * randoms = fopen("/dev/urandom", "rb");
    fread(& init, sizeof(init[0]), sizeof(init)/sizeof(init[0]), randoms);
    fclose(randoms);
    printf("random seed: %08x %08x %08x %08x\n",
           init[0], init[1], init[2], init[3]);
    init_by_array(init, sizeof(init)/sizeof(init[0]));
    uint32_t demo = genrand_int32();

    printf("ran %d tests\n", 0);
    printf("OK?\n");
}
