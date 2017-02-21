#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "deux.h"
#include "mnemonics.h"
#include "mt19937ar.h"

extern struct heap * heap0;

int randint(int maxone) {
    return genrand_real2() * maxone;
}

struct allocate_args {
    int dst_ptr_choice;
    int size;
    enum layout layout;
};

struct zero_ptr_args {
    int dst_ptr_choice;
};

struct copy_ptr_args {
    int dst_ptr_choice;
    int srcptr_choice;
};

struct add_number_args {
    int dst_ptr_choice;
    void * number;
};

struct test_action {
    enum {
        allocate_action,
        zero_ptr_action,
        copy_ptr_action,
        add_number_action,
        collect_action,
        NUM_ACTION_TYPES
    } which;

    union {
        struct allocate_args a_args;
        struct zero_ptr_args zp_args;
        struct copy_ptr_args cp_args;
        struct add_number_args an_args;
    };
};

struct test_action_seq {
    int count;
    struct test_action * actions;
};

struct block_header * choose_block() {
    float blocks_considered = 0;
    struct block_header * choice = NULL;
    for (struct block_header * candidate=(struct block_header *) heap0->start
         ; candidate < (struct block_header *) heap0->next
         ; candidate = following_header(candidate)) {
        blocks_considered += 1;
        if (genrand_real2() < 1. / blocks_considered) choice = candidate;
    }

    return choice;
}

void ** choose_pointer() {
    float pointers_considered = 0;
    void ** choice = NULL;
    for (struct block_header * header=(struct block_header *) heap0->start
         ; header < (struct block_header *) heap0->next
         ; header = following_header(header)) {
        float num_pointers = header->size / sizeof(void *);
        pointers_considered += num_pointers;
        if (genrand_real2() < num_pointers / pointers_considered) {
            int ix = genrand_real2() * num_pointers;
            choice = & header->data[ix];
        }
    }

    return choice;
}

struct test_action create_random_action() {
    struct test_action action;

    action.which = genrand_real2() * NUM_ACTION_TYPES;
    switch (action.which) {
    case allocate_action:
        action.a_args.dst_ptr_choice = 0; //TODO
        action.a_args.size = 0; //TODO
        action.a_args.layout = layouts[randint(NUM_LAYOUTS)];
        break;
    case zero_ptr_action:
        break;
    case copy_ptr_action:
        break;
    case add_number_action:
        break;
    case collect_action:
        break;
    default:
        die("invalid action type");
    }

    return action;
}

struct test_action_seq create_test_action_seq(int action_count) {
    //TODO
}

struct test_action_seq shrink_test_action_seq(struct test_action_seq seq) {
    //TODO
}

void execute_action(struct test_action action) {
    //TODO
    switch (action.which) {
    case allocate_action:
        //TODO
        //allocate(WHUT);
        break;
    case zero_ptr_action:
        //TODO
        break;
    case copy_ptr_action:
        //TODO
        break;
    case add_number_action:
        //TODO
        break;
    case collect_action:
        collect();
        break;
    default:
        die("invalid action type");
    }
}

void execute_action_sequence(struct test_action_seq seq) {
    for (int ix=0 ; ix < seq.count ; ix += 1) execute_action(seq.actions[ix]);
}

void print_action(struct test_action action) {
    //TODO
    switch (action.which) {
    case allocate_action:
        {
            struct allocate_args a = action.a_args;
            printf("allocate %d %d %s\n",
                   a.dst_ptr_choice, a.size, layout_str(a.layout));
        }
        break;
    case zero_ptr_action:
        //TODO
        break;
    case copy_ptr_action:
        //TODO
        break;
    case add_number_action:
        //TODO
        break;
    case collect_action:
        printf("collect\n");
        break;
    default:
        die("invalid action type");
    }
}

void print_action_sequence(struct test_action_seq seq) {
    for (int ix=0 ; ix < seq.count ; ix += 1) print_action(seq.actions[ix]);
}

struct result {
    int tests_run;
    struct test_action_seq * culprit;
};

struct result run_some_tests(int max_count) {
    for (int ix=0 ; ix < 0 ; ix += 1) { //XXX
        make_heap(& heap0, HEAP_SIZE);
        //TODO
    }

    return (struct result) {0, NULL};
}

int main(int argc, char * argv[]) {
    FILE * randoms = fopen("/dev/urandom", "rb");
    uint32_t init[4];
    fread(& init, sizeof(init[0]), sizeof(init)/sizeof(init[0]), randoms);
    fclose(randoms);

    printf("random seed:");
    for (int ix=0 ; ix < sizeof(init)/sizeof(init[0]) ; ix+=1) {
        printf(" %08x", init[ix]);
    }
    printf("\n\n");

    init_by_array((long unsigned int *) init, sizeof(init)/sizeof(init[0]));
    uint32_t demo = genrand_int32();

    struct result result = run_some_tests(1000);

    printf("ran %d test%s\n", result.tests_run, result.tests_run == 1 ? "" : "s");
    if (result.culprit == NULL) printf("OK\n");
    else {
        printf("failing instance:\n");
        print_action_sequence(* result.culprit);
        free(result.culprit);
    }
}
