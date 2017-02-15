#ifndef DEUX_H
#define DEUX_H

enum {
    HEAP_SIZE=16*1024*1024,     // 16 MB
    ROOT_BLOCK_SIZE=16,         // 16 items
};

static const int ALIGNMENT = 8;

enum which_root {
    SYMBOLS=0,
    LISP_ENV,
    NUM_ROOTS,
};

struct heap {
    void * memory;
    void * end;
    void * next;
    struct block_header * roots[NUM_ROOTS];
    struct block_header * new_roots[NUM_ROOTS];
    void * start[];
};

enum layout {
    no_ptr_layout= 0,
    all_ptr_layout=-1,
    cons_layout=-2,
};

struct block_header {
    void * link_ptr;
    bool marked;
    enum layout layout;
    int size;
    void * data[];
};

static const int hdr_sz = sizeof(struct block_header);

char * bool_str(bool val);

char * layout_str(enum layout val);

void * allocate_cons();

//TODO move registers into heap roots
struct registers {
    void ** code_block;
    unsigned int icount;
    void ** data_block;
};

void * allocate_symbol_intern_node();

struct symbol_intern_node {
    char * name;
    struct symbol_intern_node * child[256];
};

enum cons_tag {
    cons_tag = 0,
    sym_tag,
    char_tag,
    int_tag,
    obj_tag,
};

static bool is_ptr_tag(enum cons_tag tag) {
    return tag == cons_tag || tag == sym_tag || tag == obj_tag;
}

struct item {
    enum cons_tag tag;
    void * ptr;
};

struct maybe_item {
    bool present;
    struct item v;
};

struct cons {
    struct item head;
    struct item tail;
};

void print_cons_item(struct item item);

void print_tail_cons(struct cons * cell);

void die(char * message);

struct item lisp_eval(struct item exp, struct cons * env);

struct item lisp_apply(struct item sub, struct item args);

#endif // DEUX_H
