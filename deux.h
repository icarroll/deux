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

void make_heap(struct heap ** heap_ptr, int size);

enum layout {
    no_ptr_layout= 0,
    all_ptr_layout=-1,
    cons_layout=-2,
};

static const enum layout layouts[] = {
    no_ptr_layout,
    all_ptr_layout,
    cons_layout,
};

static const int NUM_LAYOUTS = sizeof(layouts) / sizeof(layouts[0]);

struct block_header {
    void * link_ptr;
    bool marked;
    enum layout layout;
    int size;
    uint32_t note;
    void * data[];
};

static const int hdr_sz = sizeof(struct block_header);

char * bool_str(bool val);

char * layout_str(enum layout val);

bool in_heap(void * addr);

struct block_header * following_header(struct block_header * header);

bool heap_ok();

void print_hexdump(void * addr, int length);

bool heap_ok_in(struct heap * heap);

void print_heap_in(struct heap * heap);

void collect();

void * allocate_allptr(int size);

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

struct item lisp_eval_rec(struct item exp, struct cons * env);

struct item lisp_apply_rec(struct item sub, struct item args);

#endif // DEUX_H
