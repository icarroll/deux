#ifndef DEUX_H
#define DEUX_H

enum {
    HEAP_SIZE=16*1024*1024,             // 16 MB
    ROOT_BLOCK_SIZE=16*sizeof(void *),  // 16 items
};

static const int ALIGNMENT = 8;

enum which_root {
    ROOT_BLOCK,
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
};

struct block_header {
    void * link_ptr; //TODO change type to struct block_header *
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

void print_block(struct block_header * header);

bool heap_ok_in(struct heap * heap);

void print_heap_in(struct heap * heap);

void * tagint(uint32_t val);
uint32_t untag(void * val);

void collect();

void * allocate_allptr(int size);

//TODO move registers into heap roots
struct registers {
    void ** code_block;
    unsigned int icount;
    void ** data_block;
};

void die(char * message);

#endif // DEUX_H
