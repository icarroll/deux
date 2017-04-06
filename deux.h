#ifndef DEUX_H
#define DEUX_H

static const char SYSTEM_IMAGE_BIN[] = "system_image.bin";

enum {
    HEAP_SIZE=16*1024*1024,             // 16 MB
    ROOT_BLOCK_SIZE=4*sizeof(void *),  // 4 items
};

static const int ALIGNMENT = sizeof(void *);

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

struct block_header * get_header(void * block_ptr);

struct block_header * following_header(struct block_header * header);

bool heap_ok();

void print_hexdump(void * addr, int length);

void print_block(struct block_header * header);

bool heap_ok_in(struct heap * heap);

void print_heap_in(struct heap * heap);

bool istaggedint(void * ptr);
bool istaggedblock(void * ptr);
bool istaggedsym(void * ptr);
bool istaggedcons(void * ptr);
void * tagblockptr(void * ptr);
void * tagsymptr(void * ptr);
void * tagconsptr(void * ptr);
void * untagptr(void * ptr);
uint32_t getptrtag(void * ptr);
void * tagptr(void * ptr, uint32_t tag);

void * tagint(uint32_t val);
uint32_t untagint(void * val);

void collect();

uint32_t make_note(char * c);
enum {
    CONS_NOTE = 'c' | 'o' << 8 | 'n' << 16 | 's' << 24,
    SYMB_NOTE = 's' | 'y' << 8 | 'm' << 16 | 'b' << 24,
};

void * allocate_noptr(int size);
void * allocate_allptr(int size);

//TODO move registers into heap roots
struct registers {
    void ** code_block;
    unsigned int icount;
    void ** arec_block;
    void * link_ptr;
    void * link_data;
};

struct do_next {
    enum {
        abort_code,
        halt_code,
        jump_c,
    } action;
    struct registers regs;
};

struct do_next run();

void die(char * message);

void print_heap();

void print_disassembly(void ** code, int size);

void save_heap();
void load_heap(struct heap ** heap_ptr, int size);

#endif // DEUX_H
