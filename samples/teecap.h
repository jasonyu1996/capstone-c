#ifndef TEECAP_H
#define TEECAP_H

#define TEECAP_SEALED_OFFSET_PC 0
#define TEECAP_SEALED_OFFSET_SC 1
#define TEECAP_SEALED_OFFSET_EPC 2
#define TEECAP_SEALED_OFFSET_RET 3
#define TEECAP_SEALED_OFFSET_STACK 4
#define TEECAP_SEALED_OFFSET_METAPARAM 5

#define TEECAP_GPR_N 32
#define TEECAP_SEALED_REGION_SIZE 36
#define TEECAP_METAPARAM (reg("r1"))

#define TEECAP_ATTR_HAS_METAPARAM __attribute__((pinned("r1")))

#define TEECAP_ALLOC_BOTTOM(v, tmp, mem, size) \
    (v) = splitlo((mem), (size));\
    (tmp) = (v);\
    (v) = (mem); \
    (mem) = (tmp);
#define TEECAP_BUILD_CP(v, target) \
    (v) = reg("pc");\
    scc((v), (target));
#define TEECAP_MREV_DELIN(v, m) \
    (m) = mrev((v));\
    delin((v));


struct teecap_runtime {
    int version_major;
    int version_minor;
    void* malloc; // a sealed capability for invoking the memory allocator
    void* free;
    void* new_thread;
};

void* sealed_setup(int* cap, void* pcc, void* epc, void* stack, int* metaparam) {
    cap[TEECAP_SEALED_OFFSET_PC] = pcc;
    cap[TEECAP_SEALED_OFFSET_EPC] = epc;
    cap[TEECAP_SEALED_OFFSET_STACK] = stack; // stack is actually specified upon call
    cap[TEECAP_SEALED_OFFSET_METAPARAM] = metaparam;
    seal(cap);
    return cap;
}


struct mem_region {
    struct mem_region *left, *right;
    int size, leaf, free;
    void* mem;
};

struct malloc_state {
    struct mem_region* heap;
    int alloc_n;
};

void* malloc_find(struct mem_region* region, int size) {
    if(region->size < size || (region->leaf && !region->free))
        return 0;
    if(region->leaf) {
        //print(region);
        void* mem = region->mem;
        region->mem = mrev(mem);
        if(region->size <= (size + sizeof(struct mem_region)) * 2){
            region->free = 0;
            return mem;
        }
        // split

        int sz_left = region->size / 2 - sizeof(struct mem_region);
        int sz_right = region->size - sz_left - sizeof(struct mem_region);
        void *mem_left, *mem_right;
        struct mem_region *left, *right;
        void* tmp;
        TEECAP_ALLOC_BOTTOM(left, tmp, mem, sizeof(struct mem_region));
        TEECAP_ALLOC_BOTTOM(mem_left, tmp, mem, sz_left);
        TEECAP_ALLOC_BOTTOM(right, tmp, mem, sizeof(struct mem_region));
        //TEECAP_ALLOC_BOTTOM(mem_right, tmp, mem, sz_right);
        mem_right = mem;

        delin(left);
        delin(right);
        left->mem = mem_left;
        right->mem = mem_right;
        left->size = sz_left;
        right->size = sz_right;
        left->leaf = 1;
        right->leaf = 1;
        left->free = 1;
        right->free = 1;

        region->free = 0;
        region->leaf = 0;
        region->left = left;
        region->right = right;
    }

    void* res = malloc_find(region->left, size);
    if(res == 0)
        return malloc_find(region->right, size);
    return res; 
}

// heap memory allocator
TEECAP_ATTR_HAS_METAPARAM void* malloc(int size) {
    struct malloc_state* malloc_state = TEECAP_METAPARAM;

    void* res = malloc_find(malloc_state->heap, size);
    if(res != 0){
        malloc_state->alloc_n = malloc_state->alloc_n + 1;
    }

    TEECAP_METAPARAM = malloc_state;
    returnsl(res, malloc);
}

void free_find(struct mem_region* region, void* mem) {
    if(region->leaf) {
        if(!region->free && region->mem.size == mem.size) {
            drop(mem);
            lin(region->mem); // TODO we need to initialise it first
            region->free = 1;
        }
        return;
    }
    if(mem.base < region->right.base) {
        free_find(region->left, mem);
    } else{
        free_find(region->right, mem);
    }
    // merge
    if(region->left->free && region->right->free) {
        drop(region->left->mem);
        drop(region->right->mem);
        lin(region->mem);
        region->leaf = 1;
        region->free = 1;
    }
}

TEECAP_ATTR_HAS_METAPARAM void free(void* mem) {
    struct malloc_state* malloc_state = TEECAP_METAPARAM;

    free_find(malloc_state->heap, mem);
    malloc_state->alloc_n = malloc_state->alloc_n - 1;

    TEECAP_METAPARAM = malloc_state;
    returnsl(43, free);
}

void malloc_init(struct malloc_state* malloc_state, void* heap) {
    struct mem_region* region = heap;
    void* mem = splitlo(region, sizeof(struct mem_region));
    int sz = mem.size;
    delin(region);
    region->mem = mem;
    region->left = 0;
    region->right = 0;
    region->leaf = 1;
    region->size = sz;
    region->free = 1;

    malloc_state->heap = region;
    malloc_state->alloc_n = 0;

}

#define TEECAP_SCHED_MAX_THREAD_N 16

struct sched_state {
    void* threads[TEECAP_SCHED_MAX_THREAD_N];
    int thread_n;
};


void new_thread(void* sealed_cap) {
    // given a sealed return capability, add it to the scheduler queue for scheduling
    // synchronisation might be an issue
    // more challenging is to keep malloc also safe
}

void sched_init(struct sched_state* sched_state) {
    sched_state->thread_n = 0;
    int i = 0;
    while(i < TEECAP_SCHED_MAX_THREAD_N) {
        sched_state->threads[i] = 0;
        i = i + 1;
    }
}

TEECAP_ATTR_HAS_METAPARAM void sched() {
    struct sched_state* sched_state = TEECAP_METAPARAM;

    // TODO: do scheduling here

    TEECAP_METAPARAM = sched_state;
}

void _start(void* heap) {
    struct teecap_runtime* runtime;
    void *malloc_sealed, *free_sealed, *sched_sealed, *tmp;
    TEECAP_ALLOC_BOTTOM(runtime, tmp, heap, sizeof(struct teecap_runtime));
    delin(runtime);
    // FIXME: let's say they can use the same runtime struct for now. See whether there will be problems
    runtime->version_minor = 1;
    runtime->version_major = 0;

    struct malloc_state *malloc_state;
    struct sched_state *sched_state;
    TEECAP_ALLOC_BOTTOM(malloc_sealed, tmp, heap, TEECAP_SEALED_REGION_SIZE);
    TEECAP_ALLOC_BOTTOM(free_sealed, tmp, heap, TEECAP_SEALED_REGION_SIZE);
    TEECAP_ALLOC_BOTTOM(sched_sealed, tmp, heap, TEECAP_SEALED_REGION_SIZE);
    TEECAP_ALLOC_BOTTOM(malloc_state, tmp, heap, sizeof(struct malloc_state));
    delin(malloc_state); // make sure this is written back
    TEECAP_ALLOC_BOTTOM(sched_state, tmp, heap, sizeof(struct sched_state));
    delin(sched_state);

    // set up initial malloc state

    malloc_init(malloc_state, heap);

    void *malloc_pc, *free_pc, *sched_pc, *epc;
    TEECAP_BUILD_CP(malloc_pc, malloc);
    runtime->malloc = sealed_setup(malloc_sealed, malloc_pc, 0, 0, malloc_state);
    TEECAP_BUILD_CP(free_pc, free);
    runtime->free = sealed_setup(free_sealed, free_pc, 0, 0, malloc_state);

    TEECAP_BUILD_CP(sched_pc, sched);
    epc = sealed_setup(sched_sealed, sched_pc, 0, 0, sched_state);



    main(runtime); // note that runtime is linear (hence we cannot directly pass it to a different threads)
    // when creating a new thread we need to duplicate the runtime struct as a result.
    // The sealed capabilities are linear, so we cannot do that while allowing everyone access to the interfaces
}

#endif
