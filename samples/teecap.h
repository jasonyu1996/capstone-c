#ifndef TEECAP_H
#define TEECAP_H

#define TEECAP_SEALED_OFFSET_PC 0
#define TEECAP_SEALED_OFFSET_SC 1
#define TEECAP_SEALED_OFFSET_EPC 2
#define TEECAP_SEALED_OFFSET_RET 3
#define TEECAP_SEALED_OFFSET_STACK 4
#define TEECAP_SEALED_OFFSET_METAPARAM 5
#define TEECAP_SEALED_OFFSET_DEDICATED_STACK 6

#define TEECAP_GPR_N 32
#define TEECAP_SEALED_REGION_SIZE 36
#define TEECAP_METAPARAM (reg("r1"))
#define TEECAP_STACK_SETUP reg("r0") = reg("r2");
#define TEECAP_STACK_RETURN reg("r2") = reg("r0");

#define TEECAP_ATTR_HAS_METAPARAM __attribute__((pinned("r1")))
#define TEECAP_ATTR_DEDICATED_STACK __attribute__((pinned("r2")))

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
    void* thread_start;
};

void* sealed_setup(int* cap, void* pcc, void* epc, void* stack, int* metaparam) {
    cap[TEECAP_SEALED_OFFSET_PC] = pcc;
    cap[TEECAP_SEALED_OFFSET_EPC] = epc;
    cap[TEECAP_SEALED_OFFSET_DEDICATED_STACK] = stack; // stack is actually specified upon call
    cap[TEECAP_SEALED_OFFSET_METAPARAM] = metaparam;
    seal(cap);
    return cap;
}


// TODO: make this thing more general
void* sealed_return_setup(int* cap, void* pcc, void* epc, void* stack, int* metaparam) {
    cap[TEECAP_SEALED_OFFSET_PC] = pcc;
    cap[TEECAP_SEALED_OFFSET_EPC] = epc;
    cap[TEECAP_SEALED_OFFSET_DEDICATED_STACK] = stack; // stack is actually specified upon call
    cap[TEECAP_SEALED_OFFSET_METAPARAM] = metaparam;
    sealret(cap, "epc");
    return cap;
}
// ---------------------------------------------------------------
// memory allocator

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
        void* mem = region->mem;
        region->mem = mrev(mem);
        if(region->size <= (size + sizeof(struct mem_region)) * 2){
            region->free = 0;
            return mem;
        }
        // split

        int sz_left = region->size / 2 - sizeof(struct mem_region);
        int sz_right = region->size - region->size / 2 - sizeof(struct mem_region);
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
    returnsl(0, free);
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

// --------------------------------------------
// Thread scheduler

#define TEECAP_SCHED_MAX_THREAD_N 16

struct sched_state {
    void* threads[TEECAP_SCHED_MAX_THREAD_N];
    int thread_n; // this does not include the current thread
    // so the actual number threads is 
    // thread_n + 1 assuming a single physical thread
    // More generally, it would be thread_n + n, where n is the number of 
    // physical threads
};

// the wrapper does very minimal things. We do not need a larger stack
#define TEECAP_THREAD_WRAPPER_STACK_SIZE 32

// wrapper function
TEECAP_ATTR_HAS_METAPARAM 
TEECAP_ATTR_DEDICATED_STACK void _run_thread() {
    TEECAP_STACK_SETUP;
    void* thread = TEECAP_METAPARAM;

    thread();

    // TODO: here probably we would need to seal some caps to manipulate sched state
    TEECAP_METAPARAM = thread;
    TEECAP_STACK_RETURN;
}


/// Called by a thread to add another thread to the scheduler
TEECAP_ATTR_HAS_METAPARAM void thread_start(void* thread_cap) {
    struct sched_state* sched_state = TEECAP_METAPARAM;
    // given a sealed return capability, add it to the scheduler queue for scheduling
    // synchronisation might be an issue
    // more challenging is to keep malloc also safe
    
    // need to wrap inside a seal return capability
    
    // add wrapped thread capability to thread list
    // TODO: here we assume that only one thread is using thread_start and can change thread_n + 1;
    // we need to think about synchronisation in general
    
    int thread_n = sched_state->thread_n;
    sched_state->threads[thread_n] = thread_cap;
    sched_state->thread_n = thread_n + 1;

    TEECAP_METAPARAM = sched_state;
    returnsl(0, thread_start);
}


/// An exception handler that performs scheduling
TEECAP_ATTR_HAS_METAPARAM 
TEECAP_ATTR_DEDICATED_STACK void sched() {
    TEECAP_STACK_SETUP;
    struct sched_state* sched_state = TEECAP_METAPARAM;

    int thread_n = sched_state->thread_n;
    void *next_thread, *tmp;
    int i = 0;
    if(thread_n){
        next_thread = sched_state->threads[0];
        while(i + 1 < thread_n) {
            tmp = sched_state->threads[i + 1]; // FIXME: avoiding compiler bugs for now
            sched_state->threads[i] = tmp;
            i = i + 1;
        }
        sched_state->threads[thread_n - 1] = reg("ret");
        reg("ret") = next_thread;
    }

    TEECAP_METAPARAM = sched_state;
    TEECAP_STACK_RETURN;
    returnsl(0, sched);
}

void sched_init(struct sched_state* sched_state) {
    sched_state->thread_n = 0;
    int i = 0;
    while(i < TEECAP_SCHED_MAX_THREAD_N) {
        sched_state->threads[i] = 0;
        i = i + 1;
    }
}

#define TEECAP_SCHED_STACK_SIZE 4096
#define TEECAP_THREAD_STACK_SIZE 256

/// return a sealed capability that can be passed to 
// thread_start() for scheduling
void* create_thread(struct teecap_runtime* runtime, int func) {
    void* sealed_cap = runtime->malloc(TEECAP_SEALED_REGION_SIZE);
    void* pc;
    TEECAP_BUILD_CP(pc, func);
    void* stack = runtime->malloc(TEECAP_THREAD_STACK_SIZE);

    void* wrapper_stack = runtime->malloc(TEECAP_THREAD_WRAPPER_STACK_SIZE);
    void* wrapper_sealed_region = runtime->malloc(TEECAP_SEALED_REGION_SIZE);
    void* wrapper_pc;

    sealed_cap = sealed_setup(sealed_cap, pc, 0, stack, 0);

    TEECAP_BUILD_CP(wrapper_pc, _run_thread);
    void* wrapper_sealed = sealed_return_setup(wrapper_sealed_region, wrapper_pc, 0, wrapper_stack, sealed_cap);

    return wrapper_sealed;
}


void _start(void* heap) {
    struct teecap_runtime* runtime;
    void *malloc_sealed, *free_sealed, *sched_sealed, *thread_start_sealed, *tmp;
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
    TEECAP_ALLOC_BOTTOM(thread_start_sealed, tmp, heap, TEECAP_SEALED_REGION_SIZE);
    TEECAP_ALLOC_BOTTOM(malloc_state, tmp, heap, sizeof(struct malloc_state));
    delin(malloc_state); // make sure this is written back
    TEECAP_ALLOC_BOTTOM(sched_state, tmp, heap, sizeof(struct sched_state));
    delin(sched_state);

    // scheduler stack
    void* sched_stack;
    TEECAP_ALLOC_BOTTOM(sched_stack, tmp, heap, TEECAP_SCHED_STACK_SIZE);
    // set up initial malloc state

    malloc_init(malloc_state, heap);
    sched_init(sched_state);

    void *malloc_pc, *free_pc, *sched_pc, *thread_start_pc, *epc;
    TEECAP_BUILD_CP(malloc_pc, malloc);
    runtime->malloc = sealed_setup(malloc_sealed, malloc_pc, 0, 0, malloc_state);
    TEECAP_BUILD_CP(free_pc, free);
    runtime->free = sealed_setup(free_sealed, free_pc, 0, 0, malloc_state);
    TEECAP_BUILD_CP(thread_start_pc, thread_start);
    runtime->thread_start = sealed_setup(thread_start_sealed, thread_start_pc, 0, 0, sched_state);

    TEECAP_BUILD_CP(sched_pc, sched);
    epc = sealed_setup(sched_sealed, sched_pc, 0, sched_stack, sched_state);

    // set epc of current thread
    reg("epc") = epc;

    main(runtime); // note that runtime is linear (hence we cannot directly pass it to a different threads)
    // when creating a new thread we need to duplicate the runtime struct as a result.
    // The sealed capabilities are linear, so we cannot do that while allowing everyone access to the interfaces
}

#endif
