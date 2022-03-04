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


struct teecap_runtime {
    int version_major;
    int version_minor;
    void* malloc; // a sealed capability for invoking the memory allocator
    void* free;
};

void* sealed_setup(int* cap, void* pcc, void* epc, void* stack, int* metaparam) {
    cap[TEECAP_SEALED_OFFSET_PC] = pcc;
    cap[TEECAP_SEALED_OFFSET_EPC] = epc;
    cap[TEECAP_SEALED_OFFSET_STACK] = stack; // stack is actually specified upon call
    cap[TEECAP_SEALED_OFFSET_METAPARAM] = metaparam;
    seal(cap);
    return cap;
}

struct malloc_state {
    void* heap;
    int alloc_n;
};

// heap memory allocator
TEECAP_ATTR_HAS_METAPARAM void* malloc(int size) {
    struct malloc_state* malloc_state = TEECAP_METAPARAM;
    print(malloc_state->heap);
    malloc_state->alloc_n = malloc_state->alloc_n + 1;
    TEECAP_METAPARAM = malloc_state;
    returnsl(42, malloc);
}

TEECAP_ATTR_HAS_METAPARAM void free(void* mem) {
    struct malloc_state* malloc_state = TEECAP_METAPARAM;
    print(malloc_state->heap);
    print(malloc_state->alloc_n);
    TEECAP_METAPARAM = malloc_state;
    returnsl(43, free);
}

void _start(void* heap) {
    struct teecap_runtime runtime;
    runtime.version_minor = 1;
    runtime.version_major = 0;
    void *malloc_sealed, *free_sealed, *tmp;
    struct malloc_state *malloc_state;
    TEECAP_ALLOC_BOTTOM(malloc_sealed, tmp, heap, TEECAP_SEALED_REGION_SIZE);
    TEECAP_ALLOC_BOTTOM(free_sealed, tmp, heap, TEECAP_SEALED_REGION_SIZE);
    TEECAP_ALLOC_BOTTOM(malloc_state, tmp, heap, sizeof(struct malloc_state));
    delin(malloc_state); // make sure this is written back
    print(malloc_state);

    // set up initial malloc state
    malloc_state->heap = heap;
    malloc_state->alloc_n = 0;

    void *malloc_pc, *free_pc;
    TEECAP_BUILD_CP(malloc_pc, malloc);
    runtime.malloc = sealed_setup(malloc_sealed, malloc_pc, 0, 0, malloc_state);
    TEECAP_BUILD_CP(free_pc, free);
    runtime.free = sealed_setup(free_sealed, free_pc, 0, 0, malloc_state);

    main(runtime);
}

#endif
