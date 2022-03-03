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

struct teecap_runtime {
    int version_major;
    int version_minor;
    void* malloc; // a sealed capability for invoking the memory allocator
};

void* sealed_setup(int* cap, void* pcc, void* epc, void* stack, int* metaparam) {
    cap[TEECAP_SEALED_OFFSET_PC] = pcc;
    cap[TEECAP_SEALED_OFFSET_EPC] = epc;
    cap[TEECAP_SEALED_OFFSET_STACK] = stack; // stack is actually specified upon call
    cap[TEECAP_SEALED_OFFSET_METAPARAM] = metaparam;
    seal(cap);
    return cap;
}

void* malloc(int size) {
    void* heap = TEECAP_METAPARAM;
    returnsl(42, malloc);
}

void _start(void* heap) {
    struct teecap_runtime runtime;
    runtime.version_minor = 1;
    runtime.version_major = 0;
    void* remaining_heap = splitlo(heap, TEECAP_SEALED_REGION_SIZE);
    void* malloc_pc = reg("pc");
    scc(malloc_pc, malloc);
    runtime.malloc = sealed_setup(heap, malloc_pc, 0, 0, remaining_heap);
    main(runtime);
}

#endif
