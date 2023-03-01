#include "capstone.h"
#include "enclave.h"

CAPSTONE_ATTR_DEDICATED_STACK CAPSTONE_ATTR_HAS_METAPARAM void enclave() {
    CAPSTONE_STACK_SETUP;
    
    struct enclave_runtime* runtime = CAPSTONE_METAPARAM;
    struct capstone_runtime* capstone_runtime = runtime->runtime;
    int level = runtime->heap[0];

    print(level);
    if(level > 2) {
        CAPSTONE_METAPARAM = runtime;
        CAPSTONE_STACK_RETURN;
        return;
    }

    void* inner_enclave_code = capstone_runtime->malloc(512);
    void* data = capstone_runtime->malloc(512);
    int i = 0, code_size = runtime->heap[1];
    while(i < code_size) {
        inner_enclave_code[i] = runtime->heap[2 + i];
        data[2 + i] = runtime->heap[2 + i];
        i = i + 1;
    }
    data[0] = level + 1;
    data[1] = code_size;
    scco(inner_enclave_code, 0);

    struct enclave* inner_enclave = capstone_runtime->enclave_create(inner_enclave_code, data, capstone_runtime);
    print(inner_enclave);
    direct_call(inner_enclave->sealed);
    capstone_runtime->enclave_destroy(inner_enclave, capstone_runtime);

    CAPSTONE_METAPARAM = runtime;
    CAPSTONE_STACK_RETURN;
}

int main(struct capstone_runtime* runtime) {
    void* enclave_code = runtime->malloc(512);
    int i = 0;
    void* code_base;
    CAPSTONE_BUILD_CP(code_base, 0);
    void* data = runtime->malloc(512);
    while(i + enclave < main) {
        enclave_code[i] = code_base[enclave + i];
        data[i + 2] = code_base[enclave + i];
        i = i + 1;
    }
    data[1] = i;
    data[0] = 1; // level
    scco(enclave_code, 0);
    struct enclave* encl = runtime->enclave_create(enclave_code, data, runtime);
    encl = runtime->enclave_enter(encl);
    enclave_destroy(encl, runtime);

    exit();
}

