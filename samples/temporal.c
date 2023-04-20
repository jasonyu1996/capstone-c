/* MIT License

Copyright (c) 2023 National University of Singapore

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

#include "capstone.h"
#include "enclave.h"

CAPSTONE_ATTR_DEDICATED_STACK CAPSTONE_ATTR_HAS_METAPARAM void enclave1() {
    CAPSTONE_STACK_SETUP;
    struct enclave_runtime* runtime = CAPSTONE_METAPARAM;
    struct capstone_runtime* capstone_runtime = runtime->runtime;

    void* setup_shared_code = capstone_runtime->malloc(512);

    int i = 0, code_size = runtime->heap[0];
    while(i < code_size) {
        setup_shared_code[i] = runtime->heap[i + 1];
        i = i + 1;
    }

    void* shared_mem = capstone_runtime->malloc(128);
    shared_mem[0] = 44; // some random data
    void* shared_rev = mrev(shared_mem);
    void* shared_rev_shared = mrev(shared_mem);

    drop(shared_mem); // drop to share

    runtime->heap[1] = shared_rev;
    
    void* setup_shared_cap = capstone_runtime->malloc(CAPSTONE_SEALED_REGION_SIZE);
    scco(setup_shared_code, 0);
    setup_shared_cap[CAPSTONE_SEALED_OFFSET_PC] = setup_shared_code;
    setup_shared_cap[CAPSTONE_SEALED_OFFSET_EPC] = 0;
    setup_shared_cap[CAPSTONE_SEALED_OFFSET_DEDICATED_STACK] = 0; // stack is actually specified upon call
    setup_shared_cap[CAPSTONE_SEALED_OFFSET_METAPARAM] = shared_rev_shared;
    seal(setup_shared_cap);

    runtime->shared[0] = setup_shared_cap;

    CAPSTONE_METAPARAM = runtime;
    CAPSTONE_STACK_RETURN;
}

CAPSTONE_ATTR_DEDICATED_STACK CAPSTONE_ATTR_HAS_METAPARAM void enclave2() {
    CAPSTONE_STACK_SETUP;
    struct enclave_runtime* runtime = CAPSTONE_METAPARAM;

    void* d = runtime->shared[0];
    void* shared_mem = d();
    
    revoke(shared_mem);

    print(shared_mem);
    print(shared_mem[0]);
    
    CAPSTONE_METAPARAM = runtime;
    CAPSTONE_STACK_RETURN;
}


CAPSTONE_ATTR_HAS_METAPARAM void* setup_shared() {
    void* d = CAPSTONE_METAPARAM;

    // perform attestation

    return d;
}

int main(struct capstone_runtime* runtime) {
    void* enclave1_code = runtime->malloc(512);
    void* enclave2_code = runtime->malloc(512);
    int i = 0;
    void* code_base;
    CAPSTONE_BUILD_CP(code_base, 0);

    void* data1 = runtime->malloc(512);
    while(i + enclave1 < enclave2) {
        enclave1_code[i] = code_base[enclave1 + i];
        i = i + 1;
    }
    scco(enclave1_code, 0);

    i = 0;
    while(i + setup_shared < main) {
        data1[i + 1] = code_base[setup_shared + i];
        i = i + 1;
    }
    data1[0] = i; // size
    
    void* data2 = runtime->malloc(512);
    i = 0;
    while(i + enclave2 < setup_shared){
        enclave2_code[i] = code_base[enclave2 + i];
        i = i + 1;
    }
    scco(enclave2_code, 0);

    struct enclave* encl1 = runtime->enclave_create(enclave1_code, data1, runtime);
    struct enclave* encl2 = runtime->enclave_create(enclave2_code, data2, runtime);

    encl1 = runtime->enclave_enter(encl1);
    void* d = encl1->shared[0]; // marshalls to encl2
    encl2->shared[0] = d;
    encl2 = runtime->enclave_enter(encl2);

    exit();
}

