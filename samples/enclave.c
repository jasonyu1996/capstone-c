#include "capstone.h"
#include "enclave.h"

#define CRYPTO_CALL_ENCRYPT 0
#define CRYPTO_CALL_DECRYPT 1

CAPSTONE_ATTR_DEDICATED_STACK CAPSTONE_ATTR_HAS_METAPARAM void crypto() {
    CAPSTONE_STACK_SETUP;
    
    struct enclave_runtime* runtime = CAPSTONE_METAPARAM;
    int call_code = runtime->shared[0];

    CAPSTONE_METAPARAM = runtime;

    CAPSTONE_STACK_RETURN;
}

int main(struct capstone_runtime* runtime) {
    void* crypto_code = runtime->malloc(128);
    int i = 0;
    void* crypto_base;
    CAPSTONE_BUILD_CP(crypto_base, 0);
    while(i + crypto < main) {
        crypto_code[i] = crypto_base[crypto + i];
        i = i + 1;
    }
    scco(crypto_code, 0);
    void* crypto_data = runtime->malloc(256);
    struct enclave* encl = enclave_create(crypto_code, crypto_data, runtime);
    encl = ecall(encl, CRYPTO_CALL_DECRYPT);
    enclave_destroy(encl, runtime);
    exit();
}

