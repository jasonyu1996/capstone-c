#include "teecap.h"
#include "enclave.h"

#define CRYPTO_CALL_ENCRYPT 0
#define CRYPTO_CALL_DECRYPT 1

TEECAP_ATTR_DEDICATED_STACK TEECAP_ATTR_HAS_METAPARAM void crypto() {
    TEECAP_STACK_SETUP;
    
    struct enclave_runtime* runtime = TEECAP_METAPARAM;
    int call_code = runtime->shared[0];
    // this following does not work for now
    // need relative addressing in instructions
    /*if(call_code == 0) {*/
        /*// encrypt*/
    /*} */
    /*if(call_code == 1) {*/
        /*// decrypt*/
    /*}*/

    TEECAP_METAPARAM = runtime;

    TEECAP_STACK_RETURN;
}

int main(struct teecap_runtime* runtime) {
    void* crypto_code = runtime->malloc(128);
    int i = 0;
    void* crypto_base;
    TEECAP_BUILD_CP(crypto_base, 0);
    while(i + crypto < main) {
        crypto_code[i] = crypto_base[crypto + i];
        i = i + 1;
    }
    scco(crypto_code, 0);
    void* crypto_data = runtime->malloc(256);
    struct enclave* encl = create_enclave(crypto_code, crypto_data, runtime);
    encl = ecall(encl, CRYPTO_CALL_DECRYPT);
    destroy_enclave(encl, runtime);
    exit();
}

