#include "teecap.h"
#include "enclave.h"

TEECAP_ATTR_DEDICATED_STACK TEECAP_ATTR_HAS_METAPARAM void outer() {
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

    print(call_code);
    // print(11);

    void* inner_code = runtime->code;
    scco(inner_code, 0);
    // void* crypto_data = runtime->runtime->malloc(128);
    // struct enclave* encl = enclave_create(inner_code, crypto_data, 0, runtime);
    // encl = ecall(encl, 22);
    // enclave_destroy(encl, runtime);
    void *sealed = runtime->sealed;
    sealed = sealed_setup(sealed, inner_code, 0, 0, runtime);
    direct_call(sealed);

    // void* nested_code = runtime->runtime->malloc(128);
    // int i = 0;
    // void* nested_base;
    // TEECAP_BUILD_CP(nested_base, 0);
    // while(i + inner < outer) {
    //     nested_code[i] = nested_base[outer + i];
    //     i = i + 1;
    // }
    // scco(nested_code, 0);
    // void* nested_data = runtime->runtime->malloc(128);
    // struct enclave* inner_encl = enclave_create(nested_code, nested_data, runtime);
    // inner_encl = ecall(inner_encl, 22);
    // enclave_destroy(inner_encl, runtime);

    TEECAP_METAPARAM = runtime;

    TEECAP_STACK_RETURN;
}

TEECAP_ATTR_DEDICATED_STACK TEECAP_ATTR_HAS_METAPARAM void inner() {
    TEECAP_STACK_SETUP;

    struct enclave_runtime* runtime = TEECAP_METAPARAM;
    int call_code = runtime->shared[0];

    print(call_code);
    // print(22);

    TEECAP_METAPARAM = runtime;

    TEECAP_STACK_RETURN;
}

int main(struct teecap_runtime* runtime) {
    void* outer_code = runtime->malloc(128);
    void* inner_code = runtime->malloc(128);
    int i = 0;
    void* crypto_base;
    TEECAP_BUILD_CP(crypto_base, 0);
    while(i + outer < inner) {
        outer_code[i] = crypto_base[outer + i];
        i = i + 1;
    }
    i = 0;
    while(i + inner < main) {
        inner_code[i] = crypto_base[inner + i];
        i = i + 1;
    }
    scco(outer_code, 0);
    void* crypto_data = runtime->malloc(256);
    struct enclave* encl = enclave_create(outer_code, crypto_data, inner_code, runtime);
    encl = ecall(encl, 11);
    enclave_destroy(encl, runtime);

    // scco(inner_code, 0);
    // void* crypto_data = runtime->malloc(256);
    // struct enclave* encl = enclave_create(inner_code, crypto_data, inner_code, runtime);
    // encl = ecall(encl, 22);
    // enclave_destroy(encl, runtime);

    exit();
}

