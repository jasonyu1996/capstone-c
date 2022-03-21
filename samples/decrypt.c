#include "teecap.h"


TEECAP_ATTR_DEDICATED_STACK TEECAP_ATTR_HAS_METAPARAM void decrypt() {
    TEECAP_STACK_SETUP;
    
    print(TEECAP_METAPARAM);

    TEECAP_STACK_RETURN;
}

int main(struct teecap_runtime* runtime) {
    void* decrypt_code = runtime->malloc(128);
    int i = 0;
    void* decrypt_base;
    TEECAP_BUILD_CP(decrypt_base, decrypt);
    while(i + decrypt < main) {
        decrypt_code[i] = decrypt_base[decrypt + i];
        i = i + 1;
    }
    scco(decrypt_code, 0);
    void* decrypt_data = runtime->malloc(256);
    struct enclave* encl = create_enclave(decrypt_code, decrypt_data, runtime);
    encl = enter_enclave(encl);
    destroy_enclave(encl, runtime);
    exit();
}

