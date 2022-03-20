#include "teecap.h"


TEECAP_ATTR_DEDICATED_STACK TEECAP_ATTR_HAS_METAPARAM void encrypt() {
    TEECAP_STACK_SETUP;

    print(25);
    
    TEECAP_STACK_RETURN;
}

TEECAP_ATTR_DEDICATED_STACK TEECAP_ATTR_HAS_METAPARAM void decrypt() {
    TEECAP_STACK_SETUP;

    int xyz = TEECAP_METAPARAM;
    print(xyz);
    print(26);

    TEECAP_STACK_RETURN;
}

int main(struct teecap_runtime* runtime) {
    struct enclave *e = runtime->malloc(sizeof(struct enclave));
    void *buffer = runtime->malloc(sizeof(TEECAP_SEALED_REGION_SIZE));
    delin(buffer);
    void* revoke_e = mrev(e);
    e = create_enclave(e, decrypt, buffer, runtime);
    e = enter_enclave(e);
    destroy_enclave(e, revoke_e);
    exit();
}

