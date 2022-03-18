#include "teecap.h"
#include "crypto.h"

int main(struct teecap_runtime* runtime) {
    struct enclave *e = runtime->malloc(sizeof(struct enclave));
    void *buffer = runtime->malloc(sizeof(TEECAP_SEALED_REGION_SIZE));
    delin(buffer);
    buffer = 234;
    void* revoke_e = mrev(e);
    e = create_enclave(e, _decrypt, buffer, runtime);
    e = enter_enclave(e);
    destroy_enlave(e, revoke_e);
    // drop(e);
    // lin(revoke_e);
    exit();
}