#ifndef ENCLAVE_H
#define ENCLAVE_H

#include "teecap.h"

struct enclave* ecall(struct enclave* encl, int call_no) {
    encl->shared[0] = call_no;
    return enclave_enter(encl);
}


#endif
