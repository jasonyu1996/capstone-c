#ifndef ENCLAVE_H
#define ENCLAVE_H

#include "capstone.h"

struct enclave* ecall(struct enclave* encl, int call_no) {
    encl->shared[0] = call_no;
    return enclave_enter(encl);
}


#endif
