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

