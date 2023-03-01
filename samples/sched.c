#include "capstone.h"

CAPSTONE_ATTR_DEDICATED_STACK void foo() {
    CAPSTONE_STACK_SETUP;

    int i = 0;
    while(i < 50) {
        print(i);
        i = i + 1; 
    }

    CAPSTONE_STACK_RETURN;
}

void main(struct capstone_runtime* runtime){
    int i = 0;
    void* threads[4];
    while(i < 4) {
        threads[i] = runtime->thread_create(runtime, foo);
        print(threads[i]);
        i = i + 1;
    }
    i = 0;
    while(i < 4) {
        runtime->start_thread(threads[i]);
        i = i + 1;
    }
    runtime->join_all();
    exit();
}

