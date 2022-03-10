#include "teecap.h"

TEECAP_ATTR_DEDICATED_STACK void foo() {
    TEECAP_STACK_SETUP;

    int i = 0;
    while(i < 50) {
        print(i);
        i = i + 1; 
    }

    TEECAP_STACK_RETURN;
}

void main(struct teecap_runtime* runtime){
    int i = 0;
    void* threads[4];
    while(i < 4) {
        threads[i] = runtime->create_thread(runtime, foo);
        print(threads[i]);
        i = i + 1;
    }
    i = 0;
    while(i < 4) {
        runtime->thread_start(threads[i]);
        i = i + 1;
    }
    runtime->join_all();
    exit();
}

