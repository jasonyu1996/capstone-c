#include "teecap.h"

TEECAP_ATTR_DEDICATED_STACK void foo() {
    TEECAP_STACK_SETUP;

    int i = 0;
    while(i < 50) {
        print(i);
        i = i + 1; 
    }

    while(1); // for now let's do nothing
    TEECAP_STACK_RETURN;
}

void main(struct teecap_runtime* runtime){
    int i = 0;
    void* threads[4];
    while(i < 4) {
        threads[i] = create_thread(runtime, foo);
        print(threads[i]);
        i = i + 1;
    }
    i = 0;
    while(i < 4) {
        runtime->thread_start(threads[i]);
        i = i + 1;
    }
    while(1);
    exit();
}

