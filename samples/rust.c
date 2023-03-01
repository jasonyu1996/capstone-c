#include "capstone.h"


int main(struct capstone_runtime* runtime){
    void *borrowed_foo, *borrowed_bar, *moved_baz;

    void *foo = runtime->malloc(32);
    void *bar = runtime->malloc(32);
    void *baz = runtime->malloc(32);

    print(foo);
    print(bar);
    print(baz);

    CAPSTONE_BORROW(borrowed_foo, foo);
    CAPSTONE_BORROW_MUT(borrowed_bar, bar);
    CAPSTONE_MOVE(moved_baz, baz);

    print(foo);
    print(borrowed_foo);
    print(bar);
    print(borrowed_bar);
    print(baz);
    print(moved_baz);

    CAPSTONE_BORROW_END(foo);
    CAPSTONE_BORROW_MUT_END(bar);

    print(foo);
    print(bar);

    exit();
}


