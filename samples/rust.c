#include "teecap.h"


int main(struct teecap_runtime* runtime){
    void *borrowed_foo, *borrowed_bar, *moved_baz;

    void *foo = runtime->malloc(32);
    void *bar = runtime->malloc(32);
    void *baz = runtime->malloc(32);

    print(foo);
    print(bar);
    print(baz);

    TEECAP_BORROW(borrowed_foo, foo);
    TEECAP_BORROW_MUT(borrowed_bar, bar);
    TEECAP_MOVE(moved_baz, baz);

    print(foo);
    print(borrowed_foo);
    print(bar);
    print(borrowed_bar);
    print(baz);
    print(moved_baz);

    TEECAP_BORROW_END(foo);
    TEECAP_BORROW_MUT_END(bar);

    print(foo);
    print(bar);

    exit();
}


