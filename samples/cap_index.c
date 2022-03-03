struct dummy {
    int a, b;
};

struct data {
    int foo;
    struct dummy bar;
};


int _start(struct data *heap){
    heap[0].foo = 42;
    print(heap[0].foo);
    print(heap[1].foo);
    print(heap[0].foo);
    exit();
}
