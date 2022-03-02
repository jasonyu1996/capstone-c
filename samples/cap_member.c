struct dummy {
    int a, b;
};

struct data {
    int foo;
    struct dummy bar;
};


int main(struct data *heap){
    print(heap);
    print(heap.cursor);
    print(heap->foo);
    heap->foo = 42;
    print(heap->foo);
    print(heap->bar.a);
    heap->bar.a = 51;
    print(heap->bar.b);
    print(heap->bar.a);
    int b;
    exit();
}
