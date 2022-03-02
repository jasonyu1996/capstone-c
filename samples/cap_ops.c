int main(int *heap){
    print(heap);
    print(heap.cursor);
    int* object;
    object = splitlo(heap, 4);
    print(heap);
    print(object);
    print(object.base);
    scco(object, 0);
    print(object);
    exit();
}
