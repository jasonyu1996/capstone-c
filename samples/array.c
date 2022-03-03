void _start(void* heap){
    int data[100], i = 1;
    while(i < 100) {
        data[i] = data[i - 1] + i;
        i = i + 1;
    }
    i = 0;
    while(i < 100) {
        print(data[i]);
        i = i + 1;
    }
    exit();
}
