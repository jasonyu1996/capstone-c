struct fib {
    int a0;
    int a1;
};

int _start(){
    struct fib res;
    res.a0 = 1;
    res.a1 = 1;
    int i = 0;
    int tmp;
    while(i < 10) {
        i = i + 1;
        tmp = res.a1;
        res.a1 = res.a0;
        res.a0 = res.a0 + tmp;
        print(res.a0);
    }
    exit();
}
