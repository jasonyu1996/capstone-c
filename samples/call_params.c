int fib(int n) {
    if(n == 0 || n == 1) {
        return 1;
    }
    return fib(n - 1) + fib(n - 2);
}

int _start() {
    print(fib(10));
    exit();
}

