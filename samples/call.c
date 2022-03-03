void bar() {
    int t = 42;
    print(t);
}

void foo(int a, int b) {
    int a = 1, b = 2;
    print(a + b);
    bar();
}

int _start(){
    int a = 2, b = 4;
    foo();
    print(a + b);
    foo();
    exit();
}
