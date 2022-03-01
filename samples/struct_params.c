struct Vec3 {
    int a, b, c;
};

int max(int a, int b) {
    if(a > b)
        return a;
    else
        return b;
}

int foo(struct Vec3 v1, struct Vec3 v2){
    int a; 
    int b;
    int c;

    a = max(v1.a, v2.a);
    b = max(v1.b, v2.b);
    c = max(v1.c, v2.c);

    return a + b + c;
}

int main(){
    struct Vec3 v1, v2;

    v1.a = 2; v1.b = 3; v1.c = 1;
    v2.a = 3; v2.b = 1; v2.c = 0;
    print(foo(v1, v2));

    v1 = v2;
    print(v1.a);

    exit();
}
