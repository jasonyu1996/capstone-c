int _start(){
    int a = 6024, b = 304, tmp;
    while(a != 0) {
        while(a >= b){
            a = a - b;
        }
        if(a == 0)
            break;
        tmp = a;
        a = b;
        b = tmp;
    }
    print(b);
    exit();
}
