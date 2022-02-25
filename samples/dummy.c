int main(){
    int a = 0, b;
    b = a + 1;
    if(a) {
        a = a + b;
    }
    while(b) {
        b = a + 2;
        if(a) {
            break;
        }
    }
    print(a);
    return 0;
}
