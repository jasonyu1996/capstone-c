int main(){
    int n0 = 1, n1 = 1, i = 0;
    int tmp;
    while(i < 10) {
        i = i + 1;
        tmp = n0 + n1;
        n0 = n1;
        n1 = tmp;
        print(n1);
    }
    exit();
}
