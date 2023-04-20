/* MIT License

Copyright (c) 2023 National University of Singapore

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

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

int _start(){
    struct Vec3 v1, v2;

    v1.a = 2; v1.b = 3; v1.c = 1;
    v2.a = 3; v2.b = 1; v2.c = 0;
    print(foo(v1, v2));

    v1 = v2;
    print(v1.a);

    exit();
}
