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

#include "capstone.h"

int main(struct capstone_runtime* runtime){
    print(runtime->version_major);
    print(runtime->version_minor);

    void* data[50];

    int k = 0;
    int i = 0;
    while(k < 5){
        i = 0;
        while(i < 10) {
            data[i] = runtime->malloc((i + 5) * 40);
            i = i + 1;
        }

        i = 0;
        while(i < 10) {
            print(data[i]);
            print(data[i].size);
            i = i + 1;
        }

        i = 0;
        while(i < 10) {
            print(i);
            runtime->free(data[i]);
            i = i + 1;
        }

        k = k + 1;
    }

    exit();
}
