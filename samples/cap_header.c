#include "teecap.h"

int main(struct teecap_runtime* runtime){
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
