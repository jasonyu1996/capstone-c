#include "teecap.h"

int main(struct teecap_runtime runtime){
    print(runtime.version_major);
    print(runtime.version_minor);
    print(runtime.malloc);
    print(runtime.malloc());
    print(runtime.malloc);
    print(runtime.malloc());
    print(runtime.malloc);
    print(runtime.malloc());
    print(runtime.malloc);
    print(runtime.malloc());
    print(runtime.malloc);
    runtime.free();
    exit();
}
