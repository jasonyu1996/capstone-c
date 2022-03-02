#ifndef TEECAP_H
#define TEECAP_H

void splitlo(int* rs, int rp) {
    __asm__("li r1 0"
            "scco r0 r1"
            "ld r1 r0" // get the source capability
            "li r2 1"
            "scco r0 r2"
            "ld r2 r0" // get the splitting point
            "splitlo r3 r1 r2"
            "drop sc"
            "drop r0"
            "ret ret r3");
}

#endif
