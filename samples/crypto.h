#ifndef CRYPTO_H
#define CRYPTO_H

TEECAP_ATTR_HAS_METAPARAM void _encrypt() {
    TEECAP_STACK_SETUP;

    print(25);
    
    TEECAP_STACK_RETURN;
}

TEECAP_ATTR_HAS_METAPARAM void _decrypt() {
    TEECAP_STACK_SETUP;

    int xyz = TEECAP_METAPARAM;
    print(xyz);
    print(26);

    TEECAP_STACK_RETURN;
}

#endif