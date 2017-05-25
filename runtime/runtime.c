#include <stdio.h>

int rumWrite(int val) {
    printf("%d\n", val);
    return 0;
}

int rumRead() {
    int val;
//    printf("> ");
    scanf("%d", &val);
    return val;
}