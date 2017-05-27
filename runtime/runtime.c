#include <stdio.h>
#include <string.h>

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

int rumStrlen(char *str) {
    return strlen(str);
}
//
//int rumStrget(int *str, int n) {
//    return str[n];
//}
//
//int * rumStrsub(int *str, int f, int n) {
//    int s = sizeof(str);
//    int res[n];
//    for ( i = 0; i < n && i < s; i++) {
//        res[i] = str[f + i];
//    }
//    return res;
//}
//
//int * rumStrdup(int *str) {
//    return str;
//}
//
//int * rumStrset(int *str, int i, int el) {
//
//}
