#include <stdlib.h>
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

char rumStrget(char *str, int n) {
    return str[n];
}

//
char * rumStrsub(char *str, int f, int n) {
    int s = strlen(str);
    char* res = malloc(n + 1);
    int i;
    for (i = 0; i < n && i < s; i++) {
        res[i] = str[f + i];
    }
    res[i] = '\0';
    return res;
}

char * rumStrdup(char *str) {
    return strdup(str);
}

char * rumStrset(char *str, int i, char el) {
    str[i] = el;
    return str;
}

char * rumStrcat(char *str1, char *str2) {
    char *cat = malloc(strlen(str1) + strlen(str2) + 1);
    cat[0] = '\0';
    strcat(cat, str1);
    strcat(cat, str2);
    return cat;
}

int rumStrcmp(char *str1, char *str2) {
    return strcmp(str1, str2);
}

char *rumStrmake(int n, char c) {
    char *str = malloc(n + 1);
    memset(str, c, n);
    str[n] = '\0';
    return str;
}
