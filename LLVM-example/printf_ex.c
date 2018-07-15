#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// clang-4.0 -S -emit-llvm printf_ex.c -c -o printf_ex.ll

struct MyArr {
    int* arr;
    int len;
};
int main() {


int* arr;
arr = malloc(3);
arr[0] = 1;
arr[1] = 2;
arr[2] = 3;
//int[] some_arr = {1,2,3};

struct MyArr ar;
ar.len = 3;
ar.arr = arr;

//printf("Len: %d", ar.len);
printf("arr[0]: %d\n", ar.arr[0]);
printf("arr[1]: %d\n", ar.arr[1]);
printf("arr[2]: %d\n", ar.arr[2]);
printf("arr[3]: %d\n", ar.arr[3]);

return 0;
}

/*
v :: { arr :: i32*, len :: i32 }
n = len (ArrLit)
v.arr = malloc(n);
for i := 0 to n - 1 {
  v.arr[i] = ArrLit[i];
}
v.len = n;
*/
//
//int** foo(int** arr) {
//   int* localArr;
//   arr[3] = localArr;
//   return arr;
//}
//
//int main ()
//{
//   int str [5]={1, 7, 14, 21, 28};
//   int z = str[3];
//   printf("%d", z);
//   printf("%d", sizeof(str)/sizeof(int));
//
//   return 0;
//}

//int rumArrlen(struct MyArr a) {
//    return a.len;
//}