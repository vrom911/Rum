; ModuleID = 'prog.expr'
; gcc -c -fPIC runtime.c -o runtime.o
; gcc runtime.o -shared -o runtime.so
;  lli-4.0 -load runtime/runtime.so LLVM-example/local_example.ll

source_filename = "<string>"

define i32 @main() {
main:
  %0 = alloca i32*
  %1 = call i8* @malloc(i32 2)
  %2 = bitcast i8* %1 to i32*
  store i32* %2, i32** %0
  %3 = load i32*, i32** %0
  %4 = getelementptr inbounds i32, i32* %3, i32 0
  store i32 1, i32* %4
  %5 = load i32*, i32** %0
  %6 = getelementptr inbounds i32, i32* %5, i32 1
  store i32 2, i32* %6
  %7 = alloca { i32*, i32 }
  %8 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %7, i32 0, i32 1
  store i32 2, i32* %8
  %9 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %7, i32 0, i32 0
  store i32* %2, i32** %9
  %10 = load { i32*, i32 }, { i32*, i32 }* %7
  %11 = alloca { i32*, i32 }
  store { i32*, i32 } %10, { i32*, i32 }* %11
  %12 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %11, i32 0, i32 1
  %13 = load i32, i32* %12
  %14 = call i32 @rumWrite(i32 %13)
;  %13 = load i32*, i32** %12
;  %14 = getelementptr inbounds i32, i32* %13, i32 1
;  %15 = load i32, i32* %14
;  %16 = call i32 @rumWrite(i32 %15)
  ret i32 0
}

declare i8* @malloc(i32)

declare i32 @rumRead()

declare i32 @rumWrite(i32)

declare i32 @rumWriteStr(i32)

declare i32 @rumStrlen(i8*)

declare i32 @rumStrget(i8*, i32)

declare i32 @rumStrcmp(i8*, i8*)

declare i32 @rumArrlen({ i32*, i32 })

declare i8* @rumStrsub(i8*, i32, i32)

declare i8* @rumStrdup(i8*)

declare i8* @rumStrset(i8*, i32, i8)

declare i8* @rumStrcat(i8*, i8*)

declare i8* @rumStrmake(i32, i8)
