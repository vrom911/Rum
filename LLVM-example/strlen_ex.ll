; ModuleID = 'prog.expr'
source_filename = "<string>"

define i32 @main() {
main:
  %0 = alloca [5 x i8]
  store [5 x i8] c"strss", [5 x i8]* %0
  %1 = getelementptr inbounds [5 x i8], [5 x i8]* %0, i32 0, i32 0
  %2 = call i32 @rumStrlen(i8* %1)
  %3 = alloca i32
  store i32 %2, i32* %3
  %4 = load i32, i32* %3
  %5 = call i32 @rumWrite(i32 %4)
  ret i32 0
}

declare i32 @rumRead()

declare i32 @rumWrite(i32)

declare i32 @rumStrlen(i8*)
