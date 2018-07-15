; ModuleID = 'my first module'

@.scanf_str  = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.printf_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define void @main() {
entry:
  %0 = alloca { i32*, i32 }

  %1 = call noalias i8* @malloc(i32 10) #3
  %2 = bitcast i8* %1 to i32*
  %3 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %0, i32 0, i32 0
  store i32* %2, i32** %3
  %6 = getelementptr inbounds %struct.MyArr, %struct.MyArr* %2, i32 0, i32 1
    store i32 10, i32* %6, align 8

  %x = alloca i32
  store i32 0, i32* %x
  %0 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.scanf_str, i32 0, i32 0), i32* %x)
  %x0 = load i32, i32* %x
  %1 = add i32 %x0, 10
  store i32 %1, i32* %x
  %x1 = load i32, i32* %x
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.printf_str, i32 0, i32 0), i32 %x1)
  ret void
}

declare i32 @scanf(i8*, ...)
declare i32 @printf(i8*, ...)
