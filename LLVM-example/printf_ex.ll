; ModuleID = 'printf_ex.c'
source_filename = "printf_ex.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.MyArr = type { i32*, i32 }

@.str = private unnamed_addr constant [12 x i8] c"arr[0]: %d\0A\00", align 1
@.str.1 = private unnamed_addr constant [12 x i8] c"arr[1]: %d\0A\00", align 1
@.str.2 = private unnamed_addr constant [12 x i8] c"arr[2]: %d\0A\00", align 1
@.str.3 = private unnamed_addr constant [12 x i8] c"arr[3]: %d\0A\00", align 1

; Function Attrs: noinline nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32*, align 8
  %3 = alloca %struct.MyArr, align 8
  store i32 0, i32* %1, align 4
  %4 = call noalias i8* @malloc(i64 3) #3
  %5 = bitcast i8* %4 to i32*
  store i32* %5, i32** %2, align 8
  %6 = load i32*, i32** %2, align 8
  %7 = getelementptr inbounds i32, i32* %6, i64 0
  store i32 1, i32* %7, align 4
  %8 = load i32*, i32** %2, align 8
  %9 = getelementptr inbounds i32, i32* %8, i64 1
  store i32 2, i32* %9, align 4
  %10 = load i32*, i32** %2, align 8
  %11 = getelementptr inbounds i32, i32* %10, i64 2
  store i32 3, i32* %11, align 4
  %12 = getelementptr inbounds %struct.MyArr, %struct.MyArr* %3, i32 0, i32 1
  store i32 3, i32* %12, align 8
  %13 = load i32*, i32** %2, align 8
  %14 = getelementptr inbounds %struct.MyArr, %struct.MyArr* %3, i32 0, i32 0
  store i32* %13, i32** %14, align 8
  %15 = getelementptr inbounds %struct.MyArr, %struct.MyArr* %3, i32 0, i32 0
  %16 = load i32*, i32** %15, align 8
  %17 = getelementptr inbounds i32, i32* %16, i64 0
  %18 = load i32, i32* %17, align 4
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str, i32 0, i32 0), i32 %18)
  %20 = getelementptr inbounds %struct.MyArr, %struct.MyArr* %3, i32 0, i32 0
  %21 = load i32*, i32** %20, align 8
  %22 = getelementptr inbounds i32, i32* %21, i64 1
  %23 = load i32, i32* %22, align 4
  %24 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str.1, i32 0, i32 0), i32 %23)
  %25 = getelementptr inbounds %struct.MyArr, %struct.MyArr* %3, i32 0, i32 0
  %26 = load i32*, i32** %25, align 8
  %27 = getelementptr inbounds i32, i32* %26, i64 2
  %28 = load i32, i32* %27, align 4
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str.2, i32 0, i32 0), i32 %28)
  %30 = getelementptr inbounds %struct.MyArr, %struct.MyArr* %3, i32 0, i32 0
  %31 = load i32*, i32** %30, align 8
  %32 = getelementptr inbounds i32, i32* %31, i64 3
  %33 = load i32, i32* %32, align 4
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str.3, i32 0, i32 0), i32 %33)
  ret i32 0
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1

declare i32 @printf(i8*, ...) #2

attributes #0 = { noinline nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 4.0.1-svn305187-1~exp1 (branches/release_40)"}
