; ModuleID = 'prog.expr'
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
  %11 = alloca i32*
  %12 = call i8* @malloc(i32 3)
  %13 = bitcast i8* %12 to i32*
  store i32* %13, i32** %11
  %14 = load i32*, i32** %11
  %15 = getelementptr inbounds i32, i32* %14, i32 0
  store i32 3, i32* %15
  %16 = load i32*, i32** %11
  %17 = getelementptr inbounds i32, i32* %16, i32 1
  store i32 4, i32* %17
  %18 = load i32*, i32** %11
  %19 = getelementptr inbounds i32, i32* %18, i32 2
  store i32 5, i32* %19
  %20 = alloca { i32*, i32 }
  %21 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %20, i32 0, i32 1
  store i32 3, i32* %21
  %22 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %20, i32 0, i32 0
  store i32* %13, i32** %22
  %23 = load { i32*, i32 }, { i32*, i32 }* %20
  %24 = alloca { i32*, i32 }*
  %25 = call i8* @malloc(i32 2)
  %26 = bitcast i8* %25 to { i32*, i32 }*
  store { i32*, i32 }* %26, { i32*, i32 }** %24
  %27 = load { i32*, i32 }*, { i32*, i32 }** %24
  %28 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %27, i32 0
  store { i32*, i32 } %10, { i32*, i32 }* %28
  %29 = load { i32*, i32 }*, { i32*, i32 }** %24
  %30 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %29, i32 1
  store { i32*, i32 } %23, { i32*, i32 }* %30
  %31 = alloca { { i32*, i32 }*, i32 }
  %32 = getelementptr inbounds { { i32*, i32 }*, i32 }, { { i32*, i32 }*, i32 }* %31, i32 0, i32 1
  store i32 2, i32* %32
  %33 = getelementptr inbounds { { i32*, i32 }*, i32 }, { { i32*, i32 }*, i32 }* %31, i32 0, i32 0
  store { i32*, i32 }* %26, { i32*, i32 }** %33
  %34 = load { { i32*, i32 }*, i32 }, { { i32*, i32 }*, i32 }* %31
  %35 = alloca { { i32*, i32 }*, i32 }
  store { { i32*, i32 }*, i32 } %34, { { i32*, i32 }*, i32 }* %35
  %36 = getelementptr inbounds { { i32*, i32 }*, i32 }, { { i32*, i32 }*, i32 }* %35, i32 0, i32 0
  %37 = load { i32*, i32 }*, { i32*, i32 }** %36
  %38 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %37, i32 0
  %39 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %38, i32 0, i32 0
  %40 = load i32*, i32** %39
  %41 = getelementptr inbounds i32, i32* %40, i32 1
  %42 = call i32 @rumWrite(i32* %41)
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
