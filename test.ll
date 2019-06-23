; ModuleID = 'exampleModule'
source_filename = "<string>"

%MaybeInt = type { i8, [4 x i8] }
%MaybeInt_Nothing = type { i8 }
%MaybeInt_Just = type { i8, i32 }

@gstring = unnamed_addr constant [12 x i8] c"Hello World\00"
@gstring1 = unnamed_addr constant [2 x i8] c"5\00"

declare i8* @malloc(i32)

declare void @free(i8*)

declare i8* @memcpy(i8*, i8*, i32)

declare i32 @puts(i8*)

define %MaybeInt @nothing() {
entry:
  %0 = alloca %MaybeInt, align 4
  %1 = getelementptr %MaybeInt, %MaybeInt* %0, i32 0, i32 0
  store i8 0, i8* %1, align 4
  %2 = bitcast %MaybeInt* %0 to %MaybeInt_Nothing*
  %3 = load %MaybeInt, %MaybeInt* %0, align 4
  ret %MaybeInt %3
}

define %MaybeInt* @just5() {
entry:
  %0 = call i8* @malloc(i32 5)
  %1 = alloca i8*, align 4
  store i8* %0, i8** %1, align 4
  %2 = bitcast i8** %1 to %MaybeInt**
  %3 = load %MaybeInt*, %MaybeInt** %2, align 4
  %4 = getelementptr %MaybeInt, %MaybeInt* %3, i32 0, i32 0
  store i8 1, i8* %4, align 4
  %5 = bitcast %MaybeInt* %3 to %MaybeInt_Just*
  %6 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %5, i32 0, i32 1
  %7 = alloca i32, align 4
  store i32 5, i32* %7, align 4
  %8 = load i32, i32* %7, align 4
  store i32 %8, i32* %6, align 4
  %9 = load %MaybeInt*, %MaybeInt** %2, align 4
  ret %MaybeInt* %9
}

define i32 @main(i32, i8**) {
entry:
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = alloca i8**, align 4
  store i8** %1, i8*** %3, align 4
  %4 = load i32, i32* %2, align 4
  %5 = load i8**, i8*** %3, align 4
  %6 = getelementptr inbounds [12 x i8], [12 x i8]* @gstring, i32 0, i32 0
  %7 = bitcast i8* %6 to i8*
  %8 = alloca i8*, align 4
  store i8* %7, i8** %8, align 4
  %9 = load i8*, i8** %8, align 4
  %10 = getelementptr inbounds [2 x i8], [2 x i8]* @gstring1, i32 0, i32 0
  %11 = bitcast i8* %10 to i8*
  %12 = alloca i8*, align 4
  store i8* %11, i8** %12, align 4
  %13 = load i8*, i8** %12, align 4
  %14 = alloca %MaybeInt*, align 4
  %15 = call %MaybeInt* @just5()
  store %MaybeInt* %15, %MaybeInt** %14, align 4
  %16 = load %MaybeInt*, %MaybeInt** %14, align 4
  %17 = load %MaybeInt*, %MaybeInt** %14, align 4
  %18 = load %MaybeInt, %MaybeInt* %17, align 4
  %19 = alloca %MaybeInt, align 4
  %20 = call %MaybeInt @nothing()
  store %MaybeInt %20, %MaybeInt* %19, align 4
  %21 = load %MaybeInt, %MaybeInt* %19, align 4
  %22 = getelementptr %MaybeInt, %MaybeInt* %19, i32 0, i32 0
  %23 = load i8, i8* %22, align 4
  %24 = alloca i32, align 4
  switch i8 %23, label %case_failure [
    i8 0, label %case
    i8 1, label %case1
  ]

case:                                             ; preds = %entry
  %25 = bitcast %MaybeInt* %19 to %MaybeInt_Nothing*
  %26 = load i8*, i8** %8, align 4
  %27 = call i32 @puts(i8* %26)
  %28 = alloca i32, align 4
  store i32 %27, i32* %28, align 4
  %29 = load i32, i32* %28, align 4
  store i32 %29, i32* %24, align 4
  br label %switch_exit

case1:                                            ; preds = %entry
  %30 = bitcast %MaybeInt* %19 to %MaybeInt_Just*
  %31 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %30, i32 0, i32 1
  %32 = load i32, i32* %31, align 4
  %33 = load i8*, i8** %12, align 4
  %34 = call i32 @puts(i8* %33)
  %35 = alloca i32, align 4
  store i32 %34, i32* %35, align 4
  %36 = load i32, i32* %35, align 4
  store i32 %36, i32* %24, align 4
  br label %switch_exit

case_failure:                                     ; preds = %entry
  br label %switch_exit

switch_exit:                                      ; preds = %case_failure, %case1, %case
  %37 = load i32, i32* %24, align 4
  ret i32 %37
}
