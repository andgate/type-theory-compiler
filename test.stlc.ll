; ModuleID = 'exampleModule'
source_filename = "<string>"

%MaybeInt = type { i8, [4 x i8] }
%MaybeInt_Nothing = type { i8 }
%MaybeInt_Just = type { i8, i32 }
%IVector3 = type { i8, [12 x i8] }
%IVector3_V3 = type { i8, i32, i32, i32 }

@gstring = unnamed_addr constant [12 x i8] c"Hello World\00"
@gstring1 = unnamed_addr constant [2 x i8] c"5\00"

declare i8* @malloc(i32)

declare void @free(i8*)

declare i8* @memcpy(i8*, i8*, i32)

declare i32 @puts(i8*)

define i32 @derefInt(i32*) {
entry:
  %1 = alloca i32*, align 4
  store i32* %0, i32** %1, align 4
  %2 = load i32*, i32** %1, align 4
  %3 = load i32*, i32** %1, align 4
  %4 = load i32, i32* %3, align 4
  ret i32 %4
}

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

define i32 @dot(%IVector3, %IVector3) {
entry:
  %2 = alloca %IVector3, align 4
  store %IVector3 %0, %IVector3* %2, align 4
  %3 = alloca %IVector3, align 4
  store %IVector3 %1, %IVector3* %3, align 4
  %4 = load %IVector3, %IVector3* %2, align 4
  %5 = load %IVector3, %IVector3* %3, align 4
  %6 = bitcast %IVector3* %2 to %IVector3_V3*
  %7 = getelementptr %IVector3_V3, %IVector3_V3* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 4
  %9 = bitcast %IVector3* %2 to %IVector3_V3*
  %10 = getelementptr %IVector3_V3, %IVector3_V3* %9, i32 0, i32 2
  %11 = load i32, i32* %10, align 4
  %12 = bitcast %IVector3* %2 to %IVector3_V3*
  %13 = getelementptr %IVector3_V3, %IVector3_V3* %12, i32 0, i32 3
  %14 = load i32, i32* %13, align 4
  %15 = bitcast %IVector3* %3 to %IVector3_V3*
  %16 = getelementptr %IVector3_V3, %IVector3_V3* %15, i32 0, i32 1
  %17 = load i32, i32* %16, align 4
  %18 = bitcast %IVector3* %3 to %IVector3_V3*
  %19 = getelementptr %IVector3_V3, %IVector3_V3* %18, i32 0, i32 2
  %20 = load i32, i32* %19, align 4
  %21 = bitcast %IVector3* %3 to %IVector3_V3*
  %22 = getelementptr %IVector3_V3, %IVector3_V3* %21, i32 0, i32 3
  %23 = load i32, i32* %22, align 4
  %24 = load i32, i32* %7, align 4
  %25 = load i32, i32* %16, align 4
  %26 = alloca i32, align 4
  %27 = mul i32 %24, %25
  store i32 %27, i32* %26, align 4
  %28 = load i32, i32* %26, align 4
  %29 = load i32, i32* %10, align 4
  %30 = load i32, i32* %19, align 4
  %31 = alloca i32, align 4
  %32 = mul i32 %29, %30
  store i32 %32, i32* %31, align 4
  %33 = load i32, i32* %31, align 4
  %34 = load i32, i32* %13, align 4
  %35 = load i32, i32* %22, align 4
  %36 = alloca i32, align 4
  %37 = mul i32 %34, %35
  store i32 %37, i32* %36, align 4
  %38 = load i32, i32* %36, align 4
  %39 = load i32, i32* %26, align 4
  %40 = load i32, i32* %31, align 4
  %41 = alloca i32, align 4
  %42 = add i32 %39, %40
  store i32 %42, i32* %41, align 4
  %43 = load i32, i32* %41, align 4
  %44 = load i32, i32* %36, align 4
  %45 = load i32, i32* %41, align 4
  %46 = alloca i32, align 4
  %47 = add i32 %44, %45
  store i32 %47, i32* %46, align 4
  %48 = load i32, i32* %46, align 4
  %49 = load i32, i32* %46, align 4
  ret i32 %49
}

define i32 @exMaybe(%MaybeInt) {
entry:
  %1 = alloca %MaybeInt, align 4
  store %MaybeInt %0, %MaybeInt* %1, align 4
  %2 = load %MaybeInt, %MaybeInt* %1, align 4
  %3 = getelementptr %MaybeInt, %MaybeInt* %1, i32 0, i32 0
  %4 = load i8, i8* %3, align 4
  %5 = alloca i32, align 4
  switch i8 %4, label %case_failure [
    i8 1, label %case
    i8 0, label %case1
  ]

case:                                             ; preds = %entry
  %6 = bitcast %MaybeInt* %1 to %MaybeInt_Just*
  %7 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 4
  %9 = load i32, i32* %7, align 4
  store i32 %9, i32* %5, align 4
  br label %switch_exit

case1:                                            ; preds = %entry
  %10 = bitcast %MaybeInt* %1 to %MaybeInt_Nothing*
  %11 = alloca i32, align 4
  store i32 0, i32* %11, align 4
  %12 = load i32, i32* %11, align 4
  store i32 %12, i32* %5, align 4
  br label %switch_exit

case_failure:                                     ; preds = %entry
  br label %switch_exit

switch_exit:                                      ; preds = %case_failure, %case1, %case
  %13 = load i32, i32* %5, align 4
  ret i32 %13
}

define i32 @add(i32, i32) {
entry:
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = alloca i32, align 4
  store i32 %1, i32* %3, align 4
  %4 = load i32, i32* %2, align 4
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %2, align 4
  %7 = load i32, i32* %3, align 4
  %8 = alloca i32, align 4
  %9 = add i32 %6, %7
  store i32 %9, i32* %8, align 4
  %10 = load i32, i32* %8, align 4
  ret i32 %10
}

define i32 @mul(i32, i32) {
entry:
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = alloca i32, align 4
  store i32 %1, i32* %3, align 4
  %4 = load i32, i32* %2, align 4
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %2, align 4
  %7 = load i32, i32* %3, align 4
  %8 = alloca i32, align 4
  %9 = mul i32 %6, %7
  store i32 %9, i32* %8, align 4
  %10 = load i32, i32* %8, align 4
  ret i32 %10
}

define i32 @const(i32, i32) {
entry:
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = alloca i32, align 4
  store i32 %1, i32* %3, align 4
  %4 = load i32, i32* %2, align 4
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %2, align 4
  ret i32 %6
}

define i32 @id(i32) {
entry:
  %1 = alloca i32, align 4
  store i32 %0, i32* %1, align 4
  %2 = load i32, i32* %1, align 4
  %3 = load i32, i32* %1, align 4
  ret i32 %3
}

define %MaybeInt @idMaybe(%MaybeInt) {
entry:
  %1 = alloca %MaybeInt, align 4
  store %MaybeInt %0, %MaybeInt* %1, align 4
  %2 = load %MaybeInt, %MaybeInt* %1, align 4
  %3 = load %MaybeInt, %MaybeInt* %1, align 4
  ret %MaybeInt %3
}

define i32 @addMul(i32, i32, i32) {
entry:
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = alloca i32, align 4
  store i32 %1, i32* %4, align 4
  %5 = alloca i32, align 4
  store i32 %2, i32* %5, align 4
  %6 = load i32, i32* %3, align 4
  %7 = load i32, i32* %4, align 4
  %8 = load i32, i32* %5, align 4
  %9 = load i32, i32* %3, align 4
  %10 = load i32, i32* %4, align 4
  %11 = alloca i32, align 4
  %12 = add i32 %9, %10
  store i32 %12, i32* %11, align 4
  %13 = load i32, i32* %11, align 4
  %14 = load i32, i32* %11, align 4
  %15 = load i32, i32* %5, align 4
  %16 = alloca i32, align 4
  %17 = mul i32 %14, %15
  store i32 %17, i32* %16, align 4
  %18 = load i32, i32* %16, align 4
  %19 = load i32, i32* %16, align 4
  %20 = call i32 @id(i32 %19)
  %21 = alloca i32, align 4
  store i32 %20, i32* %21, align 4
  %22 = load i32, i32* %21, align 4
  ret i32 %22
}

define %MaybeInt @maybeAddMul(%MaybeInt, %MaybeInt, %MaybeInt) {
entry:
  %3 = alloca %MaybeInt, align 4
  store %MaybeInt %0, %MaybeInt* %3, align 4
  %4 = alloca %MaybeInt, align 4
  store %MaybeInt %1, %MaybeInt* %4, align 4
  %5 = alloca %MaybeInt, align 4
  store %MaybeInt %2, %MaybeInt* %5, align 4
  %6 = load %MaybeInt, %MaybeInt* %3, align 4
  %7 = load %MaybeInt, %MaybeInt* %4, align 4
  %8 = load %MaybeInt, %MaybeInt* %5, align 4
  %9 = getelementptr %MaybeInt, %MaybeInt* %3, i32 0, i32 0
  %10 = load i8, i8* %9, align 4
  %11 = alloca %MaybeInt, align 4
  switch i8 %10, label %case_failure2 [
    i8 0, label %case
    i8 1, label %case1
  ]

case:                                             ; preds = %entry
  %12 = bitcast %MaybeInt* %3 to %MaybeInt_Nothing*
  %13 = alloca %MaybeInt, align 4
  %14 = getelementptr %MaybeInt, %MaybeInt* %13, i32 0, i32 0
  store i8 0, i8* %14, align 4
  %15 = bitcast %MaybeInt* %13 to %MaybeInt_Nothing*
  %16 = load %MaybeInt, %MaybeInt* %13, align 4
  store %MaybeInt %16, %MaybeInt* %11, align 4
  br label %switch_exit2

case1:                                            ; preds = %entry
  %17 = bitcast %MaybeInt* %3 to %MaybeInt_Just*
  %18 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %17, i32 0, i32 1
  %19 = load i32, i32* %18, align 4
  %20 = getelementptr %MaybeInt, %MaybeInt* %4, i32 0, i32 0
  %21 = load i8, i8* %20, align 4
  %22 = alloca %MaybeInt, align 4
  switch i8 %21, label %case_failure1 [
    i8 0, label %case2
    i8 1, label %case3
  ]

case2:                                            ; preds = %case1
  %23 = bitcast %MaybeInt* %4 to %MaybeInt_Nothing*
  %24 = alloca %MaybeInt, align 4
  %25 = getelementptr %MaybeInt, %MaybeInt* %24, i32 0, i32 0
  store i8 0, i8* %25, align 4
  %26 = bitcast %MaybeInt* %24 to %MaybeInt_Nothing*
  %27 = load %MaybeInt, %MaybeInt* %24, align 4
  store %MaybeInt %27, %MaybeInt* %22, align 4
  br label %switch_exit1

case3:                                            ; preds = %case1
  %28 = bitcast %MaybeInt* %4 to %MaybeInt_Just*
  %29 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %28, i32 0, i32 1
  %30 = load i32, i32* %29, align 4
  %31 = getelementptr %MaybeInt, %MaybeInt* %5, i32 0, i32 0
  %32 = load i8, i8* %31, align 4
  %33 = alloca %MaybeInt, align 4
  switch i8 %32, label %case_failure [
    i8 0, label %case4
    i8 1, label %case5
  ]

case4:                                            ; preds = %case3
  %34 = bitcast %MaybeInt* %5 to %MaybeInt_Nothing*
  %35 = alloca %MaybeInt, align 4
  %36 = getelementptr %MaybeInt, %MaybeInt* %35, i32 0, i32 0
  store i8 0, i8* %36, align 4
  %37 = bitcast %MaybeInt* %35 to %MaybeInt_Nothing*
  %38 = load %MaybeInt, %MaybeInt* %35, align 4
  store %MaybeInt %38, %MaybeInt* %33, align 4
  br label %switch_exit

case5:                                            ; preds = %case3
  %39 = bitcast %MaybeInt* %5 to %MaybeInt_Just*
  %40 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %39, i32 0, i32 1
  %41 = load i32, i32* %40, align 4
  %42 = load i32, i32* %18, align 4
  %43 = load i32, i32* %29, align 4
  %44 = load i32, i32* %40, align 4
  %45 = call i32 @addMul(i32 %42, i32 %43, i32 %44)
  %46 = alloca i32, align 4
  store i32 %45, i32* %46, align 4
  %47 = load i32, i32* %46, align 4
  %48 = alloca %MaybeInt, align 4
  %49 = getelementptr %MaybeInt, %MaybeInt* %48, i32 0, i32 0
  store i8 1, i8* %49, align 4
  %50 = bitcast %MaybeInt* %48 to %MaybeInt_Just*
  %51 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %50, i32 0, i32 1
  %52 = load i32, i32* %46, align 4
  store i32 %52, i32* %51, align 4
  %53 = load %MaybeInt, %MaybeInt* %48, align 4
  store %MaybeInt %53, %MaybeInt* %33, align 4
  br label %switch_exit

case_failure:                                     ; preds = %case3
  br label %switch_exit

switch_exit:                                      ; preds = %case_failure, %case5, %case4
  %54 = load %MaybeInt, %MaybeInt* %33, align 4
  store %MaybeInt %54, %MaybeInt* %22, align 4
  br label %switch_exit1

case_failure1:                                    ; preds = %case1
  br label %switch_exit1

switch_exit1:                                     ; preds = %case_failure1, %switch_exit, %case2
  %55 = load %MaybeInt, %MaybeInt* %22, align 4
  store %MaybeInt %55, %MaybeInt* %11, align 4
  br label %switch_exit2

case_failure2:                                    ; preds = %entry
  br label %switch_exit2

switch_exit2:                                     ; preds = %case_failure2, %switch_exit1, %case
  %56 = load %MaybeInt, %MaybeInt* %11, align 4
  ret %MaybeInt %56
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
  %22 = getelementptr %MaybeInt, %MaybeInt* %17, i32 0, i32 0
  %23 = load i8, i8* %22, align 4
  %24 = alloca i32, align 4
  switch i8 %23, label %case_failure [
    i8 0, label %case
    i8 1, label %case1
  ]

case:                                             ; preds = %entry
  %25 = bitcast %MaybeInt* %17 to %MaybeInt_Nothing*
  %26 = load i8*, i8** %8, align 4
  %27 = call i32 @puts(i8* %26)
  %28 = alloca i32, align 4
  store i32 %27, i32* %28, align 4
  %29 = load i32, i32* %28, align 4
  store i32 %29, i32* %24, align 4
  br label %switch_exit

case1:                                            ; preds = %entry
  %30 = bitcast %MaybeInt* %17 to %MaybeInt_Just*
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
