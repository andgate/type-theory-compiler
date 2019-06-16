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

define i32 @derefInt(i32* %a) {
entry:
  %0 = alloca i32*, align 4
  store i32* %a, i32** %0, align 4
  %1 = load i32*, i32** %0, align 4
  %2 = load i32, i32* %1, align 4
  %3 = alloca i32, align 4
  store i32 %2, i32* %3, align 4
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
  %0 = alloca i32, align 4
  store i32 5, i32* %0, align 4
  %1 = load i32, i32* %0, align 4
  %2 = alloca i8*, align 4
  %3 = call i8* @malloc(i32 %1)
  store i8* %3, i8** %2, align 4
  %4 = bitcast i8** %2 to %MaybeInt**
  %5 = load %MaybeInt*, %MaybeInt** %4, align 4
  %6 = getelementptr %MaybeInt, %MaybeInt* %5, i32 0, i32 0
  store i8 1, i8* %6, align 4
  %7 = bitcast %MaybeInt* %5 to %MaybeInt_Just*
  %8 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %7, i32 0, i32 1
  %9 = alloca i32, align 4
  store i32 5, i32* %9, align 4
  %10 = load i32, i32* %9, align 4
  store i32 %10, i32* %8, align 4
  %11 = load %MaybeInt*, %MaybeInt** %4, align 4
  ret %MaybeInt* %11
}

define i32 @dot(%IVector3 %v1, %IVector3 %v2) {
entry:
  %0 = alloca %IVector3, align 4
  store %IVector3 %v1, %IVector3* %0, align 4
  %1 = alloca %IVector3, align 4
  store %IVector3 %v2, %IVector3* %1, align 4
  %2 = alloca i32, align 4
  %3 = bitcast %IVector3* %0 to %IVector3_V3*
  %4 = getelementptr %IVector3_V3, %IVector3_V3* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 4
  store i32 %5, i32* %2, align 4
  %6 = alloca i32, align 4
  %7 = bitcast %IVector3* %0 to %IVector3_V3*
  %8 = getelementptr %IVector3_V3, %IVector3_V3* %7, i32 0, i32 2
  %9 = load i32, i32* %8, align 4
  store i32 %9, i32* %6, align 4
  %10 = alloca i32, align 4
  %11 = bitcast %IVector3* %0 to %IVector3_V3*
  %12 = getelementptr %IVector3_V3, %IVector3_V3* %11, i32 0, i32 3
  %13 = load i32, i32* %12, align 4
  store i32 %13, i32* %10, align 4
  %14 = alloca i32, align 4
  %15 = bitcast %IVector3* %1 to %IVector3_V3*
  %16 = getelementptr %IVector3_V3, %IVector3_V3* %15, i32 0, i32 1
  %17 = load i32, i32* %16, align 4
  store i32 %17, i32* %14, align 4
  %18 = alloca i32, align 4
  %19 = bitcast %IVector3* %1 to %IVector3_V3*
  %20 = getelementptr %IVector3_V3, %IVector3_V3* %19, i32 0, i32 2
  %21 = load i32, i32* %20, align 4
  store i32 %21, i32* %18, align 4
  %22 = alloca i32, align 4
  %23 = bitcast %IVector3* %1 to %IVector3_V3*
  %24 = getelementptr %IVector3_V3, %IVector3_V3* %23, i32 0, i32 3
  %25 = load i32, i32* %24, align 4
  store i32 %25, i32* %22, align 4
  %26 = alloca i32, align 4
  %27 = alloca i32, align 4
  %28 = load i32, i32* %2, align 4
  %29 = load i32, i32* %14, align 4
  %30 = mul i32 %28, %29
  store i32 %30, i32* %27, align 4
  %31 = load i32, i32* %27, align 4
  store i32 %31, i32* %26, align 4
  %32 = alloca i32, align 4
  %33 = alloca i32, align 4
  %34 = load i32, i32* %6, align 4
  %35 = load i32, i32* %18, align 4
  %36 = mul i32 %34, %35
  store i32 %36, i32* %33, align 4
  %37 = load i32, i32* %33, align 4
  store i32 %37, i32* %32, align 4
  %38 = alloca i32, align 4
  %39 = alloca i32, align 4
  %40 = load i32, i32* %10, align 4
  %41 = load i32, i32* %22, align 4
  %42 = mul i32 %40, %41
  store i32 %42, i32* %39, align 4
  %43 = load i32, i32* %39, align 4
  store i32 %43, i32* %38, align 4
  %44 = alloca i32, align 4
  %45 = alloca i32, align 4
  %46 = load i32, i32* %26, align 4
  %47 = load i32, i32* %32, align 4
  %48 = add i32 %46, %47
  store i32 %48, i32* %45, align 4
  %49 = load i32, i32* %45, align 4
  store i32 %49, i32* %44, align 4
  %50 = alloca i32, align 4
  %51 = alloca i32, align 4
  %52 = load i32, i32* %38, align 4
  %53 = load i32, i32* %44, align 4
  %54 = add i32 %52, %53
  store i32 %54, i32* %51, align 4
  %55 = load i32, i32* %51, align 4
  store i32 %55, i32* %50, align 4
  %56 = load i32, i32* %50, align 4
  ret i32 %56
}

define i32 @exMaybe(%MaybeInt %may_x) {
entry:
  %0 = alloca %MaybeInt, align 4
  store %MaybeInt %may_x, %MaybeInt* %0, align 4
  %1 = getelementptr %MaybeInt, %MaybeInt* %0, i32 0, i32 0
  %2 = load i8, i8* %1, align 4
  %3 = alloca i32, align 4
  switch i8 %2, label %case_failure [
    i8 1, label %case
    i8 0, label %case1
  ]

case:                                             ; preds = %entry
  %4 = bitcast %MaybeInt* %0 to %MaybeInt_Just*
  %5 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %4, i32 0, i32 1
  %6 = load i32, i32* %5, align 4
  store i32 %6, i32* %3, align 4
  br label %switch_exit

case1:                                            ; preds = %entry
  %7 = bitcast %MaybeInt* %0 to %MaybeInt_Nothing*
  %8 = alloca i32, align 4
  store i32 0, i32* %8, align 4
  %9 = load i32, i32* %8, align 4
  store i32 %9, i32* %3, align 4
  br label %switch_exit

case_failure:                                     ; preds = %entry
  br label %switch_exit

switch_exit:                                      ; preds = %case_failure, %case1, %case
  %10 = load i32, i32* %3, align 4
  ret i32 %10
}

define i32 @add(i32 %a, i32 %b) {
entry:
  %0 = alloca i32, align 4
  store i32 %a, i32* %0, align 4
  %1 = alloca i32, align 4
  store i32 %b, i32* %1, align 4
  %2 = alloca i32, align 4
  %3 = load i32, i32* %0, align 4
  %4 = load i32, i32* %1, align 4
  %5 = add i32 %3, %4
  store i32 %5, i32* %2, align 4
  %6 = load i32, i32* %2, align 4
  ret i32 %6
}

define i32 @mul(i32 %a, i32 %b) {
entry:
  %0 = alloca i32, align 4
  store i32 %a, i32* %0, align 4
  %1 = alloca i32, align 4
  store i32 %b, i32* %1, align 4
  %2 = alloca i32, align 4
  %3 = load i32, i32* %0, align 4
  %4 = load i32, i32* %1, align 4
  %5 = mul i32 %3, %4
  store i32 %5, i32* %2, align 4
  %6 = load i32, i32* %2, align 4
  ret i32 %6
}

define i32 @const(i32 %a, i32 %b) {
entry:
  %0 = alloca i32, align 4
  store i32 %a, i32* %0, align 4
  %1 = alloca i32, align 4
  store i32 %b, i32* %1, align 4
  %2 = load i32, i32* %0, align 4
  ret i32 %2
}

define i32 @id(i32 %x) {
entry:
  %0 = alloca i32, align 4
  store i32 %x, i32* %0, align 4
  %1 = load i32, i32* %0, align 4
  ret i32 %1
}

define %MaybeInt @idMaybe(%MaybeInt %x) {
entry:
  %0 = alloca %MaybeInt, align 4
  store %MaybeInt %x, %MaybeInt* %0, align 4
  %1 = load %MaybeInt, %MaybeInt* %0, align 4
  ret %MaybeInt %1
}

define i32 @addMul(i32 %a, i32 %b, i32 %c) {
entry:
  %0 = alloca i32, align 4
  store i32 %a, i32* %0, align 4
  %1 = alloca i32, align 4
  store i32 %b, i32* %1, align 4
  %2 = alloca i32, align 4
  store i32 %c, i32* %2, align 4
  %3 = alloca i32, align 4
  %4 = load i32, i32* %0, align 4
  %5 = load i32, i32* %1, align 4
  %6 = alloca i32, align 4
  %7 = call i32 @add(i32 %4, i32 %5)
  store i32 %7, i32* %6, align 4
  %8 = load i32, i32* %6, align 4
  store i32 %8, i32* %3, align 4
  %9 = alloca i32, align 4
  %10 = load i32, i32* %3, align 4
  %11 = load i32, i32* %2, align 4
  %12 = alloca i32, align 4
  %13 = call i32 @mul(i32 %10, i32 %11)
  store i32 %13, i32* %12, align 4
  %14 = load i32, i32* %12, align 4
  store i32 %14, i32* %9, align 4
  %15 = load i32, i32* %9, align 4
  %16 = alloca i32, align 4
  %17 = call i32 @id(i32 %15)
  store i32 %17, i32* %16, align 4
  %18 = load i32, i32* %16, align 4
  ret i32 %18
}

define %MaybeInt @maybeAddMul(%MaybeInt %may_a, %MaybeInt %may_b, %MaybeInt %may_c) {
entry:
  %0 = alloca %MaybeInt, align 4
  store %MaybeInt %may_a, %MaybeInt* %0, align 4
  %1 = alloca %MaybeInt, align 4
  store %MaybeInt %may_b, %MaybeInt* %1, align 4
  %2 = alloca %MaybeInt, align 4
  store %MaybeInt %may_c, %MaybeInt* %2, align 4
  %3 = getelementptr %MaybeInt, %MaybeInt* %0, i32 0, i32 0
  %4 = load i8, i8* %3, align 4
  %5 = alloca %MaybeInt, align 4
  switch i8 %4, label %case_failure2 [
    i8 0, label %case
    i8 1, label %case1
  ]

case:                                             ; preds = %entry
  %6 = bitcast %MaybeInt* %0 to %MaybeInt_Nothing*
  %7 = alloca %MaybeInt, align 4
  %8 = getelementptr %MaybeInt, %MaybeInt* %7, i32 0, i32 0
  store i8 0, i8* %8, align 4
  %9 = bitcast %MaybeInt* %7 to %MaybeInt_Nothing*
  %10 = load %MaybeInt, %MaybeInt* %7, align 4
  store %MaybeInt %10, %MaybeInt* %5, align 4
  br label %switch_exit2

case1:                                            ; preds = %entry
  %11 = bitcast %MaybeInt* %0 to %MaybeInt_Just*
  %12 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %11, i32 0, i32 1
  %13 = getelementptr %MaybeInt, %MaybeInt* %1, i32 0, i32 0
  %14 = load i8, i8* %13, align 4
  %15 = alloca %MaybeInt, align 4
  switch i8 %14, label %case_failure1 [
    i8 0, label %case2
    i8 1, label %case3
  ]

case2:                                            ; preds = %case1
  %16 = bitcast %MaybeInt* %1 to %MaybeInt_Nothing*
  %17 = alloca %MaybeInt, align 4
  %18 = getelementptr %MaybeInt, %MaybeInt* %17, i32 0, i32 0
  store i8 0, i8* %18, align 4
  %19 = bitcast %MaybeInt* %17 to %MaybeInt_Nothing*
  %20 = load %MaybeInt, %MaybeInt* %17, align 4
  store %MaybeInt %20, %MaybeInt* %15, align 4
  br label %switch_exit1

case3:                                            ; preds = %case1
  %21 = bitcast %MaybeInt* %1 to %MaybeInt_Just*
  %22 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %21, i32 0, i32 1
  %23 = getelementptr %MaybeInt, %MaybeInt* %2, i32 0, i32 0
  %24 = load i8, i8* %23, align 4
  %25 = alloca %MaybeInt, align 4
  switch i8 %24, label %case_failure [
    i8 0, label %case4
    i8 1, label %case5
  ]

case4:                                            ; preds = %case3
  %26 = bitcast %MaybeInt* %2 to %MaybeInt_Nothing*
  %27 = alloca %MaybeInt, align 4
  %28 = getelementptr %MaybeInt, %MaybeInt* %27, i32 0, i32 0
  store i8 0, i8* %28, align 4
  %29 = bitcast %MaybeInt* %27 to %MaybeInt_Nothing*
  %30 = load %MaybeInt, %MaybeInt* %27, align 4
  store %MaybeInt %30, %MaybeInt* %25, align 4
  br label %switch_exit

case5:                                            ; preds = %case3
  %31 = bitcast %MaybeInt* %2 to %MaybeInt_Just*
  %32 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %31, i32 0, i32 1
  %33 = alloca i32, align 4
  %34 = load i32, i32* %12, align 4
  %35 = load i32, i32* %22, align 4
  %36 = load i32, i32* %32, align 4
  %37 = alloca i32, align 4
  %38 = call i32 @addMul(i32 %34, i32 %35, i32 %36)
  store i32 %38, i32* %37, align 4
  %39 = load i32, i32* %37, align 4
  store i32 %39, i32* %33, align 4
  %40 = alloca %MaybeInt, align 4
  %41 = getelementptr %MaybeInt, %MaybeInt* %40, i32 0, i32 0
  store i8 1, i8* %41, align 4
  %42 = bitcast %MaybeInt* %40 to %MaybeInt_Just*
  %43 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %42, i32 0, i32 1
  %44 = load i32, i32* %33, align 4
  store i32 %44, i32* %43, align 4
  %45 = load %MaybeInt, %MaybeInt* %40, align 4
  store %MaybeInt %45, %MaybeInt* %25, align 4
  br label %switch_exit

case_failure:                                     ; preds = %case3
  br label %switch_exit

switch_exit:                                      ; preds = %case_failure, %case5, %case4
  %46 = load %MaybeInt, %MaybeInt* %25, align 4
  store %MaybeInt %46, %MaybeInt* %15, align 4
  br label %switch_exit1

case_failure1:                                    ; preds = %case1
  br label %switch_exit1

switch_exit1:                                     ; preds = %case_failure1, %switch_exit, %case2
  %47 = load %MaybeInt, %MaybeInt* %15, align 4
  store %MaybeInt %47, %MaybeInt* %5, align 4
  br label %switch_exit2

case_failure2:                                    ; preds = %entry
  br label %switch_exit2

switch_exit2:                                     ; preds = %case_failure2, %switch_exit1, %case
  %48 = load %MaybeInt, %MaybeInt* %5, align 4
  ret %MaybeInt %48
}

define i32 @main(i32 %argc, i8** %argv) {
entry:
  %0 = alloca i32, align 4
  store i32 %argc, i32* %0, align 4
  %1 = alloca i8**, align 4
  store i8** %argv, i8*** %1, align 4
  %2 = alloca i8*, align 4
  %3 = getelementptr inbounds [12 x i8], [12 x i8]* @gstring, i32 0, i32 0
  %4 = bitcast i8* %3 to i8*
  %5 = alloca i8*, align 4
  store i8* %3, i8** %5, align 4
  %6 = load i8*, i8** %5, align 4
  store i8* %6, i8** %2, align 4
  %7 = alloca i8*, align 4
  %8 = getelementptr inbounds [2 x i8], [2 x i8]* @gstring1, i32 0, i32 0
  %9 = bitcast i8* %8 to i8*
  %10 = alloca i8*, align 4
  store i8* %8, i8** %10, align 4
  %11 = load i8*, i8** %10, align 4
  store i8* %11, i8** %7, align 4
  %12 = alloca %MaybeInt*, align 4
  %13 = alloca %MaybeInt*, align 4
  %14 = call %MaybeInt* @just5()
  store %MaybeInt* %14, %MaybeInt** %13, align 4
  %15 = load %MaybeInt*, %MaybeInt** %13, align 4
  store %MaybeInt* %15, %MaybeInt** %12, align 4
  %16 = alloca %MaybeInt, align 4
  %17 = load %MaybeInt*, %MaybeInt** %12, align 4
  %18 = load %MaybeInt, %MaybeInt* %17, align 4
  %19 = alloca %MaybeInt, align 4
  store %MaybeInt %18, %MaybeInt* %19, align 4
  %20 = load %MaybeInt, %MaybeInt* %19, align 4
  store %MaybeInt %20, %MaybeInt* %16, align 4
  %21 = alloca %MaybeInt, align 4
  %22 = alloca %MaybeInt, align 4
  %23 = call %MaybeInt @nothing()
  store %MaybeInt %23, %MaybeInt* %22, align 4
  %24 = load %MaybeInt, %MaybeInt* %22, align 4
  store %MaybeInt %24, %MaybeInt* %21, align 4
  %25 = getelementptr %MaybeInt, %MaybeInt* %16, i32 0, i32 0
  %26 = load i8, i8* %25, align 4
  %27 = alloca i32, align 4
  switch i8 %26, label %case_failure [
    i8 0, label %case
    i8 1, label %case1
  ]

case:                                             ; preds = %entry
  %28 = bitcast %MaybeInt* %16 to %MaybeInt_Nothing*
  %29 = load i8*, i8** %2, align 4
  %30 = alloca i32, align 4
  %31 = call i32 @puts(i8* %29)
  store i32 %31, i32* %30, align 4
  %32 = load i32, i32* %30, align 4
  store i32 %32, i32* %27, align 4
  br label %switch_exit

case1:                                            ; preds = %entry
  %33 = bitcast %MaybeInt* %16 to %MaybeInt_Just*
  %34 = getelementptr %MaybeInt_Just, %MaybeInt_Just* %33, i32 0, i32 1
  %35 = load i8*, i8** %7, align 4
  %36 = alloca i32, align 4
  %37 = call i32 @puts(i8* %35)
  store i32 %37, i32* %36, align 4
  %38 = load i32, i32* %36, align 4
  store i32 %38, i32* %27, align 4
  br label %switch_exit

case_failure:                                     ; preds = %entry
  br label %switch_exit

switch_exit:                                      ; preds = %case_failure, %case1, %case
  %39 = load i32, i32* %27, align 4
  ret i32 %39
}
