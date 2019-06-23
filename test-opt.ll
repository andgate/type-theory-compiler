; ModuleID = 'test.ll'
source_filename = "<string>"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%MaybeInt = type { i8, [4 x i8] }

@gstring = unnamed_addr constant [12 x i8] c"Hello World\00"
@gstring1 = unnamed_addr constant [2 x i8] c"5\00"

; Function Attrs: nounwind
declare noalias i8* @malloc(i32) local_unnamed_addr #0

; Function Attrs: nounwind
declare i32 @puts(i8* nocapture readonly) local_unnamed_addr #0

; Function Attrs: norecurse nounwind readnone
define %MaybeInt @nothing() local_unnamed_addr #1 {
entry:
  ret %MaybeInt { i8 0, [4 x i8] undef }
}

; Function Attrs: nounwind
define noalias %MaybeInt* @just5() local_unnamed_addr #0 {
entry:
  %0 = tail call i8* @malloc(i32 5)
  %1 = bitcast i8* %0 to %MaybeInt*
  store i8 1, i8* %0, align 4
  %2 = getelementptr i8, i8* %0, i64 4
  %3 = bitcast i8* %2 to i32*
  store i32 5, i32* %3, align 4
  ret %MaybeInt* %1
}

; Function Attrs: nounwind
define i32 @main(i32, i8** nocapture readnone) local_unnamed_addr #0 {
entry:
  %2 = tail call %MaybeInt* @just5()
  %3 = tail call i32 @puts(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @gstring, i64 0, i64 0))
  ret i32 %3
}

attributes #0 = { nounwind }
attributes #1 = { norecurse nounwind readnone }
