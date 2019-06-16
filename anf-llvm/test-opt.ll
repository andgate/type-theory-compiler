; ModuleID = 'test.ll'
source_filename = "<string>"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%MaybeInt = type { i8, [4 x i8] }
%IVector3 = type { i8, [12 x i8] }

@gstring = unnamed_addr constant [12 x i8] c"Hello World\00"
@gstring1 = unnamed_addr constant [2 x i8] c"5\00"

; Function Attrs: nounwind
declare noalias i8* @malloc(i32) local_unnamed_addr #0

; Function Attrs: nounwind
declare i32 @puts(i8* nocapture readonly) local_unnamed_addr #0

; Function Attrs: norecurse nounwind readonly
define i32 @derefInt(i32* nocapture readonly %a) local_unnamed_addr #1 {
entry:
  %0 = load i32, i32* %a, align 4
  ret i32 %0
}

; Function Attrs: norecurse nounwind readnone
define %MaybeInt @nothing() local_unnamed_addr #2 {
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

; Function Attrs: norecurse nounwind readnone
define i32 @dot(%IVector3 %v1, %IVector3 %v2) local_unnamed_addr #2 {
entry:
  %v1.fca.1.3.extract = extractvalue %IVector3 %v1, 1, 3
  %v1.fca.1.4.extract = extractvalue %IVector3 %v1, 1, 4
  %v1.fca.1.5.extract = extractvalue %IVector3 %v1, 1, 5
  %v1.fca.1.6.extract = extractvalue %IVector3 %v1, 1, 6
  %v1.fca.1.7.extract = extractvalue %IVector3 %v1, 1, 7
  %v1.fca.1.8.extract = extractvalue %IVector3 %v1, 1, 8
  %v1.fca.1.9.extract = extractvalue %IVector3 %v1, 1, 9
  %v1.fca.1.10.extract = extractvalue %IVector3 %v1, 1, 10
  %v1.fca.1.11.extract = extractvalue %IVector3 %v1, 1, 11
  %v2.fca.1.3.extract = extractvalue %IVector3 %v2, 1, 3
  %v2.fca.1.4.extract = extractvalue %IVector3 %v2, 1, 4
  %v2.fca.1.5.extract = extractvalue %IVector3 %v2, 1, 5
  %v2.fca.1.6.extract = extractvalue %IVector3 %v2, 1, 6
  %v2.fca.1.7.extract = extractvalue %IVector3 %v2, 1, 7
  %v2.fca.1.8.extract = extractvalue %IVector3 %v2, 1, 8
  %v2.fca.1.9.extract = extractvalue %IVector3 %v2, 1, 9
  %v2.fca.1.10.extract = extractvalue %IVector3 %v2, 1, 10
  %v2.fca.1.11.extract = extractvalue %IVector3 %v2, 1, 11
  %.sroa.823.4.insert.ext = zext i8 %v1.fca.1.6.extract to i32
  %.sroa.823.4.insert.shift = shl nuw i32 %.sroa.823.4.insert.ext, 24
  %.sroa.722.4.insert.ext = zext i8 %v1.fca.1.5.extract to i32
  %.sroa.722.4.insert.shift = shl nuw nsw i32 %.sroa.722.4.insert.ext, 16
  %.sroa.621.4.insert.ext = zext i8 %v1.fca.1.4.extract to i32
  %.sroa.621.4.insert.shift = shl nuw nsw i32 %.sroa.621.4.insert.ext, 8
  %.sroa.420.4.insert.ext = zext i8 %v1.fca.1.3.extract to i32
  %.sroa.722.4.insert.insert = or i32 %.sroa.722.4.insert.shift, %.sroa.420.4.insert.ext
  %.sroa.621.4.insert.insert = or i32 %.sroa.722.4.insert.insert, %.sroa.823.4.insert.shift
  %.sroa.420.4.insert.insert = or i32 %.sroa.621.4.insert.insert, %.sroa.621.4.insert.shift
  %.sroa.1327.8.insert.ext = zext i8 %v1.fca.1.10.extract to i32
  %.sroa.1327.8.insert.shift = shl nuw i32 %.sroa.1327.8.insert.ext, 24
  %.sroa.1226.8.insert.ext = zext i8 %v1.fca.1.9.extract to i32
  %.sroa.1226.8.insert.shift = shl nuw nsw i32 %.sroa.1226.8.insert.ext, 16
  %.sroa.1125.8.insert.ext = zext i8 %v1.fca.1.8.extract to i32
  %.sroa.1125.8.insert.shift = shl nuw nsw i32 %.sroa.1125.8.insert.ext, 8
  %.sroa.924.8.insert.ext = zext i8 %v1.fca.1.7.extract to i32
  %.sroa.1226.8.insert.insert = or i32 %.sroa.1226.8.insert.shift, %.sroa.924.8.insert.ext
  %.sroa.1125.8.insert.insert = or i32 %.sroa.1226.8.insert.insert, %.sroa.1327.8.insert.shift
  %.sroa.924.8.insert.insert = or i32 %.sroa.1125.8.insert.insert, %.sroa.1125.8.insert.shift
  %.sroa.1428.12.load.ext = zext i8 %v1.fca.1.11.extract to i32
  %.sroa.8.4.insert.ext = zext i8 %v2.fca.1.6.extract to i32
  %.sroa.8.4.insert.shift = shl nuw i32 %.sroa.8.4.insert.ext, 24
  %.sroa.7.4.insert.ext = zext i8 %v2.fca.1.5.extract to i32
  %.sroa.7.4.insert.shift = shl nuw nsw i32 %.sroa.7.4.insert.ext, 16
  %.sroa.6.4.insert.ext = zext i8 %v2.fca.1.4.extract to i32
  %.sroa.6.4.insert.shift = shl nuw nsw i32 %.sroa.6.4.insert.ext, 8
  %.sroa.4.4.insert.ext = zext i8 %v2.fca.1.3.extract to i32
  %.sroa.7.4.insert.insert = or i32 %.sroa.7.4.insert.shift, %.sroa.4.4.insert.ext
  %.sroa.6.4.insert.insert = or i32 %.sroa.7.4.insert.insert, %.sroa.8.4.insert.shift
  %.sroa.4.4.insert.insert = or i32 %.sroa.6.4.insert.insert, %.sroa.6.4.insert.shift
  %.sroa.13.8.insert.ext = zext i8 %v2.fca.1.10.extract to i32
  %.sroa.13.8.insert.shift = shl nuw i32 %.sroa.13.8.insert.ext, 24
  %.sroa.12.8.insert.ext = zext i8 %v2.fca.1.9.extract to i32
  %.sroa.12.8.insert.shift = shl nuw nsw i32 %.sroa.12.8.insert.ext, 16
  %.sroa.11.8.insert.ext = zext i8 %v2.fca.1.8.extract to i32
  %.sroa.11.8.insert.shift = shl nuw nsw i32 %.sroa.11.8.insert.ext, 8
  %.sroa.9.8.insert.ext = zext i8 %v2.fca.1.7.extract to i32
  %.sroa.12.8.insert.insert = or i32 %.sroa.12.8.insert.shift, %.sroa.9.8.insert.ext
  %.sroa.11.8.insert.insert = or i32 %.sroa.12.8.insert.insert, %.sroa.13.8.insert.shift
  %.sroa.9.8.insert.insert = or i32 %.sroa.11.8.insert.insert, %.sroa.11.8.insert.shift
  %.sroa.14.12.load.ext = zext i8 %v2.fca.1.11.extract to i32
  %0 = mul i32 %.sroa.4.4.insert.insert, %.sroa.420.4.insert.insert
  %1 = mul i32 %.sroa.9.8.insert.insert, %.sroa.924.8.insert.insert
  %2 = mul nuw nsw i32 %.sroa.14.12.load.ext, %.sroa.1428.12.load.ext
  %3 = add i32 %1, %2
  %4 = add i32 %3, %0
  ret i32 %4
}

; Function Attrs: norecurse nounwind readnone
define i32 @exMaybe(%MaybeInt %may_x) local_unnamed_addr #2 {
entry:
  %may_x.fca.0.extract = extractvalue %MaybeInt %may_x, 0
  %cond = icmp eq i8 %may_x.fca.0.extract, 1
  %may_x.fca.1.3.extract = extractvalue %MaybeInt %may_x, 1, 3
  %narrow = select i1 %cond, i8 %may_x.fca.1.3.extract, i8 0
  %.0 = zext i8 %narrow to i32
  ret i32 %.0
}

; Function Attrs: norecurse nounwind readnone
define i32 @add(i32 %a, i32 %b) local_unnamed_addr #2 {
entry:
  %0 = add i32 %b, %a
  ret i32 %0
}

; Function Attrs: norecurse nounwind readnone
define i32 @mul(i32 %a, i32 %b) local_unnamed_addr #2 {
entry:
  %0 = mul i32 %b, %a
  ret i32 %0
}

; Function Attrs: norecurse nounwind readnone
define i32 @const(i32 returned %a, i32 %b) local_unnamed_addr #2 {
entry:
  ret i32 %a
}

; Function Attrs: norecurse nounwind readnone
define i32 @id(i32 returned %x) local_unnamed_addr #2 {
entry:
  ret i32 %x
}

; Function Attrs: norecurse nounwind readnone
define %MaybeInt @idMaybe(%MaybeInt returned %x) local_unnamed_addr #2 {
entry:
  ret %MaybeInt %x
}

; Function Attrs: norecurse nounwind readnone
define i32 @addMul(i32 %a, i32 %b, i32 %c) local_unnamed_addr #2 {
entry:
  %0 = tail call i32 @add(i32 %a, i32 %b)
  %1 = tail call i32 @mul(i32 %0, i32 %c)
  %2 = tail call i32 @id(i32 %1)
  ret i32 %2
}

; Function Attrs: norecurse nounwind readnone
define %MaybeInt @maybeAddMul(%MaybeInt %may_a, %MaybeInt %may_b, %MaybeInt %may_c) local_unnamed_addr #2 {
entry:
  %may_a.fca.0.extract = extractvalue %MaybeInt %may_a, 0
  %may_a.fca.1.3.extract = extractvalue %MaybeInt %may_a, 1, 3
  %may_b.fca.1.3.extract = extractvalue %MaybeInt %may_b, 1, 3
  %may_c.fca.0.extract = extractvalue %MaybeInt %may_c, 0
  %may_c.fca.1.3.extract = extractvalue %MaybeInt %may_c, 1, 3
  switch i8 %may_a.fca.0.extract, label %switch_exit2 [
    i8 0, label %case
    i8 1, label %case1
  ]

case:                                             ; preds = %entry
  %0 = alloca %MaybeInt, align 8
  %1 = getelementptr inbounds %MaybeInt, %MaybeInt* %0, i64 0, i32 0
  store i8 0, i8* %1, align 8
  %.unpack120.elt = getelementptr inbounds %MaybeInt, %MaybeInt* %0, i64 0, i32 1, i64 0
  %.unpack120.unpack = load i8, i8* %.unpack120.elt, align 1
  %.unpack120.elt121 = getelementptr inbounds %MaybeInt, %MaybeInt* %0, i64 0, i32 1, i64 1
  %.unpack120.unpack122 = load i8, i8* %.unpack120.elt121, align 1
  %.unpack120.elt123 = getelementptr inbounds %MaybeInt, %MaybeInt* %0, i64 0, i32 1, i64 2
  %.unpack120.unpack124 = load i8, i8* %.unpack120.elt123, align 1
  %.unpack120.elt125 = getelementptr inbounds %MaybeInt, %MaybeInt* %0, i64 0, i32 1, i64 3
  %.unpack120.unpack126 = load i8, i8* %.unpack120.elt125, align 1
  br label %switch_exit2

case1:                                            ; preds = %entry
  %may_b.fca.0.extract = extractvalue %MaybeInt %may_b, 0
  %2 = alloca %MaybeInt, align 8
  switch i8 %may_b.fca.0.extract, label %switch_exit1 [
    i8 0, label %case2
    i8 1, label %case3
  ]

case2:                                            ; preds = %case1
  %3 = alloca %MaybeInt, align 8
  %4 = getelementptr inbounds %MaybeInt, %MaybeInt* %3, i64 0, i32 0
  store i8 0, i8* %4, align 8
  br label %switch_exit1.sink.split

case3:                                            ; preds = %case1
  %5 = alloca %MaybeInt, align 8
  switch i8 %may_c.fca.0.extract, label %switch_exit [
    i8 0, label %case4
    i8 1, label %case5
  ]

case4:                                            ; preds = %case3
  %6 = alloca %MaybeInt, align 8
  %7 = getelementptr inbounds %MaybeInt, %MaybeInt* %6, i64 0, i32 0
  store i8 0, i8* %7, align 8
  %.unpack78.elt = getelementptr inbounds %MaybeInt, %MaybeInt* %6, i64 0, i32 1, i64 0
  %.unpack78.elt79 = getelementptr inbounds %MaybeInt, %MaybeInt* %6, i64 0, i32 1, i64 1
  %.unpack78.elt81 = getelementptr inbounds %MaybeInt, %MaybeInt* %6, i64 0, i32 1, i64 2
  %.unpack78.elt83 = getelementptr inbounds %MaybeInt, %MaybeInt* %6, i64 0, i32 1, i64 3
  br label %switch_exit.sink.split

case5:                                            ; preds = %case3
  %.sroa.527.4.load.ext = zext i8 %may_a.fca.1.3.extract to i32
  %.sroa.522.4.load.ext = zext i8 %may_b.fca.1.3.extract to i32
  %.sroa.5.4.load.ext = zext i8 %may_c.fca.1.3.extract to i32
  %8 = tail call i32 @addMul(i32 %.sroa.527.4.load.ext, i32 %.sroa.522.4.load.ext, i32 %.sroa.5.4.load.ext)
  %9 = alloca %MaybeInt, align 8
  %10 = getelementptr inbounds %MaybeInt, %MaybeInt* %9, i64 0, i32 0
  store i8 1, i8* %10, align 8
  %11 = getelementptr inbounds %MaybeInt, %MaybeInt* %9, i64 0, i32 1, i64 3
  %12 = bitcast i8* %11 to i32*
  store i32 %8, i32* %12, align 4
  %.unpack29.elt = getelementptr inbounds %MaybeInt, %MaybeInt* %9, i64 0, i32 1, i64 0
  %.unpack29.elt30 = getelementptr inbounds %MaybeInt, %MaybeInt* %9, i64 0, i32 1, i64 1
  %.unpack29.elt32 = getelementptr inbounds %MaybeInt, %MaybeInt* %9, i64 0, i32 1, i64 2
  br label %switch_exit.sink.split

switch_exit.sink.split:                           ; preds = %case4, %case5
  %.sink128 = phi i8* [ %11, %case5 ], [ %.unpack78.elt83, %case4 ]
  %.sink = phi i8 [ 1, %case5 ], [ 0, %case4 ]
  %.unpack29.unpack.sink.in = phi i8* [ %.unpack29.elt, %case5 ], [ %.unpack78.elt, %case4 ]
  %.unpack29.unpack31.sink.in = phi i8* [ %.unpack29.elt30, %case5 ], [ %.unpack78.elt79, %case4 ]
  %.unpack29.unpack33.sink.in = phi i8* [ %.unpack29.elt32, %case5 ], [ %.unpack78.elt81, %case4 ]
  %.unpack29.unpack33.sink = load i8, i8* %.unpack29.unpack33.sink.in, align 1
  %.unpack29.unpack31.sink = load i8, i8* %.unpack29.unpack31.sink.in, align 1
  %.unpack29.unpack.sink = load i8, i8* %.unpack29.unpack.sink.in, align 1
  %.unpack29.unpack35 = load i8, i8* %.sink128, align 1
  %.repack = getelementptr inbounds %MaybeInt, %MaybeInt* %5, i64 0, i32 0
  store i8 %.sink, i8* %.repack, align 8
  %.repack37.repack = getelementptr inbounds %MaybeInt, %MaybeInt* %5, i64 0, i32 1, i64 0
  store i8 %.unpack29.unpack.sink, i8* %.repack37.repack, align 1
  %.repack37.repack39 = getelementptr inbounds %MaybeInt, %MaybeInt* %5, i64 0, i32 1, i64 1
  store i8 %.unpack29.unpack31.sink, i8* %.repack37.repack39, align 1
  %.repack37.repack41 = getelementptr inbounds %MaybeInt, %MaybeInt* %5, i64 0, i32 1, i64 2
  store i8 %.unpack29.unpack33.sink, i8* %.repack37.repack41, align 1
  %.repack37.repack43 = getelementptr inbounds %MaybeInt, %MaybeInt* %5, i64 0, i32 1, i64 3
  store i8 %.unpack29.unpack35, i8* %.repack37.repack43, align 1
  br label %switch_exit

switch_exit:                                      ; preds = %switch_exit.sink.split, %case3
  %.elt = getelementptr inbounds %MaybeInt, %MaybeInt* %5, i64 0, i32 0
  %.unpack = load i8, i8* %.elt, align 8
  br label %switch_exit1.sink.split

switch_exit1.sink.split:                          ; preds = %case2, %switch_exit
  %.sink132 = phi %MaybeInt* [ %5, %switch_exit ], [ %3, %case2 ]
  %.unpack.sink = phi i8 [ %.unpack, %switch_exit ], [ 0, %case2 ]
  %.unpack46.elt = getelementptr inbounds %MaybeInt, %MaybeInt* %.sink132, i64 0, i32 1, i64 0
  %.unpack46.unpack = load i8, i8* %.unpack46.elt, align 1
  %.unpack46.elt47 = getelementptr inbounds %MaybeInt, %MaybeInt* %.sink132, i64 0, i32 1, i64 1
  %.unpack46.unpack48 = load i8, i8* %.unpack46.elt47, align 1
  %.unpack46.elt49 = getelementptr inbounds %MaybeInt, %MaybeInt* %.sink132, i64 0, i32 1, i64 2
  %.unpack46.unpack50 = load i8, i8* %.unpack46.elt49, align 1
  %.unpack46.elt51 = getelementptr inbounds %MaybeInt, %MaybeInt* %.sink132, i64 0, i32 1, i64 3
  %.unpack46.unpack52 = load i8, i8* %.unpack46.elt51, align 1
  %.repack54 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 0
  store i8 %.unpack.sink, i8* %.repack54, align 8
  %.repack56.repack = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 0
  store i8 %.unpack46.unpack, i8* %.repack56.repack, align 1
  %.repack56.repack58 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 1
  store i8 %.unpack46.unpack48, i8* %.repack56.repack58, align 1
  %.repack56.repack60 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 2
  store i8 %.unpack46.unpack50, i8* %.repack56.repack60, align 1
  %.repack56.repack62 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 3
  store i8 %.unpack46.unpack52, i8* %.repack56.repack62, align 1
  br label %switch_exit1

switch_exit1:                                     ; preds = %switch_exit1.sink.split, %case1
  %.elt64 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 0
  %.unpack65 = load i8, i8* %.elt64, align 8
  %.unpack67.elt = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 0
  %.unpack67.unpack = load i8, i8* %.unpack67.elt, align 1
  %.unpack67.elt68 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 1
  %.unpack67.unpack69 = load i8, i8* %.unpack67.elt68, align 1
  %.unpack67.elt70 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 2
  %.unpack67.unpack71 = load i8, i8* %.unpack67.elt70, align 1
  %.unpack67.elt72 = getelementptr inbounds %MaybeInt, %MaybeInt* %2, i64 0, i32 1, i64 3
  %.unpack67.unpack73 = load i8, i8* %.unpack67.elt72, align 1
  br label %switch_exit2

switch_exit2:                                     ; preds = %entry, %switch_exit1, %case
  %.sroa.12.0 = phi i8 [ undef, %entry ], [ %.unpack67.unpack73, %switch_exit1 ], [ %.unpack120.unpack126, %case ]
  %.sroa.9.0 = phi i8 [ undef, %entry ], [ %.unpack67.unpack71, %switch_exit1 ], [ %.unpack120.unpack124, %case ]
  %.sroa.6.0 = phi i8 [ undef, %entry ], [ %.unpack67.unpack69, %switch_exit1 ], [ %.unpack120.unpack122, %case ]
  %.sroa.3.0 = phi i8 [ undef, %entry ], [ %.unpack67.unpack, %switch_exit1 ], [ %.unpack120.unpack, %case ]
  %.sroa.0.0 = phi i8 [ undef, %entry ], [ %.unpack65, %switch_exit1 ], [ 0, %case ]
  %.fca.0.insert = insertvalue %MaybeInt undef, i8 %.sroa.0.0, 0
  %.fca.1.0.insert = insertvalue %MaybeInt %.fca.0.insert, i8 %.sroa.3.0, 1, 0
  %.fca.1.1.insert = insertvalue %MaybeInt %.fca.1.0.insert, i8 %.sroa.6.0, 1, 1
  %.fca.1.2.insert = insertvalue %MaybeInt %.fca.1.1.insert, i8 %.sroa.9.0, 1, 2
  %.fca.1.3.insert = insertvalue %MaybeInt %.fca.1.2.insert, i8 %.sroa.12.0, 1, 3
  ret %MaybeInt %.fca.1.3.insert
}

; Function Attrs: nounwind
define i32 @main(i32 %argc, i8** nocapture readnone %argv) local_unnamed_addr #0 {
entry:
  %0 = tail call %MaybeInt* @just5()
  %.elt = getelementptr inbounds %MaybeInt, %MaybeInt* %0, i64 0, i32 0
  %.unpack = load i8, i8* %.elt, align 4
  switch i8 %.unpack, label %switch_exit [
    i8 0, label %switch_exit.sink.split
    i8 1, label %case1
  ]

case1:                                            ; preds = %entry
  br label %switch_exit.sink.split

switch_exit.sink.split:                           ; preds = %entry, %case1
  %.sink = phi i8* [ getelementptr inbounds ([2 x i8], [2 x i8]* @gstring1, i64 0, i64 0), %case1 ], [ getelementptr inbounds ([12 x i8], [12 x i8]* @gstring, i64 0, i64 0), %entry ]
  %1 = tail call i32 @puts(i8* %.sink)
  br label %switch_exit

switch_exit:                                      ; preds = %switch_exit.sink.split, %entry
  %.0 = phi i32 [ undef, %entry ], [ %1, %switch_exit.sink.split ]
  ret i32 %.0
}

attributes #0 = { nounwind }
attributes #1 = { norecurse nounwind readonly }
attributes #2 = { norecurse nounwind readnone }
