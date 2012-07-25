; ModuleID = 'HelloWorld.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64"
target triple = "nvptx-nvidia-cl.1.0"

@str = private addrspace(4) constant [16 x i8] c"Hello World!!!\0A\00"
@llvm.used = appending global [1 x i8*] [i8* bitcast (void ()* @helloWorld0 to i8*)], section "llvm.metadata"

define external default void @helloWorld0() {
entry:
  %0 = tail call i8* @llvm.nvvm.ptr.constant.to.gen.p0i8.p4i8(i8 addrspace(4)* getelementptr inbounds ([16 x i8] addrspace(4)* @str, i64 0, i64 0))
  %call = tail call i32 @vprintf(i8* %0, i8* null)
  ret void
}

declare i8* @llvm.nvvm.ptr.constant.to.gen.p0i8.p4i8(i8 addrspace(4)* nocapture) nounwind readnone

declare i32 @vprintf(i8* nocapture, i8*) nounwind

!nvvm.annotations = !{!0}

!0 = metadata !{void ()* @helloWorld0, metadata !"kernel", i32 1}
