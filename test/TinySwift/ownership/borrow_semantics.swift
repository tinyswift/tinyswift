// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Test that borrowing semantics work in TinySwift mode.
// In -parse-stdlib mode, ~Copyable is unavailable, so we test borrowing
// via SIL output on regular structs (all TinySwift types use OSSA).

struct Buffer {
  var ptr: Builtin.Int64
  var len: Builtin.Int64
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}10readBuffer
// CHECK-NOT: copy_value
func readBuffer(_ b: Buffer) -> Builtin.Int64 {
  return b.len
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}11swapBuffers
func swapBuffers(_ a: Buffer, _ b: Buffer) -> (Buffer, Buffer) {
  return (b, a)
}

func makeBuffer() -> Buffer {
  return Buffer(ptr: Builtin.zeroInitializer(), len: Builtin.zeroInitializer())
}
