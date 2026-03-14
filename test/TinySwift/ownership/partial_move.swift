// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Test partial struct field consumption in TinySwift mode.
// Fields should be individually movable without ARC.

struct TwoFields {
  var a: Builtin.Int64
  var b: Builtin.Int64
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}8getFirst
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
func getFirst(_ s: TwoFields) -> Builtin.Int64 {
  return s.a
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}9getSecond
func getSecond(_ s: TwoFields) -> Builtin.Int64 {
  return s.b
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}8getBoth
func getBoth(_ s: TwoFields) -> (Builtin.Int64, Builtin.Int64) {
  return (s.a, s.b)
}
