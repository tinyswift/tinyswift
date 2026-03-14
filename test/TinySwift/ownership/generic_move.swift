// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Test move semantics through generic functions in TinySwift mode.
// Values passed through generics should not introduce ARC operations.

struct Wrapper<T> {
  var value: T
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}7unwrap
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
func unwrap<T>(_ w: Wrapper<T>) -> T {
  return w.value
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}6rewrap
func rewrap<T>(_ val: T) -> Wrapper<T> {
  return Wrapper(value: val)
}

func testGenericMove() {
  let w = Wrapper<Builtin.Int64>(value: Builtin.zeroInitializer())
  let val = unwrap(w)
  let w2 = rewrap(val)
  let _ = unwrap(w2)
}
