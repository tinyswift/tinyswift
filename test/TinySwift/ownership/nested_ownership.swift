// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Test ownership through nested struct hierarchies in TinySwift mode.

struct Inner {
  var x: Builtin.Int64
}

struct Middle {
  var inner: Inner
  var y: Builtin.Int64
}

struct Outer {
  var middle: Middle
  var z: Builtin.Int64
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12extractInner
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
func extractInner(_ o: Outer) -> Inner {
  return o.middle.inner
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}10extractAll
func extractAll(_ o: Outer) -> (Builtin.Int64, Builtin.Int64, Builtin.Int64) {
  return (o.middle.inner.x, o.middle.y, o.z)
}

func buildNested() -> Outer {
  let i = Inner(x: Builtin.zeroInitializer())
  let m = Middle(inner: i, y: Builtin.zeroInitializer())
  return Outer(middle: m, z: Builtin.zeroInitializer())
}
