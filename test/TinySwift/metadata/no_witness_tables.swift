// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that no protocol witness tables appear in the emitted IR.

// CHECK-NOT: protocol witness table for
// CHECK-NOT: protocol conformance descriptor for
// CHECK-NOT: @"$s{{.*}}WP"
// CHECK-NOT: @"$s{{.*}}Mc"

protocol Summable {
  func sum() -> Builtin.Int64
}

struct Vector: Summable {
  var x: Builtin.Int64
  var y: Builtin.Int64

  func sum() -> Builtin.Int64 {
    return Builtin.add_Int64(x, y)
  }
}

func callSum<T: Summable>(_ v: T) -> Builtin.Int64 {
  return v.sum()
}

func test() -> Builtin.Int64 {
  let v = Vector(x: Builtin.zeroInitializer(), y: Builtin.zeroInitializer())
  return callSum(v)
}
