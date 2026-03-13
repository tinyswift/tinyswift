// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that TinySwift mode produces no ARC runtime calls in IR output.

// CHECK-NOT: @swift_retain
// CHECK-NOT: @swift_release
// CHECK-NOT: @swift_allocObject

struct Point {
  var x: Builtin.Int64
  var y: Builtin.Int64
}

func makePoint() -> Point {
  return Point(x: Builtin.zeroInitializer(), y: Builtin.zeroInitializer())
}

func copyPoint(_ p: Point) -> Point {
  return p
}
