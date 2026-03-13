// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that no type metadata or type metadata accessor symbols appear in IR.

// CHECK-NOT: type metadata for
// CHECK-NOT: type metadata accessor for
// CHECK-NOT: nominal type descriptor for
// CHECK-NOT: @"$s{{.*}}MN"
// CHECK-NOT: @"$s{{.*}}Ma"

struct Pair {
  var first: Builtin.Int32
  var second: Builtin.Int32
}

func makePair() -> Pair {
  return Pair(first: Builtin.zeroInitializer(), second: Builtin.zeroInitializer())
}
