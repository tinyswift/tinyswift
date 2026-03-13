// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that no ARC function references appear in IR output.

// CHECK-NOT: swift_retain
// CHECK-NOT: swift_release
// CHECK-NOT: swift_allocObject
// CHECK-NOT: swift_deallocObject
// CHECK-NOT: swift_retain_n
// CHECK-NOT: swift_release_n

struct Pair {
  var first: Builtin.Int64
  var second: Builtin.Int64
}

func swapPair(_ p: Pair) -> Pair {
  return Pair(first: p.second, second: p.first)
}

func nestedCall() -> Pair {
  let p = Pair(first: Builtin.zeroInitializer(), second: Builtin.zeroInitializer())
  return swapPair(p)
}
