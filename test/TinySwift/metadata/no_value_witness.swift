// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that no value witness tables appear in the emitted IR.

// CHECK-NOT: value witness table for
// CHECK-NOT: @"$s{{.*}}WV"

struct Triple {
  var a: Builtin.Int64
  var b: Builtin.Int64
  var c: Builtin.Int64
}

func makeTriple() -> Triple {
  return Triple(
    a: Builtin.zeroInitializer(),
    b: Builtin.zeroInitializer(),
    c: Builtin.zeroInitializer()
  )
}
