// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Full regression test: exercises all allowed constructs through -emit-ir.
// Verifies that TinySwift produces valid IR for value types, enums,
// generics, and closures — with no ARC runtime calls.

// CHECK-NOT: @swift_retain
// CHECK-NOT: @swift_release
// CHECK-NOT: @swift_allocObject

// --- Structs ---

struct Point {
  var x: Builtin.Int64
  var y: Builtin.Int64
}

func makePoint() -> Point {
  return Point(x: Builtin.zeroInitializer(), y: Builtin.zeroInitializer())
}

func getX(_ p: Point) -> Builtin.Int64 {
  return p.x
}

// --- Tuples ---

func makeTuple() -> (Builtin.Int64, Builtin.Int64) {
  return (Builtin.zeroInitializer(), Builtin.zeroInitializer())
}

// --- Enums ---

enum Optional<T> {
  case none
  case some(T)
}

func wrapInOptional(_ v: Builtin.Int64) -> Optional<Builtin.Int64> {
  return .some(v)
}

// --- Functions ---

func identity(_ x: Builtin.Int64) -> Builtin.Int64 {
  return x
}

func applyToZero(_ f: (Builtin.Int64) -> Builtin.Int64) -> Builtin.Int64 {
  return f(Builtin.zeroInitializer())
}

// --- Control Flow ---

func max(_ a: Builtin.Int64, _ b: Builtin.Int64) -> Builtin.Int64 {
  if Bool(Builtin.cmp_sgt_Int64(a, b)) {
    return a
  }
  return b
}

struct Bool {
  var _value: Builtin.Int1

  init(_ v: Builtin.Int1) {
    self._value = v
  }
}
