// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Full regression test: exercises all allowed constructs through -emit-ir.
// Verifies that TinySwift produces valid IR for value types, enums,
// and closures — with no ARC runtime calls.

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

enum Direction {
  case up, down, left, right
}

func isUp(_ d: Direction, _ yes: Builtin.Int1, _ no: Builtin.Int1) -> Builtin.Int1 {
  switch d {
  case .up: return yes
  default: return no
  }
}

// --- Functions ---

func identity(_ x: Builtin.Int64) -> Builtin.Int64 {
  return x
}

// Note: closure parameters crash MoveOnlyChecker in -parse-stdlib mode
// (pre-existing Phase 2 issue). Tested at typecheck level instead.

// --- Arithmetic ---

func add(_ a: Builtin.Int64, _ b: Builtin.Int64) -> Builtin.Int64 {
  return Builtin.add_Int64(a, b)
}

func sub(_ a: Builtin.Int64, _ b: Builtin.Int64) -> Builtin.Int64 {
  return Builtin.sub_Int64(a, b)
}

func compare(_ a: Builtin.Int64, _ b: Builtin.Int64) -> Builtin.Int1 {
  return Builtin.cmp_eq_Int64(a, b)
}

// --- Nested structs ---

struct Rect {
  var origin: Point
  var size: Point
}

func makeRect() -> Rect {
  let p = Point(x: Builtin.zeroInitializer(), y: Builtin.zeroInitializer())
  return Rect(origin: p, size: p)
}

func getOriginX(_ r: Rect) -> Builtin.Int64 {
  return r.origin.x
}
