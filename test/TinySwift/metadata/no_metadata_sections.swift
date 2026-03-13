// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that no __swift5_* metadata sections appear in the emitted IR.

// CHECK-NOT: __swift5_types
// CHECK-NOT: __swift5_proto
// CHECK-NOT: __swift5_fieldmd
// CHECK-NOT: __swift5_reflstr
// CHECK-NOT: __swift5_typeref
// CHECK-NOT: __swift5_capture
// CHECK-NOT: __swift5_builtin
// CHECK-NOT: __swift5_assocty
// CHECK-NOT: __swift5_mpenum
// CHECK-NOT: swift5_type_metadata
// CHECK-NOT: swift5_protocols
// CHECK-NOT: swift5_protocol_conformances

struct Point {
  var x: Builtin.Int64
  var y: Builtin.Int64
}

enum Direction {
  case north
  case south
  case east
  case west
}

func makePoint() -> Point {
  return Point(x: Builtin.zeroInitializer(), y: Builtin.zeroInitializer())
}

func getDirection() -> Direction {
  return .north
}
