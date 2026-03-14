// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Comprehensive check: no metadata sections of any kind in TinySwift IR.
// This test combines structs, enums, protocols, and generics.

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

protocol Identifiable {
  func id() -> Builtin.Int64
}

struct Entity {
  var eid: Builtin.Int64
}

extension Entity: Identifiable {
  func id() -> Builtin.Int64 { return eid }
}

enum Status {
  case active
  case inactive
}

struct Container<T> {
  var item: T
}

func identify<T: Identifiable>(_ x: T) -> Builtin.Int64 {
  return x.id()
}

func makeContainer() -> Container<Entity> {
  return Container(item: Entity(eid: Builtin.zeroInitializer()))
}
