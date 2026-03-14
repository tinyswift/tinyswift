// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify no __swift5_fieldmd section appears for struct field descriptors.

// CHECK-NOT: __swift5_fieldmd
// CHECK-NOT: __swift5_reflstr
// CHECK-NOT: __swift5_typeref

struct Point3D {
  var x: Builtin.Int64
  var y: Builtin.Int64
  var z: Builtin.Int64
}

struct NamedPoint {
  var name: Builtin.Int64
  var point: Point3D
}

func makePoint() -> Point3D {
  return Point3D(x: Builtin.zeroInitializer(),
                 y: Builtin.zeroInitializer(),
                 z: Builtin.zeroInitializer())
}
