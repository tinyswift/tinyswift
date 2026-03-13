// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that the emitted IR is minimal: no metadata globals, no runtime
// calls for type metadata access.

// The IR should contain the function definitions but no metadata.
// CHECK: define {{.*}}@"$s{{.*}}5emptyyyF"
// CHECK-NOT: @"$s{{.*}}MN"
// CHECK-NOT: @"$s{{.*}}Ma"
// CHECK-NOT: swift_getTypeMetadata
// CHECK-NOT: swift_allocObject
// CHECK-NOT: swift_retain
// CHECK-NOT: swift_release

func empty() {}

struct Small {
  var value: Builtin.Int32
}

func makeSmall() -> Small {
  return Small(value: Builtin.zeroInitializer())
}
