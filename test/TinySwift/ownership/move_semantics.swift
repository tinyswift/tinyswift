// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Test move-like patterns in TinySwift mode.
// In -parse-stdlib mode, ~Copyable and `consume` are unavailable,
// so we verify via SIL that value types are moved without ARC.

struct Resource {
  var handle: Builtin.Int64
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12takeResource
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
func takeResource(_ r: Resource) -> Builtin.Int64 {
  return r.handle
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12passResource
func passResource() -> Builtin.Int64 {
  let r = Resource(handle: Builtin.zeroInitializer())
  return takeResource(r)
}

// Multiple value transfers without copies
func chainResources(_ a: Resource, _ b: Resource) -> (Builtin.Int64, Builtin.Int64) {
  return (takeResource(a), takeResource(b))
}
