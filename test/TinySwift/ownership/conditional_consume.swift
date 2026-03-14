// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Test consuming in only one branch in TinySwift mode.
// This exercises drop flag determination for partially-consumed values.

struct Handle {
  var fd: Builtin.Int64
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}15conditionalUse
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
func conditionalUse(_ h: Handle, _ flag: Builtin.Int1) -> Builtin.Int64 {
  if Bool(flag) {
    return h.fd
  }
  return Builtin.zeroInitializer()
}

// Simple Bool-like wrapper for branching (needed in -parse-stdlib).
struct Bool {
  var _value: Builtin.Int1
  init(_ v: Builtin.Int1) { self._value = v }
}
