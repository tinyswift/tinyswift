// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify that generic functions are specialized (monomorphized) and the
// specialized versions appear in SIL.

// CHECK-LABEL: sil {{.*}}@$s{{.*}}2id{{.*}} :
// CHECK-NOT: apply {{.*}}<

func id<T>(_ x: T) -> T {
  return x
}

func helper(_ x: Builtin.Int64) -> Builtin.Int64 {
  return id(x)
}
