// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify no __swift5_types section appears for struct/enum definitions.

// CHECK-NOT: __swift5_types
// CHECK-NOT: swift5_type_metadata

struct SimpleStruct {
  var a: Builtin.Int64
  var b: Builtin.Int64
}

enum SimpleEnum {
  case x
  case y
  case z
}

func makeStruct() -> SimpleStruct {
  return SimpleStruct(a: Builtin.zeroInitializer(), b: Builtin.zeroInitializer())
}

func makeEnum() -> SimpleEnum {
  return .x
}
