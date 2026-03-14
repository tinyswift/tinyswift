// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify no reflection metadata appears in TinySwift IR.

// CHECK-NOT: __swift5_reflstr
// CHECK-NOT: __swift5_capture
// CHECK-NOT: __swift5_builtin
// CHECK-NOT: __swift5_assocty

protocol Convertible {
  associatedtype Target
  func convert() -> Target
}

struct Celsius {
  var degrees: Builtin.Int64
}

struct Fahrenheit {
  var degrees: Builtin.Int64
}

extension Celsius: Convertible {
  typealias Target = Fahrenheit
  func convert() -> Fahrenheit {
    return Fahrenheit(degrees: degrees)
  }
}
