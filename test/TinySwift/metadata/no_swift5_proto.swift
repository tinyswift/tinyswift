// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature TinySwift \
// RUN:   -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Verify no __swift5_proto section appears for protocol conformances.

// CHECK-NOT: __swift5_proto
// CHECK-NOT: swift5_protocols
// CHECK-NOT: swift5_protocol_conformances

protocol Measurable {
  func measure() -> Builtin.Int64
}

struct Sensor {
  var reading: Builtin.Int64
}

extension Sensor: Measurable {
  func measure() -> Builtin.Int64 {
    return reading
  }
}

func doMeasure<T: Measurable>(_ m: T) -> Builtin.Int64 {
  return m.measure()
}
