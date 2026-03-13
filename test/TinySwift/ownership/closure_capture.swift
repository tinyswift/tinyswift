// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s -verify
// REQUIRES: swift_feature_TinySwift

// Test closure capture behavior in TinySwift mode.

struct Data {
  var value: Builtin.Int64
}

func useClosure(_ fn: () -> ()) {
  fn()
}

func testCapture() {
  let d = Data(value: Builtin.zeroInitializer())
  useClosure {
    _ = d.value
  }
}
