// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s -verify
// REQUIRES: swift_feature_TinySwift

// Test that borrowing works in TinySwift mode.

struct Buffer {
  var ptr: Builtin.Int64
  var len: Builtin.Int64

  deinit {}
}

func readBuffer(_ b: borrowing Buffer) -> Builtin.Int64 {
  return b.len
}
