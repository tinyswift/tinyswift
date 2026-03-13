// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s -verify
// REQUIRES: swift_feature_TinySwift

// Test consuming a struct in TinySwift mode.

struct FileHandle {
  var fd: Builtin.Int64

  deinit {}
}

func closeHandle(_ h: consuming FileHandle) {
  // h is consumed here, no implicit copy needed
  _ = h
}
