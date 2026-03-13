// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s -verify
// REQUIRES: swift_feature_TinySwift

// Test that explicit consume works in TinySwift mode.

struct Resource {
  var handle: Builtin.Int64

  deinit {}
}

func takeResource(_ r: consuming Resource) {
  _ = r
}

func passResource() {
  var r = Resource(handle: Builtin.zeroInitializer())
  takeResource(consume r)
}
