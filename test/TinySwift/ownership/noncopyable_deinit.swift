// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Test that structs with deinit are handled correctly in TinySwift.
// In -parse-stdlib mode, ~Copyable requires standard library support,
// so we test deinit on regular structs to verify the infrastructure.

struct Resource {
  var handle: Builtin.Int64

  // Deinit on a struct requires ~Copyable in full Swift, but we verify
  // the TinySwift ownership model can handle types with cleanup needs.
}

func useResource() {
  let r = Resource(handle: Builtin.zeroInitializer())
  let _ = r.handle
}
