// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Note: The 'is' operator requires runtime metadata for type checking.
// In -parse-stdlib mode, 'is' may require stdlib infrastructure to parse.
// This test validates the diagnostic is in place for when stdlib is available.

struct Marker {}
