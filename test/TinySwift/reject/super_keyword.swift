// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Note: 'super' requires class inheritance which is rejected at the class
// declaration level. Since classes are rejected before super can be used,
// this test validates the diagnostic infrastructure is in place.

struct PlainStruct {}
