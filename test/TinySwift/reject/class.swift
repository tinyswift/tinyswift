// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

class Foo {} // expected-error {{classes are not supported in TinySwift; use 'struct'}}
