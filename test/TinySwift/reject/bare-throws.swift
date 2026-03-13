// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

func canFail() throws {} // expected-error {{bare 'throws' is not supported in TinySwift; use 'throws(ConcreteType)'}}
