// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

open struct Foo {} // expected-error {{'open' access level is not supported in TinySwift; use 'public'}}
