// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

func takesBlock(_ fn: @convention(block) () -> ()) {} // expected-error {{'@convention(block)' is not supported in TinySwift}}
