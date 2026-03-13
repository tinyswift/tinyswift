// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

protocol Drawable {}

func draw(_ shape: any Drawable) {} // expected-error {{existential types are not supported in TinySwift; use generic constraints}}
