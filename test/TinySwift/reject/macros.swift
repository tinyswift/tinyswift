// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

macro myMacro() = #externalMacro(module: "M", type: "T") // expected-error {{macros are not supported in TinySwift}}
