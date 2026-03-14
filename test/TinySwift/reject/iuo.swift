// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

struct Wrapper {
    var value: Builtin.Int64! // expected-error {{implicitly unwrapped optionals are not supported in TinySwift}}
}
