// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

@globalActor // expected-error {{'@globalActor' is not supported in TinySwift; no concurrency}}
struct MyActor {
    actor Impl {} // expected-error {{actors are not supported in TinySwift}}
    static let shared = Impl() // expected-error {{concurrency is not supported in TinySwift}}
}
