// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

func apply(_ fn: () throws -> Builtin.Int64) rethrows -> Builtin.Int64 { // expected-error {{'rethrows' is not supported in TinySwift; use typed throws}}
    return try fn()
}
