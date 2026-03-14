// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

struct Point {
    var x: Builtin.Int64
    var y: Builtin.Int64
}

func getMetatype() -> Point.Type { // expected-error {{'.Type' and '.Protocol' metatypes are not supported in TinySwift}}
    return Point.self // expected-error {{'.Type' and '.Protocol' metatypes are not supported in TinySwift}}
}
