// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

struct Empty {}

struct Point {
    var x: Builtin.Int64
    var y: Builtin.Int64
}

struct Generic<T> {
    var value: T
}

struct Nested {
    struct Inner {
        var val: Builtin.Int32
    }
    var inner: Inner
}
