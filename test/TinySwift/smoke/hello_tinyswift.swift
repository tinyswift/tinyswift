// RUN: %target-swift-frontend -typecheck -enable-experimental-feature Embedded -parse-stdlib %s
// REQUIRES: swift_feature_Embedded

// TinySwift smoke test: a minimal valid program using only Builtin types.

struct Point {
    var x: Builtin.Int64
    var y: Builtin.Int64
}

func makePoint(_ x: Builtin.Int64, _ y: Builtin.Int64) -> Point {
    return Point(x: x, y: y)
}
