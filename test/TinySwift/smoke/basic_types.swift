// RUN: %target-swift-frontend -typecheck -enable-experimental-feature Embedded -parse-stdlib %s
// REQUIRES: swift_feature_Embedded

// TinySwift smoke test: minimal struct-only program.

struct Empty {}

struct Pair {
    var first: Builtin.Int32
    var second: Builtin.Int32
}

func makePair(_ a: Builtin.Int32, _ b: Builtin.Int32) -> Pair {
    return Pair(first: a, second: b)
}
