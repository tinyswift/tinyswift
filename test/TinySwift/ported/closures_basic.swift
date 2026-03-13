// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/expr/closure/basic.swift — closure syntax and types

struct Val {
    var x: Builtin.Int64
}

func takeClosure(_ f: (Val) -> Val, _ v: Val) -> Val {
    return f(v)
}

func testClosures(_ v: Val) {
    let _ = takeClosure({ v in return v }, v)
    let _ = takeClosure({ v in v }, v)
    let _ = takeClosure({ $0 }, v)
}

func testClosureTypeAnnotation() {
    let double: (Builtin.Int64) -> Builtin.Int64 = { v in
        return v
    }
    let _ = double
}

func multiParam() {
    let f: (Builtin.Int64, Builtin.Int64) -> Builtin.Int64 = { a, b in a }
    let _ = f
}

func nestedClosure() {
    let outer: (Builtin.Int64) -> (Builtin.Int64) -> Builtin.Int64 = { a in
        return { b in a }
    }
    let _ = outer
}

func closureReturningTuple() {
    let f: (Builtin.Int64) -> (Builtin.Int64, Builtin.Int64) = { x in (x, x) }
    let _ = f
}

func trailingClosure(_ v: Val) -> Val {
    return takeClosure({ $0 }, v)
}
