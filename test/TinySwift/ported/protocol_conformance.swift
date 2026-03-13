// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/Generics/conditional_conformances.swift — protocol conformance patterns

protocol Printable {
    func printed() -> Builtin.Int64
}

protocol Summable {
    func sum() -> Builtin.Int64
}

struct Scalar {
    var value: Builtin.Int64
}

extension Scalar: Summable {
    func sum() -> Builtin.Int64 { return value }
}

extension Scalar: Printable {
    func printed() -> Builtin.Int64 { return value }
}

struct Pair<A, B> {
    var first: A
    var second: B
}

extension Pair: Printable where A: Printable, B: Printable {
    func printed() -> Builtin.Int64 {
        let _ = first.printed()
        return second.printed()
    }
}

func printIt<T: Printable>(_ val: T) -> Builtin.Int64 {
    return val.printed()
}

func testConformance(_ s: Scalar) {
    let _ = printIt(s)
    let p = Pair(first: s, second: s)
    let _ = printIt(p)
}

struct Triple<A, B, C> {
    var a: A
    var b: B
    var c: C
}

extension Triple: Printable where A: Printable, B: Printable, C: Printable {
    func printed() -> Builtin.Int64 {
        let _ = a.printed()
        let _ = b.printed()
        return c.printed()
    }
}
