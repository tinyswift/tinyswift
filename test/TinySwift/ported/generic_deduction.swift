// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/Generics/deduction.swift — generic argument deduction

func id<T>(_ x: T) -> T { return x }

func testDeduction(_ v32: Builtin.Int32, _ v64: Builtin.Int64) {
    let _: Builtin.Int32 = id(v32)
    let _: Builtin.Int64 = id(v64)
}

struct Box<T> {
    var value: T
}

func unbox<T>(_ b: Box<T>) -> T {
    return b.value
}

func testBoxDeduction(_ v: Builtin.Int32) {
    let b = Box(value: v)
    let _: Builtin.Int32 = unbox(b)
}

func firstOf<T>(_ a: T, _ b: T) -> T {
    return a
}

func pairOf<A, B>(_ a: A, _ b: B) -> (A, B) {
    return (a, b)
}

func testMultipleDeduction(_ a: Builtin.Int32, _ b: Builtin.Int64) {
    let _ = firstOf(a, a)
    let _: (Builtin.Int32, Builtin.Int64) = pairOf(a, b)
}

func nested<T>(_ x: T) -> Box<T> {
    return Box(value: x)
}

func testNestedDeduction(_ v: Builtin.Int64) {
    let _: Box<Builtin.Int64> = nested(v)
    let _: Box<Box<Builtin.Int64>> = nested(nested(v))
}
