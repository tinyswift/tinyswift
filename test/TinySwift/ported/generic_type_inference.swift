// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test type inference with generics

struct Box<T> {
    var value: T
}

struct Pair<A, B> {
    var first: A
    var second: B
}

func identity<T>(_ x: T) -> T { return x }
func makePair<A, B>(_ a: A, _ b: B) -> Pair<A, B> { return Pair(first: a, second: b) }
func wrap<T>(_ x: T) -> Box<T> { return Box(value: x) }

func testInference(_ i32: Builtin.Int32, _ i64: Builtin.Int64) {
    // Infer T from argument
    let _: Builtin.Int32 = identity(i32)
    let _: Builtin.Int64 = identity(i64)

    // Infer A and B from arguments
    let _: Pair<Builtin.Int32, Builtin.Int64> = makePair(i32, i64)

    // Infer through nesting
    let _: Box<Builtin.Int64> = wrap(i64)
    let _: Box<Box<Builtin.Int32>> = wrap(wrap(i32))

    // Infer through chained calls
    let _: Pair<Box<Builtin.Int32>, Builtin.Int64> = makePair(wrap(i32), i64)

    // Infer from memberwise init
    let _ = Box(value: i32)
    let _ = Pair(first: i32, second: i64)
}
