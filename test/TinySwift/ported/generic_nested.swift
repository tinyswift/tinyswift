// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test nested generic types and functions

struct Outer<T> {
    var value: T

    struct Inner<U> {
        var first: T
        var second: U
    }

    func wrap<U>(_ other: U) -> Inner<U> {
        return Inner(first: value, second: other)
    }
}

func makeNested<A, B>(_ a: A, _ b: B) -> Outer<A>.Inner<B> {
    return Outer<A>.Inner(first: a, second: b)
}

struct Stack<T> {
    struct Entry {
        var element: T
        var index: Builtin.Int32
    }

    var top: Entry

    func mapEntry<U>(_ f: (T) -> U) -> Stack<U>.Entry {
        return Stack<U>.Entry(element: f(top.element), index: top.index)
    }
}

func testNested(_ x: Builtin.Int64, _ y: Builtin.Int32) {
    let o = Outer(value: x)
    let _ = o.wrap(y)
    let _ = makeNested(x, y)
    let entry = Stack<Builtin.Int64>.Entry(element: x, index: y)
    let s = Stack(top: entry)
    let _ = s.mapEntry { v in v }
}
