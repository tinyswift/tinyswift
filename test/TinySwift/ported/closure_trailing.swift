// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test trailing closure syntax

func apply(_ x: Builtin.Int64, _ f: (Builtin.Int64) -> Builtin.Int64) -> Builtin.Int64 {
    return f(x)
}

func combine(_ a: Builtin.Int64, _ b: Builtin.Int64,
             using f: (Builtin.Int64, Builtin.Int64) -> Builtin.Int64) -> Builtin.Int64 {
    return f(a, b)
}

struct Box<T> {
    var value: T
    func map<U>(_ f: (T) -> U) -> Box<U> {
        return Box<U>(value: f(value))
    }
}

func testTrailing(_ x: Builtin.Int64, _ y: Builtin.Int64) {
    // Trailing closure syntax
    let _ = apply(x) { v in v }

    let _ = combine(x, y) { a, b in
        Builtin.add_Int64(a, b)
    }

    let b = Box(value: x)
    let _ = b.map { v in
        Builtin.mul_Int64(v, v)
    }

    // Nested trailing closures
    let _ = apply(x) { outer in
        apply(outer) { inner in inner }
    }
}
