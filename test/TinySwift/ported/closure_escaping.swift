// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test escaping closures using @escaping in function types

struct Callback {
    var action: () -> Builtin.Int64
}

func storeEscaping(_ f: @escaping () -> Builtin.Int64) -> Callback {
    return Callback(action: f)
}

func applyEscaping(_ cb: Callback) -> Builtin.Int64 {
    return cb.action()
}

struct Transform<T, U> {
    var fn: (T) -> U
}

func makeTransform<T, U>(_ f: @escaping (T) -> U) -> Transform<T, U> {
    return Transform(fn: f)
}

func compose<A, B, C>(
    _ f: @escaping (A) -> B,
    _ g: @escaping (B) -> C
) -> (A) -> C {
    return { a in g(f(a)) }
}

func testEscaping(_ v: Builtin.Int64) {
    let cb = storeEscaping { v }
    let _ = applyEscaping(cb)
    let t = makeTransform { (x: Builtin.Int64) in x }
    let _ = t.fn(v)
    let composed = compose(
        { (x: Builtin.Int64) in x },
        { (y: Builtin.Int64) in y }
    )
    let _ = composed(v)
}
