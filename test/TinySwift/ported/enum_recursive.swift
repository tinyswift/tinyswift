// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test enums with associated values and recursive patterns using generic wrappers
// Note: indirect enums are rejected in TinySwift, so recursion is modeled
// through generic wrapper structs.

struct Box<T> {
    var value: T
}

enum Expr {
    case literal(Builtin.Int64)
    case negate(Box<Expr>)
    case add(Box<Expr>, Box<Expr>)
}

func eval(_ e: Expr) -> Builtin.Int64 {
    switch e {
    case .literal(let v):
        return v
    case .negate(let inner):
        return Builtin.sub_Int64(Builtin.zeroInitializer(), eval(inner.value))
    case .add(let lhs, let rhs):
        return Builtin.add_Int64(eval(lhs.value), eval(rhs.value))
    }
}

enum ListNode<T> {
    case end
    case cons(T, Box<ListNode<T>>)
}

func headOrDefault<T>(_ list: ListNode<T>, _ fallback: T) -> T {
    switch list {
    case .end:
        return fallback
    case .cons(let value, _):
        return value
    }
}
