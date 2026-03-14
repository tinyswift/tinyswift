// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test custom operator definitions and overloading

precedencegroup AdditionPrecedence {
    associativity: left
}

precedencegroup MultiplicationPrecedence {
    associativity: left
    higherThan: AdditionPrecedence
}

precedencegroup ComparisonPrecedence {
    associativity: none
}

infix operator <+> : AdditionPrecedence
infix operator <*> : MultiplicationPrecedence
prefix operator ~
infix operator === : ComparisonPrecedence

struct Val {
    var n: Builtin.Int64
}

func <+> (lhs: Val, rhs: Val) -> Val {
    return Val(n: Builtin.add_Int64(lhs.n, rhs.n))
}

func <*> (lhs: Val, rhs: Val) -> Val {
    return Val(n: Builtin.mul_Int64(lhs.n, rhs.n))
}

prefix func ~ (v: Val) -> Val {
    return Val(n: Builtin.sub_Int64(Builtin.zeroInitializer(), v.n))
}

func === (lhs: Val, rhs: Val) -> Builtin.Int1 {
    return Builtin.cmp_eq_Int64(lhs.n, rhs.n)
}

func testCustomOps(_ a: Val, _ b: Val) {
    let _ = a <+> b
    let _ = a <*> b
    let _ = a <+> b <*> a   // tests precedence
    let _ = ~a
    let _ = a === b
}
