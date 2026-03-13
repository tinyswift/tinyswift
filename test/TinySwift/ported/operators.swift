// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/decl/func/operator.swift — operator declarations and overloading

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

infix operator + : AdditionPrecedence
infix operator - : AdditionPrecedence
prefix operator -
infix operator == : ComparisonPrecedence

struct Vec2 {
    var x: Builtin.Int64
    var y: Builtin.Int64
}

func + (lhs: Vec2, rhs: Vec2) -> Vec2 {
    return Vec2(
        x: Builtin.add_Int64(lhs.x, rhs.x),
        y: Builtin.add_Int64(lhs.y, rhs.y)
    )
}

func - (lhs: Vec2, rhs: Vec2) -> Vec2 {
    return Vec2(
        x: Builtin.sub_Int64(lhs.x, rhs.x),
        y: Builtin.sub_Int64(lhs.y, rhs.y)
    )
}

prefix func - (v: Vec2) -> Vec2 {
    return Vec2(
        x: Builtin.sub_Int64(Builtin.zeroInitializer(), v.x),
        y: Builtin.sub_Int64(Builtin.zeroInitializer(), v.y)
    )
}

func == (lhs: Vec2, rhs: Vec2) -> Builtin.Int1 {
    let xEq = Builtin.cmp_eq_Int64(lhs.x, rhs.x)
    let yEq = Builtin.cmp_eq_Int64(lhs.y, rhs.y)
    return Builtin.and_Int1(xEq, yEq)
}

func testOperators(_ a: Vec2, _ b: Vec2) {
    let _ = a + b
    let _ = a - b
    let _ = -a
    let _ = a == b
}
