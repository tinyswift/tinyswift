// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test compound assignment operators

precedencegroup AssignmentPrecedence {
    assignment: true
}

precedencegroup AdditionPrecedence {
    associativity: left
}

infix operator += : AssignmentPrecedence
infix operator -= : AssignmentPrecedence
infix operator *= : AssignmentPrecedence

struct Number {
    var value: Builtin.Int64
}

func += (lhs: inout Number, rhs: Number) {
    lhs = Number(value: Builtin.add_Int64(lhs.value, rhs.value))
}

func -= (lhs: inout Number, rhs: Number) {
    lhs = Number(value: Builtin.sub_Int64(lhs.value, rhs.value))
}

func *= (lhs: inout Number, rhs: Number) {
    lhs = Number(value: Builtin.mul_Int64(lhs.value, rhs.value))
}

func testCompound(_ a: Builtin.Int64, _ b: Builtin.Int64) {
    var x = Number(value: a)
    let y = Number(value: b)
    x += y
    x -= y
    x *= y
    let _ = x
}
