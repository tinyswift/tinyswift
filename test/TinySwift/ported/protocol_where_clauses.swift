// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/Generics/algorithms.swift — generic algorithms with where clauses
// Note: `if <method_call>` crashes in -parse-stdlib mode (pre-existing compiler bug),
// so algorithms use switch-based dispatch instead.

protocol Equatable {
    func isEqual(to other: Self) -> Builtin.Int1
}

protocol Comparable: Equatable {
    func isLessThan(_ other: Self) -> Builtin.Int1
}

struct Int32Val: Comparable {
    var value: Builtin.Int32

    func isEqual(to other: Int32Val) -> Builtin.Int1 {
        return Builtin.cmp_eq_Int32(value, other.value)
    }

    func isLessThan(_ other: Int32Val) -> Builtin.Int1 {
        return Builtin.cmp_slt_Int32(value, other.value)
    }
}

func allEqual<T: Equatable>(_ a: T, _ b: T, _ c: T) -> Builtin.Int1 {
    let ab = a.isEqual(to: b)
    let bc = b.isEqual(to: c)
    return Builtin.and_Int1(ab, bc)
}

func testAlgorithms(_ a: Int32Val, _ b: Int32Val) {
    let _ = allEqual(a, a, a)
}

protocol Container {
    associatedtype Element: Equatable
    func first() -> Element
}

func sameFirst<C: Container, D: Container>(_ c: C, _ d: D) -> Builtin.Int1
    where C.Element == D.Element
{
    return c.first().isEqual(to: d.first())
}

struct Wrapper<T: Equatable>: Container {
    var stored: T
    func first() -> T { return stored }
}

func testSameFirst(_ a: Int32Val, _ b: Int32Val) {
    let w1 = Wrapper(stored: a)
    let w2 = Wrapper(stored: b)
    let _ = sameFirst(w1, w2)
}
