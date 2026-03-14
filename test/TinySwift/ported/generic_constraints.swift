// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test generics with multiple constraints and where clauses

protocol Equatable {
    func isEqual(to other: Self) -> Builtin.Int1
}

protocol Hashable: Equatable {
    func hashValue() -> Builtin.Int64
}

protocol Addable {
    func added(to other: Self) -> Self
}

func findAndAdd<T: Hashable & Addable>(_ a: T, _ b: T) -> T {
    let _ = a.isEqual(to: b)
    let _ = a.hashValue()
    return a.added(to: b)
}

struct Pair<A, B> {
    var first: A
    var second: B
}

func matchPairs<A: Equatable, B: Equatable>(
    _ p1: Pair<A, B>, _ p2: Pair<A, B>
) -> Builtin.Int1 {
    let firstMatch = p1.first.isEqual(to: p2.first)
    let secondMatch = p1.second.isEqual(to: p2.second)
    return Builtin.and_Int1(firstMatch, secondMatch)
}

func sameMapped<C1, C2>(_ a: C1, _ b: C2) -> Builtin.Int1
    where C1: Hashable, C2: Hashable, C1: Addable
{
    let _ = a.added(to: a)
    let h1 = a.hashValue()
    let h2 = b.hashValue()
    return Builtin.cmp_eq_Int64(h1, h2)
}
