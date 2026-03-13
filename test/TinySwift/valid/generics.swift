// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

struct Pair<A, B> {
    var first: A
    var second: B
}

func swap<T, U>(_ pair: Pair<T, U>) -> Pair<U, T> {
    return Pair(first: pair.second, second: pair.first)
}

protocol Combinable {
    func combine(_ other: Self) -> Self
}

func merge<T: Combinable>(_ a: T, _ b: T) -> T {
    return a.combine(b)
}
