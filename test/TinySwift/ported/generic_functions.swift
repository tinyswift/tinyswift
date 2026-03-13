// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/Generics/function_decls.swift — generic function declarations

func identity<T>(_ x: T) -> T { return x }

func composeIdentity<T>(_ x: T) -> T {
    return identity(identity(x))
}

protocol Transformable {
    associatedtype Output
    func transform() -> Output
}

func applyTransform<T: Transformable>(_ value: T) -> T.Output {
    return value.transform()
}

func twoConstraints<T: Transformable, U: Transformable>(
    _ a: T, _ b: U
) -> (T.Output, U.Output) {
    return (a.transform(), b.transform())
}

struct Wrapper<T> {
    var value: T
    func map<U>(_ f: (T) -> U) -> Wrapper<U> {
        return Wrapper<U>(value: f(value))
    }
}
