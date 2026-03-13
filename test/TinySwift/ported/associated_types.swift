// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/Generics/associated_types.swift — associated type deduction

protocol Container {
    associatedtype Element
    func get() -> Element
}

struct IntBox {
    var value: Builtin.Int64
}

struct BoxContainer: Container {
    typealias Element = IntBox
    var stored: IntBox
    func get() -> IntBox { return stored }
}

func extractElement<C: Container>(_ c: C) -> C.Element {
    return c.get()
}

protocol Stackable {
    associatedtype Item
    func peek() -> Item
    func pushed(_ item: Item) -> Self
}

struct SingleStack<T> : Stackable {
    typealias Item = T
    var top: T
    func peek() -> T { return top }
    func pushed(_ item: T) -> SingleStack<T> {
        return SingleStack<T>(top: item)
    }
}

protocol Transformable {
    associatedtype Input
    associatedtype Output
    func transform(_ input: Input) -> Output
}

struct Mapper<A, B> : Transformable {
    typealias Input = A
    typealias Output = B
    var f: (A) -> B
    func transform(_ input: A) -> B { return f(input) }
}

func applyTransform<T: Transformable>(_ t: T, _ input: T.Input) -> T.Output {
    return t.transform(input)
}
