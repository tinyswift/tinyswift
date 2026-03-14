// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test protocols with associated types used as constraints

protocol Collection {
    associatedtype Element
    associatedtype Index
    func elementAt(_ i: Index) -> Element
}

protocol Iterator {
    associatedtype Value
    func current() -> Value
    func advanced() -> Self
}

struct ArraySlice: Collection {
    typealias Element = Builtin.Int64
    typealias Index = Builtin.Int32
    var base: Builtin.RawPointer
    func elementAt(_ i: Builtin.Int32) -> Builtin.Int64 {
        return Builtin.zeroInitializer()
    }
}

struct SliceIterator: Iterator {
    typealias Value = Builtin.Int64
    var pos: Builtin.Int32
    func current() -> Builtin.Int64 { return Builtin.zeroInitializer() }
    func advanced() -> SliceIterator {
        return SliceIterator(pos: Builtin.add_Int32(pos, Builtin.trunc_Int64_Int32(Builtin.integerLiteral_Int64(1))))
    }
}

func firstElement<C: Collection>(_ c: C) -> C.Element {
    let idx: C.Index = Builtin.zeroInitializer()
    return c.elementAt(idx)
}

func step<I: Iterator>(_ it: I) -> (I.Value, I) {
    return (it.current(), it.advanced())
}

func testAssociatedTypes(_ s: ArraySlice, _ it: SliceIterator) {
    let _ = firstElement(s)
    let _ = step(it)
}
