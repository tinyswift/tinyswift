// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test tuple creation, decomposition, and return

func makeTuple(_ a: Builtin.Int64, _ b: Builtin.Int32) -> (Builtin.Int64, Builtin.Int32) {
    return (a, b)
}

func swapTuple(_ t: (Builtin.Int64, Builtin.Int32)) -> (Builtin.Int32, Builtin.Int64) {
    return (t.1, t.0)
}

func decompose(_ t: (Builtin.Int64, Builtin.Int64)) -> Builtin.Int64 {
    let (a, b) = t
    return Builtin.add_Int64(a, b)
}

func nestedTuple(_ a: Builtin.Int64, _ b: Builtin.Int32, _ c: Builtin.Int1)
    -> ((Builtin.Int64, Builtin.Int32), Builtin.Int1) {
    return ((a, b), c)
}

func tripleTuple(_ x: Builtin.Int64) -> (Builtin.Int64, Builtin.Int64, Builtin.Int64) {
    return (x, x, x)
}

func accessByIndex(_ t: (Builtin.Int64, Builtin.Int32, Builtin.Int1)) -> Builtin.Int64 {
    return t.0
}

func testTuples(_ a: Builtin.Int64, _ b: Builtin.Int32, _ c: Builtin.Int1) {
    let t = makeTuple(a, b)
    let _ = swapTuple(t)
    let _ = decompose((a, a))
    let _ = nestedTuple(a, b, c)
    let _ = tripleTuple(a)
    let _ = accessByIndex((a, b, c))
}
