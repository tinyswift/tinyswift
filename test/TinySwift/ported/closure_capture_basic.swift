// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test basic closure capture of local variables

func captureLocal(_ x: Builtin.Int64) -> () -> Builtin.Int64 {
    return { x }
}

func captureMultiple(_ a: Builtin.Int64, _ b: Builtin.Int32) -> () -> (Builtin.Int64, Builtin.Int32) {
    return { (a, b) }
}

struct Pair {
    var x: Builtin.Int64
    var y: Builtin.Int64
}

func captureStruct(_ p: Pair) -> () -> Builtin.Int64 {
    return { p.x }
}

func captureAndTransform(_ v: Builtin.Int64) -> (Builtin.Int64) -> Builtin.Int64 {
    return { other in Builtin.add_Int64(v, other) }
}

func nestedCapture(_ a: Builtin.Int64) -> () -> () -> Builtin.Int64 {
    return {
        return { a }
    }
}

func testCaptures(_ x: Builtin.Int64, _ y: Builtin.Int32) {
    let f1 = captureLocal(x)
    let _ = f1()
    let f2 = captureMultiple(x, y)
    let _ = f2()
    let f3 = captureAndTransform(x)
    let _ = f3(x)
}
