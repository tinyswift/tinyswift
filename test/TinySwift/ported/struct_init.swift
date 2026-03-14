// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test struct initialization: memberwise init, custom init

struct Pair {
    var a: Builtin.Int64
    var b: Builtin.Int64
}

struct Rect {
    var originX: Builtin.Int64
    var originY: Builtin.Int64
    var width: Builtin.Int64
    var height: Builtin.Int64

    init(square side: Builtin.Int64) {
        originX = Builtin.zeroInitializer()
        originY = Builtin.zeroInitializer()
        width = side
        height = side
    }

    init(at x: Builtin.Int64, _ y: Builtin.Int64, size w: Builtin.Int64, _ h: Builtin.Int64) {
        originX = x
        originY = y
        width = w
        height = h
    }

    func area() -> Builtin.Int64 {
        return Builtin.mul_Int64(width, height)
    }
}

func testMemberwiseInit(_ a: Builtin.Int64, _ b: Builtin.Int64) -> Pair {
    return Pair(a: a, b: b)
}

func testCustomInit(_ side: Builtin.Int64) -> Rect {
    return Rect(square: side)
}

func testFullInit(_ x: Builtin.Int64, _ y: Builtin.Int64,
                  _ w: Builtin.Int64, _ h: Builtin.Int64) -> Rect {
    return Rect(at: x, y, size: w, h)
}
