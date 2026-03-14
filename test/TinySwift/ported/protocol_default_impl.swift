// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test protocol default implementations via extensions

protocol Describable {
    func tag() -> Builtin.Int64
}

extension Describable {
    func doubleTag() -> Builtin.Int64 {
        return Builtin.add_Int64(tag(), tag())
    }

    func isZeroTag() -> Builtin.Int1 {
        return Builtin.cmp_eq_Int64(tag(), Builtin.zeroInitializer())
    }
}

protocol Resetable {
    func reset() -> Self
    func isReset() -> Builtin.Int1
}

extension Resetable where Self: Describable {
    func resetAndTag() -> Builtin.Int64 {
        let r = reset()
        return r.tag()
    }
}

struct Sensor: Describable, Resetable {
    var reading: Builtin.Int64

    func tag() -> Builtin.Int64 { return reading }

    func reset() -> Sensor {
        return Sensor(reading: Builtin.zeroInitializer())
    }

    func isReset() -> Builtin.Int1 {
        return Builtin.cmp_eq_Int64(reading, Builtin.zeroInitializer())
    }
}

func testDefaults(_ s: Sensor) {
    let _ = s.doubleTag()
    let _ = s.isZeroTag()
    let _ = s.resetAndTag()
}
