// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test struct methods, static methods, and computed properties

struct Vec3 {
    var x: Builtin.Int64
    var y: Builtin.Int64
    var z: Builtin.Int64

    func lengthSquared() -> Builtin.Int64 {
        let xx = Builtin.mul_Int64(x, x)
        let yy = Builtin.mul_Int64(y, y)
        let zz = Builtin.mul_Int64(z, z)
        return Builtin.add_Int64(Builtin.add_Int64(xx, yy), zz)
    }

    func scaled(_ s: Builtin.Int64) -> Vec3 {
        return Vec3(x: Builtin.mul_Int64(x, s),
                    y: Builtin.mul_Int64(y, s),
                    z: Builtin.mul_Int64(z, s))
    }

    static func zero() -> Vec3 {
        return Vec3(x: Builtin.zeroInitializer(),
                    y: Builtin.zeroInitializer(),
                    z: Builtin.zeroInitializer())
    }

    var isOrigin: Builtin.Int1 {
        let zeroVal: Builtin.Int64 = Builtin.zeroInitializer()
        let xz = Builtin.cmp_eq_Int64(x, zeroVal)
        let yz = Builtin.cmp_eq_Int64(y, zeroVal)
        let zz = Builtin.cmp_eq_Int64(z, zeroVal)
        return Builtin.and_Int1(Builtin.and_Int1(xz, yz), zz)
    }
}

func testMethods(_ v: Vec3, _ s: Builtin.Int64) {
    let _ = v.lengthSquared()
    let _ = v.scaled(s)
    let _ = Vec3.zero()
    let _ = v.isOrigin
}
