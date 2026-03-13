// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/decl/func/default-values.swift — default parameter values

struct Flags {
    var a: Builtin.Int1
    var b: Builtin.Int1
}

func zeroInt1() -> Builtin.Int1 {
    return Builtin.zeroInitializer()
}

func withDefaultBool(_ x: Builtin.Int1 = Builtin.zeroInitializer()) -> Builtin.Int1 {
    return x
}

func identity<T>(_ x: T) -> T { return x }

func withDefaultGenericCall(_ x: Builtin.Int1 = withDefaultBool()) -> Builtin.Int1 {
    return x
}

func multipleDefaults(
    _ a: Builtin.Int1 = Builtin.zeroInitializer(),
    _ b: Builtin.Int1 = Builtin.zeroInitializer()
) -> Flags {
    return Flags(a: a, b: b)
}

func testDefaults() {
    let _ = withDefaultBool()
    let _ = withDefaultGenericCall()
    let _ = multipleDefaults()
}

struct Config {
    var enabled: Builtin.Int1

    func withEnabled(_ e: Builtin.Int1 = Builtin.zeroInitializer()) -> Config {
        return Config(enabled: e)
    }
}
