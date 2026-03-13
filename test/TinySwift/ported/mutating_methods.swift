// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/decl/func/functions.swift — mutating methods and value semantics

// In -parse-stdlib mode, AssignmentPrecedence is not available, so we cannot
// use assignment or mutating methods. Instead we test the same value-type
// method patterns using functional (non-mutating) approaches.

struct Counter {
    var count: Builtin.Int32

    func incremented(_ n: Builtin.Int32) -> Counter {
        return Counter(count: Builtin.add_Int32(count, n))
    }

    func decremented(_ n: Builtin.Int32) -> Counter {
        return Counter(count: Builtin.sub_Int32(count, n))
    }

    func isZero() -> Builtin.Int1 {
        return Builtin.cmp_eq_Int32(count, Builtin.zeroInitializer())
    }
}

struct Buffer {
    var start: Builtin.RawPointer
    var count: Builtin.Int64

    func advanced(_ n: Builtin.Int64) -> Buffer {
        return Buffer(start: start, count: Builtin.sub_Int64(count, n))
    }

    func isEmpty() -> Builtin.Int1 {
        return Builtin.cmp_eq_Int64(count, Builtin.zeroInitializer())
    }
}

func testValueSemantics(_ c: Counter, _ step: Builtin.Int32) {
    let c2 = c.incremented(step)
    let c3 = c2.incremented(step)
    let _ = c3.isZero()
}

func testBuffer(_ b: Buffer, _ n: Builtin.Int64) {
    let b2 = b.advanced(n)
    let _ = b2.isEmpty()
}
