// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test willSet/didSet property observers on structs

precedencegroup AssignmentPrecedence {
    assignment: true
}

struct Counter {
    var total: Builtin.Int64

    var count: Builtin.Int64 {
        willSet {
            let _ = newValue
        }
        didSet {
            total = Builtin.add_Int64(total, count)
        }
    }
}

struct Clamped {
    var limit: Builtin.Int64

    var value: Builtin.Int64 {
        didSet {
            // Re-clamp after set: just demonstrate the observer fires
            let _ = Builtin.cmp_slt_Int64(value, limit)
        }
    }
}

struct Tracker {
    var version: Builtin.Int32

    var data: Builtin.Int64 {
        willSet {
            version = Builtin.add_Int32(version, Builtin.trunc_Int64_Int32(Builtin.integerLiteral_Int64(1)))
        }
        didSet {
            let _ = data
        }
    }
}

func testObservers(_ v: Builtin.Int64) {
    var c = Counter(total: Builtin.zeroInitializer(), count: Builtin.zeroInitializer())
    c.count = v
    var cl = Clamped(limit: v, value: Builtin.zeroInitializer())
    cl.value = v
    var t = Tracker(version: Builtin.zeroInitializer(), data: Builtin.zeroInitializer())
    t.data = v
}
