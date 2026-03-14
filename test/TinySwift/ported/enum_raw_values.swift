// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test enums with cases, matching, and nested enums

enum Priority {
    case low
    case medium
    case high
}

func priorityToInt(_ p: Priority) -> Builtin.Int32 {
    switch p {
    case .low:    return Builtin.zeroInitializer()
    case .medium: return Builtin.trunc_Int64_Int32(Builtin.integerLiteral_Int64(1))
    case .high:   return Builtin.trunc_Int64_Int32(Builtin.integerLiteral_Int64(2))
    }
}

struct Task {
    enum Status {
        case pending
        case running
        case done
    }
    var status: Status
    var id: Builtin.Int64
}

func isDone(_ t: Task, _ yes: Builtin.Int1, _ no: Builtin.Int1) -> Builtin.Int1 {
    switch t.status {
    case .done:
        return yes
    case .pending, .running:
        return no
    }
}

func testEnums(_ p: Priority, _ t: Task, _ y: Builtin.Int1, _ n: Builtin.Int1) {
    let _ = priorityToInt(p)
    let _ = isDone(t, y, n)
}
