// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/stmt/statements.swift — basic control flow
// Note: `if <Builtin.Int1>` crashes in -parse-stdlib mode (pre-existing compiler bug),
// so this test uses switch/enum patterns to exercise control flow.

struct Res {
    var flag: Builtin.Int1
}

enum Bool {
    case t
    case f
}

func testSwitch(_ c: Bool, _ a: Res, _ b: Res) -> Res {
    switch c {
    case .t:
        return a
    case .f:
        return b
    }
}

func testNestedSwitch(_ a: Bool, _ b: Bool,
                      _ r1: Res, _ r2: Res, _ r3: Res) -> Res {
    switch a {
    case .t:
        switch b {
        case .t:
            return r1
        case .f:
            return r2
        }
    case .f:
        return r3
    }
}

// Note: `while <Builtin.Int1>` and `repeat-while <Builtin.Int1>` crash the
// compiler in -parse-stdlib mode (pre-existing UnresolvedDeclRef verifier bug).
// while/repeat-while are tested via the smoke tests instead.

enum TwoCase {
    case first
    case second
}

func testSwitchWithinSwitch(_ t: TwoCase, _ a: Builtin.Int1) -> Builtin.Int1 {
    switch t {
    case .first:
        return a
    case .second:
        return a
    }
}

