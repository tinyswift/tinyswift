// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/decl/enum/enumtest.swift — enum associated values and pattern matching

enum Shape {
    case circle(radius: Builtin.Int64)
    case rectangle(width: Builtin.Int64, height: Builtin.Int64)
    case point
}

func extractRadius(_ s: Shape, _ fallback: Builtin.Int64) -> Builtin.Int64 {
    switch s {
    case .circle(let r):
        return r
    case .rectangle(let w, _):
        return w
    case .point:
        return fallback
    }
}

func isCircle(_ s: Shape, _ yes: Builtin.Int1, _ no: Builtin.Int1) -> Builtin.Int1 {
    switch s {
    case .circle:
        return yes
    default:
        return no
    }
}

enum Token {
    case number(Builtin.Int64)
    case op(Builtin.Int8)
    case end
}

func isEnd(_ t: Token, _ yes: Builtin.Int1, _ no: Builtin.Int1) -> Builtin.Int1 {
    switch t {
    case .end:
        return yes
    default:
        return no
    }
}

func extractNumber(_ t: Token, _ fallback: Builtin.Int64) -> Builtin.Int64 {
    switch t {
    case .number(let n):
        return n
    default:
        return fallback
    }
}

enum Optional<T> {
    case some(T)
    case none

    func map<U>(_ transform: (T) -> U) -> Optional<U> {
        switch self {
        case .some(let value):
            return .some(transform(value))
        case .none:
            return .none
        }
    }
}
