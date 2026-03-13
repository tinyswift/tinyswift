// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/decl/ext/extensions.swift — extensions on structs and enums

struct Point {
    var x: Builtin.Int64
    var y: Builtin.Int64
}

extension Point {
    func swapped() -> Point {
        return Point(x: y, y: x)
    }

    func withX(_ newX: Builtin.Int64) -> Point {
        return Point(x: newX, y: y)
    }

    func withY(_ newY: Builtin.Int64) -> Point {
        return Point(x: x, y: newY)
    }
}

enum Direction {
    case up, down, left, right
}

extension Direction {
    func isVertical(_ yes: Builtin.Int1, _ no: Builtin.Int1) -> Builtin.Int1 {
        switch self {
        case .up, .down:
            return yes
        case .left, .right:
            return no
        }
    }

    func opposite() -> Direction {
        switch self {
        case .up: return .down
        case .down: return .up
        case .left: return .right
        case .right: return .left
        }
    }
}

protocol Taggable {
    func tag() -> Builtin.Int64
}

extension Point: Taggable {
    func tag() -> Builtin.Int64 {
        return x
    }
}

extension Direction: Taggable {
    func tag() -> Builtin.Int64 {
        return Builtin.zeroInitializer()
    }
}
