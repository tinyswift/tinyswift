// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Non-indirect enums are allowed.

enum Direction {
    case north
    case south
    case east
    case west
}

enum Option<T> {
    case some(T)
    case none
}

func opposite(_ d: Direction) -> Direction {
    switch d {
    case .north: return .south
    case .south: return .north
    case .east:  return .west
    case .west:  return .east
    }
}
