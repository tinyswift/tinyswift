// RUN: %target-swift-frontend -typecheck -enable-experimental-feature Embedded -parse-stdlib %s
// REQUIRES: swift_feature_Embedded

// TinySwift smoke test: enums and pattern matching.

enum Direction {
    case north
    case south
    case east
    case west
}

struct Compass {
    var heading: Direction
}

func opposite(_ d: Direction) -> Direction {
    switch d {
    case .north: return .south
    case .south: return .north
    case .east:  return .west
    case .west:  return .east
    }
}
