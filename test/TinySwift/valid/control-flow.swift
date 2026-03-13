// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

enum Color {
    case red
    case green
    case blue
}

func classify(_ c: Color) -> Color {
    switch c {
    case .red:   return .red
    case .green: return .green
    case .blue:  return .blue
    }
}
