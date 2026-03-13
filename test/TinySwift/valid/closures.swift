// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

struct Transformer<T, U> {
    var transform: (T) -> U
}

func apply<T, U>(_ t: Transformer<T, U>, _ value: T) -> U {
    return t.transform(value)
}
