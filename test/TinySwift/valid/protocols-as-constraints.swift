// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Protocols used as generic constraints (not existentials) are allowed.

protocol Describable {
    func describe() -> Builtin.Int1
}

struct Wrapper<T: Describable> {
    var value: T
    func check() -> Builtin.Int1 {
        return value.describe()
    }
}

func process<T: Describable>(_ item: T) -> Builtin.Int1 {
    return item.describe()
}
