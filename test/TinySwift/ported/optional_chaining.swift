// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test optional types and chaining
// Note: Since -parse-stdlib provides no standard Optional, we define our own.

enum Optional<T> {
    case some(T)
    case none
}

struct Address {
    var street: Builtin.Int64
}

struct Person {
    var name: Builtin.Int64
    var addr: Optional<Address>
}

func getStreet(_ p: Person) -> Optional<Builtin.Int64> {
    switch p.addr {
    case .some(let a):
        return .some(a.street)
    case .none:
        return .none
    }
}

func flatMap<T, U>(_ opt: Optional<T>, _ f: (T) -> Optional<U>) -> Optional<U> {
    switch opt {
    case .some(let v):
        return f(v)
    case .none:
        return .none
    }
}

func map<T, U>(_ opt: Optional<T>, _ f: (T) -> U) -> Optional<U> {
    switch opt {
    case .some(let v):
        return .some(f(v))
    case .none:
        return .none
    }
}

func orDefault<T>(_ opt: Optional<T>, _ fallback: T) -> T {
    switch opt {
    case .some(let v): return v
    case .none: return fallback
    }
}

func testOptional(_ p: Person, _ fallback: Builtin.Int64) {
    let street = getStreet(p)
    let _ = orDefault(street, fallback)
    let _ = map(street) { v in v }
}
