// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test protocol composition constraints in generics

protocol Readable {
    func read() -> Builtin.Int64
}

protocol Writable {
    func written(_ v: Builtin.Int64) -> Self
}

protocol Identifiable {
    func id() -> Builtin.Int32
}

func readAndWrite<T: Readable & Writable>(_ item: T) -> T {
    let v = item.read()
    return item.written(v)
}

func readWithId<T: Readable & Identifiable>(_ item: T) -> (Builtin.Int64, Builtin.Int32) {
    return (item.read(), item.id())
}

func tripleConstraint<T: Readable & Writable & Identifiable>(_ item: T) -> T {
    let _ = item.id()
    return readAndWrite(item)
}

struct Register: Readable, Writable, Identifiable {
    var value: Builtin.Int64
    var regId: Builtin.Int32

    func read() -> Builtin.Int64 { return value }
    func written(_ v: Builtin.Int64) -> Register { return Register(value: v, regId: regId) }
    func id() -> Builtin.Int32 { return regId }
}

func testComposition(_ r: Register) {
    let _ = readAndWrite(r)
    let _ = readWithId(r)
    let _ = tripleConstraint(r)
}
