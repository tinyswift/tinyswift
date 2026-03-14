// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test struct layout with various field types, nested structs, and empty structs

struct Empty {}

struct SingleField {
    var x: Builtin.Int64
}

struct MultiField {
    var a: Builtin.Int32
    var b: Builtin.Int64
    var c: Builtin.Int1
}

struct Nested {
    var inner: SingleField
    var flag: Builtin.Int1
}

struct DeepNested {
    var outer: Nested
    var extra: Builtin.Int32
}

func makeEmpty() -> Empty { return Empty() }
func makeSingle(_ v: Builtin.Int64) -> SingleField { return SingleField(x: v) }
func makeMulti(_ a: Builtin.Int32, _ b: Builtin.Int64, _ c: Builtin.Int1) -> MultiField {
    return MultiField(a: a, b: b, c: c)
}
func makeNested(_ v: Builtin.Int64, _ f: Builtin.Int1) -> Nested {
    return Nested(inner: SingleField(x: v), flag: f)
}
func makeDeep(_ v: Builtin.Int64, _ f: Builtin.Int1, _ e: Builtin.Int32) -> DeepNested {
    return DeepNested(outer: Nested(inner: SingleField(x: v), flag: f), extra: e)
}
func accessDeep(_ d: DeepNested) -> Builtin.Int64 {
    return d.outer.inner.x
}
