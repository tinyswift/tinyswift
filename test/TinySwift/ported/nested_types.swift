// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/type/nested_types.swift — nested struct and enum types

struct Outer {
    struct Inner {
        var value: Builtin.Int32
    }

    var inner: Inner

    enum Kind {
        case typeA
        case typeB
    }

    var kind: Kind
}

func makeOuter(_ v: Builtin.Int32) -> Outer {
    return Outer(
        inner: Outer.Inner(value: v),
        kind: .typeA
    )
}

func getInner(_ o: Outer) -> Outer.Inner {
    return o.inner
}

func getKind(_ o: Outer) -> Outer.Kind {
    return o.kind
}

struct Matrix {
    struct Row {
        var col0: Builtin.Int64
        var col1: Builtin.Int64
    }

    var row0: Row
    var row1: Row
}

func makeMatrix(_ a: Builtin.Int64, _ b: Builtin.Int64,
                _ c: Builtin.Int64, _ d: Builtin.Int64) -> Matrix {
    return Matrix(
        row0: Matrix.Row(col0: a, col1: b),
        row1: Matrix.Row(col0: c, col1: d)
    )
}

struct Tree {
    enum Node {
        case leaf(Builtin.Int64)
        case branch(Builtin.Int64, Builtin.Int64)
    }
    var root: Node
}

func describeNode(_ t: Tree) -> Builtin.Int64 {
    switch t.root {
    case .leaf(let v):
        return v
    case .branch(let l, _):
        return l
    }
}
