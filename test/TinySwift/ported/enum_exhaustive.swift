// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Test exhaustive switch with enum patterns

enum Suit {
    case hearts, diamonds, clubs, spades
}

func suitIndex(_ s: Suit) -> Builtin.Int32 {
    switch s {
    case .hearts:   return Builtin.zeroInitializer()
    case .diamonds: return Builtin.trunc_Int64_Int32(Builtin.integerLiteral_Int64(1))
    case .clubs:    return Builtin.trunc_Int64_Int32(Builtin.integerLiteral_Int64(2))
    case .spades:   return Builtin.trunc_Int64_Int32(Builtin.integerLiteral_Int64(3))
    }
}

enum Instruction {
    case load(Builtin.Int64)
    case store(Builtin.Int64, Builtin.Int64)
    case halt
}

func describe(_ inst: Instruction, _ a: Builtin.Int64, _ b: Builtin.Int64) -> Builtin.Int64 {
    switch inst {
    case .load(let addr):
        return addr
    case .store(let addr, _):
        return addr
    case .halt:
        return Builtin.zeroInitializer()
    }
}

enum Comparison {
    case less, equal, greater
}

func fromCmp(_ v: Builtin.Int1, _ eq: Builtin.Int1) -> Comparison {
    switch (v, eq) {
    case (_, _):
        return .equal
    }
}
