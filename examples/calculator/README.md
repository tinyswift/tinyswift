# Stack-Based Calculator — TinySwift Language Features

A stack-based (RPN) calculator that demonstrates how structs, enums, and
generics work together in TinySwift without a standard library.

## How it works

### Enums: Operation type

Operations are modeled as a simple enum with four cases:

```swift
enum Operation {
    case add
    case sub
    case mul
    case div
}

func apply(_ op: Operation, _ a: Builtin.Int32, _ b: Builtin.Int32) -> Builtin.Int32 {
    switch op {
    case .add: return Builtin.add_Int32(a, b)
    case .sub: return Builtin.sub_Int32(a, b)
    case .mul: return Builtin.mul_Int32(a, b)
    case .div: return Builtin.sdiv_Int32(a, b)
    }
}
```

TinySwift supports enums with and without associated values. The compiler
lays them out as tagged unions with no runtime metadata overhead.

### Structs: Fixed-size stack

Since TinySwift has no heap allocation and no `Array` type, the stack is
implemented as a struct with 8 individual `Builtin.Int32` fields plus a
`top` counter:

```swift
struct Stack {
    var s0: Builtin.Int32
    var s1: Builtin.Int32
    // ... s2 through s7
    var top: Builtin.Int32

    func push(_ value: Builtin.Int32) -> Stack { ... }
    func pop() -> (Stack, Builtin.Int32) { ... }
}
```

The stack is immutable-by-default: `push` and `pop` return new `Stack`
values. This is idiomatic value-type programming and maps directly to
TinySwift's ownership model (no reference counting needed).

### Generics: Pair and swap

A generic `Pair<A, B>` struct and `swapPair` function demonstrate compile-time
monomorphization:

```swift
struct Pair<A, B> {
    var first: A
    var second: B
}

func swapPair<A, B>(_ p: Pair<A, B>) -> Pair<B, A> {
    return Pair(first: p.second, second: p.first)
}
```

TinySwift requires that all generic code is fully specialized at compile time.
There is no runtime type metadata or witness tables -- the compiler generates
a concrete version of every generic function for each type combination used.

### Integer printing

Since there is no `String` type or `print()` function, integer output is done
by extracting digits with repeated division and emitting each ASCII character
via `putchar`:

```swift
let remainder = Builtin.srem_Int32(val, ten)
let digit = Builtin.add_Int8(asciiZero, Builtin.trunc_Int32_Int8(remainder))
```

## Build

```bash
make
```

## Run

```bash
make run
```

Expected output:
```
10 + 20 = 30
30 * 3 = 90
90 - 7 = 83
83 / 4 = 20
HALT
```

## Key concepts

- **Enums with switch**: Pattern matching compiles to efficient jump tables
- **Value-type structs**: No heap, no ARC -- stack values are copied by value
- **Generics**: Fully monomorphized at compile time, zero runtime overhead
- **Tuple returns**: `func pop() -> (Stack, Builtin.Int32)` returns multiple values
- **Builtin arithmetic**: `add_Int32`, `sub_Int32`, `mul_Int32`, `sdiv_Int32`, `srem_Int32`
- **Type conversion**: `Builtin.trunc_Int32_Int8` truncates wider integers to narrower ones
