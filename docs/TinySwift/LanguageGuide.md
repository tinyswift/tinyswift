# TinySwift Language Guide

TinySwift is a strict subset of Swift designed for embedded systems. It eliminates all runtime overhead (ARC, metadata, heap allocation) by restricting the language to value types with compile-time-resolved generics and move/borrow ownership.

## Supported Types

### Structs

Structs are the primary data type in TinySwift. They behave identically to Swift structs.

```swift
struct Point {
  var x: Int32
  var y: Int32

  func distanceSquared(to other: Point) -> Int32 {
    let dx = x - other.x
    let dy = y - other.y
    return dx * dx + dy * dy
  }

  mutating func translate(dx: Int32, dy: Int32) {
    x = x + dx
    y = y + dy
  }
}
```

### Enums

Enums with associated values are supported. Indirect enums are not (they require heap allocation).

```swift
enum Shape {
  case circle(radius: Int32)
  case rectangle(width: Int32, height: Int32)
  case triangle(base: Int32, height: Int32)
}

func area(_ shape: Shape) -> Int32 {
  switch shape {
  case .circle(let r):
    return 3 * r * r  // approximate
  case .rectangle(let w, let h):
    return w * h
  case .triangle(let b, let h):
    return (b * h) / 2
  }
}
```

### Tuples

Tuples work as in standard Swift:

```swift
func divmod(_ a: Int32, _ b: Int32) -> (quotient: Int32, remainder: Int32) {
  return (a / b, a % b)
}
```

### Optionals

Optionals are sugar for `Optional<T>` enum and work normally:

```swift
func find(_ value: Int32, in array: UnsafePointer<Int32>, count: Int32) -> Int32? {
  var i: Int32 = 0
  while i < count {
    if (array + Int(i)).pointee == value {
      return i
    }
    i = i + 1
  }
  return nil
}
```

### Primitive Types

TinySwift provides these types via `Builtins.swift`:

| Type | Size | Description |
|------|------|-------------|
| `Bool` | 1 bit | Boolean value |
| `Int8`, `Int16`, `Int32`, `Int64` | 8-64 bits | Signed integers |
| `UInt8`, `UInt16`, `UInt32`, `UInt64` | 8-64 bits | Unsigned integers |
| `Float` | 32 bits | IEEE 754 single precision |
| `Double` | 64 bits | IEEE 754 double precision |
| `Int` | Platform width | Pointer-sized signed integer |
| `UInt` | Platform width | Pointer-sized unsigned integer |

All primitive types conform to `Equatable`, `Comparable`, `Hashable`, `BitwiseCopyable`, and `Sendable`.

## Protocols as Generic Constraints

TinySwift supports protocols, but only as generic constraints — not as existential types. This means you write `func foo<T: MyProtocol>(_ x: T)` (resolved at compile time) rather than `func foo(_ x: any MyProtocol)` (requires runtime dispatch).

```swift
protocol Drawable {
  func draw()
}

// This works — T is resolved at compile time:
func render<T: Drawable>(_ item: T) {
  item.draw()
}

// This does NOT work — existentials are rejected:
// func render(_ item: any Drawable)  // ERROR
```

### Protocol Extensions and Default Implementations

```swift
protocol HasArea {
  var area: Int32 { get }
}

extension HasArea {
  var isLarge: Bool {
    return area > 1000
  }
}
```

### Associated Types

```swift
protocol Container {
  associatedtype Element
  var count: Int32 { get }
  func element(at index: Int32) -> Element
}
```

### Protocol Composition

```swift
func process<T: Equatable & Comparable>(_ a: T, _ b: T) -> Bool {
  return a < b
}
```

## Generics

All generics in TinySwift are fully monomorphized at compile time. The compiler creates a specialized copy of every generic function for each concrete type it is called with. This eliminates all runtime type metadata and witness table lookups.

```swift
func max<T: Comparable>(_ a: T, _ b: T) -> T {
  return a > b ? a : b
}

// When called:
let x = max(Int32(10), Int32(20))  // Compiler generates max_Int32
let y = max(Float(1.0), Float(2.0))  // Compiler generates max_Float
```

### Generic Structs

```swift
struct Pair<A, B> {
  var first: A
  var second: B
}

let p = Pair<Int32, Bool>(first: 42, second: true)
```

### Where Clauses

```swift
func findFirst<C: Container>(in container: C, where predicate: (C.Element) -> Bool) -> C.Element?
    where C.Element: Equatable
{
  var i: Int32 = 0
  while i < container.count {
    let elem = container.element(at: i)
    if predicate(elem) {
      return elem
    }
    i = i + 1
  }
  return nil
}
```

### Monomorphization Guarantee

TinySwift enforces that all generic functions are fully specialized before code generation. If a generic function cannot be specialized (e.g., it is called with a type that is itself generic and unresolved), the compiler will emit an error. This is verified by `TinySwiftGenericSpecializationVerifier`.

## Ownership: Move, Borrow, Consume, Inout

TinySwift preserves Swift's OSSA (Ownership SSA) form through code generation instead of lowering it into ARC retain/release calls. This gives you explicit control over value lifetimes.

### Borrowing (Default)

By default, function parameters are borrowed — the caller retains ownership and the callee gets a read-only reference:

```swift
func length(_ p: Point) -> Int32 {  // p is borrowed
  return p.x * p.x + p.y * p.y
}
```

### Consuming

Use `consuming` to transfer ownership into a function. The caller cannot use the value after the call:

```swift
func process(consuming value: Point) -> Int32 {
  // value is consumed — caller gives up ownership
  return value.x + value.y
}
```

### Inout

Use `inout` for mutation through a mutable borrow:

```swift
func increment(_ p: inout Point) {
  p.x = p.x + 1
  p.y = p.y + 1
}

var origin = Point(x: 0, y: 0)
increment(&origin)
```

### Move Semantics

For `~Copyable` types, the compiler tracks ownership statically and rejects use-after-move:

```swift
struct UniqueHandle: ~Copyable {
  var fd: Int32

  consuming func close() {
    // fd is consumed, handle cannot be used after this
  }
}

func example() {
  var h = UniqueHandle(fd: 42)
  h.close()
  // h.close()  // ERROR: use after consume
}
```

### How This Differs from Standard Swift

In standard Swift, ownership is managed automatically through ARC (reference counting). In TinySwift:

- There is no runtime reference counting
- The compiler statically verifies all ownership transfers
- `~Copyable` types get compile-time move checking via `MoveOnlyChecker`
- All non-trivial types go through the ownership checker in TinySwift mode
- OSSA instructions (`begin_borrow`, `end_borrow`, `copy_value`, `destroy_value`) are handled directly in IRGen

## Typed Throws

TinySwift supports typed throws but not bare `throws` (which requires existential `any Error`):

```swift
enum ParseError {
  case invalidCharacter(UInt8)
  case unexpectedEnd
}

func parse(_ input: UInt8) throws(ParseError) -> Int32 {
  if input < 0x30 || input > 0x39 {
    throw ParseError.invalidCharacter(input)
  }
  return Int32(input) - 0x30
}

func tryParse(_ input: UInt8) -> Int32 {
  do {
    return try parse(input)
  } catch .invalidCharacter(_) {
    return -1
  } catch .unexpectedEnd {
    return -2
  }
}
```

Bare `throws` is rejected because `any Error` requires existential containers and runtime type metadata.

## C Interop via `@_silgen_name`

TinySwift interfaces with C code using `@_silgen_name` to declare external functions by their linker symbol name. There is no ClangImporter or bridging header support.

### Calling C Functions

```swift
// Declare a C function by its symbol name
@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

// Call it from Swift
_ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x41 as Builtin.Int8))  // 'A'
```

### Exporting Swift Functions to C

```swift
// This Swift function will be visible to the linker as "swift_handler"
@_silgen_name("swift_handler")
func handler(_ code: Builtin.Int32) -> Builtin.Int32 {
  return code
}
```

### Entry Point

The `main` function is declared with `@_silgen_name("main")` so the linker resolves it:

```swift
@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  // program logic
  return Builtin.zeroInitializer()
}
```

### Type Mapping

| C Type | TinySwift `Builtin.*` | TinySwift Stdlib |
|--------|----------------------|------------------|
| `int` / `int32_t` | `Builtin.Int32` | `Int32` |
| `char` / `uint8_t` | `Builtin.Int8` | `UInt8` |
| `void *` | `Builtin.RawPointer` | `UnsafeMutableRawPointer` |
| `const void *` | `Builtin.RawPointer` | `UnsafeRawPointer` |
| `size_t` | `Builtin.Word` | `Int` / `UInt` |

## Memory-Mapped I/O

For embedded targets, hardware registers are accessed through memory-mapped I/O using pointer types:

```swift
// Write to a memory-mapped register
let uartBase = UnsafeMutablePointer<UInt8>(
  bitPattern: UInt(Builtin.inttoptr_Word(0x0900_0000 as Builtin.Word))
)
uartBase?.pointee = 0x41  // Write 'A' to UART data register

// Read from a register
let status = uartBase?.pointee
```

Or using `Builtin` intrinsics directly for maximum control:

```swift
// Direct volatile store
let addr = Builtin.inttoptr_Word(0x0900_0000 as Builtin.Word)
Builtin.store_volatile_Int8(0x41 as Builtin.Int8, addr)
```

## What's Different from Standard Swift

| Feature | Standard Swift | TinySwift |
|---------|---------------|-----------|
| Classes | Supported | Rejected |
| Actors | Supported | Rejected |
| `any Protocol` (existentials) | Supported | Rejected |
| `async`/`await` | Supported | Rejected |
| Indirect enums | Supported | Rejected |
| Dynamic casts (`as?`, `is`) | Supported | Rejected |
| ARC (retain/release) | Automatic | Eliminated |
| Runtime type metadata | Present | Stripped |
| Generic dispatch | Via witness tables | Monomorphized |
| Macros | Supported | Rejected |
| ObjC interop | Supported | Rejected |
| `open` access level | Supported | Rejected |
| Bare `throws` | Supported | Rejected (use typed throws) |
| C interop | ClangImporter | `@_silgen_name` only |
| Heap allocation | Automatic | Traps by default |
| Standard library | Full stdlib | Minimal `Builtins.swift` |

### Why These Restrictions?

Every rejected feature requires either:

1. **Heap allocation** — classes, actors, indirect enums, and existential containers all allocate on the heap
2. **Runtime metadata** — dynamic casts, existentials, and witness tables require type metadata at runtime
3. **Reference counting** — classes and actors require ARC retain/release calls
4. **Runtime support code** — async/await requires a cooperative scheduler, macros require a plugin host

TinySwift's goal is zero-overhead embedded binaries. Every byte of output must come from your source code, not from runtime infrastructure.
