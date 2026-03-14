# TinySwift for Embedded C Developers

This guide maps familiar C patterns to TinySwift equivalents. If you've been writing embedded C for microcontrollers, you already understand the execution model — TinySwift compiles to the same kind of flat binaries with no heap, no GC, and no runtime overhead.

## Why TinySwift Instead of C?

TinySwift gives you:

- **Type safety** — no implicit integer conversions, no void pointer casts without explicit intent
- **Enums with associated data** — tagged unions without manual tag management
- **Generics** — zero-cost type-parametric code (monomorphized, no vtables)
- **Ownership checking** — use-after-move and double-free detected at compile time
- **Same binary size** — TinySwift binaries are competitive with hand-written C

What you give up: inline assembly (use C support files), preprocessor macros (use generics), and union types (use enums).

## Structs

C structs map directly to TinySwift structs with methods attached:

**C:**
```c
typedef struct {
    int32_t x;
    int32_t y;
} Point;

int32_t point_distance_sq(const Point *a, const Point *b) {
    int32_t dx = a->x - b->x;
    int32_t dy = a->y - b->y;
    return dx * dx + dy * dy;
}

void point_translate(Point *p, int32_t dx, int32_t dy) {
    p->x += dx;
    p->y += dy;
}
```

**TinySwift:**
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

Key differences:
- Methods live inside the struct definition
- `mutating` marks functions that modify `self` (equivalent to taking a non-const pointer)
- No manual memory management — structs are value types, copied on assignment

## Enums (Tagged Unions)

C tagged unions require manual tag management. TinySwift enums handle this automatically:

**C:**
```c
typedef enum { SHAPE_CIRCLE, SHAPE_RECT } ShapeTag;

typedef struct {
    ShapeTag tag;
    union {
        struct { int32_t radius; } circle;
        struct { int32_t w, h; } rect;
    };
} Shape;

int32_t shape_area(const Shape *s) {
    switch (s->tag) {
        case SHAPE_CIRCLE: return 3 * s->circle.radius * s->circle.radius;
        case SHAPE_RECT:   return s->rect.w * s->rect.h;
    }
    return 0;  // unreachable, but C doesn't know
}
```

**TinySwift:**
```swift
enum Shape {
  case circle(radius: Int32)
  case rectangle(width: Int32, height: Int32)
}

func area(_ shape: Shape) -> Int32 {
  switch shape {
  case .circle(let r):
    return 3 * r * r
  case .rectangle(let w, let h):
    return w * h
  }
  // No default needed — compiler verifies exhaustiveness
}
```

Benefits:
- Compiler ensures you handle all cases (exhaustive switches)
- No possibility of reading the wrong union member
- Associated data is type-safe per case

## Pointers and MMIO Register Access

TinySwift provides the same pointer types you use in C, with explicit mutability:

**C:**
```c
#define UART0_BASE 0x09000000
#define UART0_DR   (*(volatile uint8_t *)(UART0_BASE + 0x00))
#define UART0_FR   (*(volatile uint8_t *)(UART0_BASE + 0x18))

void uart_putc(char c) {
    while (UART0_FR & 0x20) {}  // Wait for TX FIFO not full
    UART0_DR = c;
}
```

**TinySwift:**
```swift
struct UART {
  let base: UnsafeMutablePointer<UInt8>

  init(base: UInt) {
    self.base = UnsafeMutablePointer<UInt8>(bitPattern: base)!
  }

  var dataRegister: UnsafeMutablePointer<UInt8> {
    return base
  }

  var flagRegister: UnsafeMutablePointer<UInt8> {
    return base + 0x18
  }

  func putc(_ c: UInt8) {
    while flagRegister.pointee & 0x20 != 0 {}
    dataRegister.pointee = c
  }
}

let uart = UART(base: 0x0900_0000)
uart.putc(0x41)  // 'A'
```

### Pointer Type Mapping

| C | TinySwift |
|---|-----------|
| `T *` | `UnsafeMutablePointer<T>` |
| `const T *` | `UnsafePointer<T>` |
| `void *` | `UnsafeMutableRawPointer` |
| `const void *` | `UnsafeRawPointer` |

### Register Struct Pattern

For complex peripherals, wrap registers in a struct:

```swift
struct GPIORegisters {
  let base: UnsafeMutablePointer<UInt32>

  var mode: UnsafeMutablePointer<UInt32> { base + 0 }       // MODER
  var output: UnsafeMutablePointer<UInt32> { base + 5 }     // ODR

  func setOutput(pin: UInt32) {
    // Set pin to output mode (01 in 2-bit field)
    let shift = pin * 2
    var modeVal = mode.pointee
    modeVal = modeVal & ~(UInt32(3) << shift)
    modeVal = modeVal | (UInt32(1) << shift)
    mode.pointee = modeVal
  }

  func toggle(pin: UInt32) {
    output.pointee = output.pointee ^ (UInt32(1) << pin)
  }
}
```

## Interrupt Handlers

TinySwift does not have inline assembly or `__attribute__((interrupt))`. Interrupt handlers are written in C and call into Swift via `@_silgen_name`:

**C support file (`support.c`):**
```c
// Forward declaration of Swift handler
extern void swift_timer_handler(void);

void __attribute__((interrupt)) TIM2_IRQHandler(void) {
    // Clear interrupt flag
    *(volatile uint32_t *)(0x40000010) = 0;
    // Call Swift handler
    swift_timer_handler();
}
```

**TinySwift (`app.swift`):**
```swift
var tickCount: Int32 = 0

@_silgen_name("swift_timer_handler")
func timerHandler() {
  tickCount = tickCount + 1
}
```

This pattern keeps the minimum C in the interrupt vector setup while letting you write application logic in TinySwift.

## Linker Scripts

TinySwift uses standard GNU LD linker scripts, identical to what you use in C embedded projects.

### Minimal Linker Script (QEMU virt)

```ld
MEMORY
{
   ram : ORIGIN = 0x40000000, LENGTH = 128K
}

SECTIONS
{
   .text     : { *(.start*) ; *(.text*) } > ram
   .rodata   : { *(.rodata*) } > ram
   .data     : { *(.data*) } > ram
   .bss      : { *(.bss*) } > ram
   /DISCARD/ : { *(.swift_modhash*) *(.comment*) *(.note*) }
}
```

Key points:
- `.start` section comes first (contains the startup code from `support.c`)
- `/DISCARD/` removes Swift module hash sections and comments
- Standard `.text`, `.rodata`, `.data`, `.bss` layout — nothing Swift-specific

### For Real Hardware

Adapt the `MEMORY` block to match your MCU's flash and RAM regions:

```ld
MEMORY
{
   flash : ORIGIN = 0x08000000, LENGTH = 256K
   ram   : ORIGIN = 0x20000000, LENGTH = 64K
}

SECTIONS
{
   .text     : { *(.start*) ; *(.text*) ; *(.rodata*) } > flash
   .data     : AT(ADDR(.text) + SIZEOF(.text)) { *(.data*) } > ram
   .bss      : { *(.bss*) } > ram
   /DISCARD/ : { *(.swift_modhash*) *(.comment*) *(.note*) }
}
```

## Binary Size Comparison: C vs TinySwift

TinySwift targets the same binary sizes as equivalent C code. Here are the budgets enforced by CI:

| Program | Budget | What It Tests |
|---------|--------|---------------|
| Empty program | < 200 B | Baseline overhead |
| Arithmetic | < 500 B | Integer math operations |
| Structs | < 500 B | Struct creation, methods, mutation |
| Enums | < 800 B | Enum switch, associated values |
| Generics | < 1000 B | Monomorphized generic functions |

These are `.text` section sizes measured with `llvm-size` across all three targets (ARM64, RISC-V 32, WebAssembly).

### Why TinySwift Matches C

1. **No runtime** — there are no hidden runtime calls. Every instruction in the binary comes from your code.
2. **Monomorphized generics** — generic functions are specialized at compile time, producing the same code as if you wrote separate functions by hand.
3. **`--gc-sections`** — the linker removes all unused functions and data, just like in C.
4. **`-Osize`** — LLVM's size-optimizing passes run on TinySwift output identically to C output.

### Measuring Your Own Binaries

```bash
# Compile to object
swift-frontend -emit-object -target aarch64-none-elf \
  -enable-experimental-feature Embedded \
  -enable-experimental-feature TinySwift \
  -disable-objc-interop -parse-stdlib -parse-as-library -Osize \
  -module-name myapp myapp.swift -o myapp.o

# Check .text size
llvm-size myapp.o

# Verify no hidden runtime symbols
nm myapp.o | grep -E 'swift_retain|swift_release|swift_allocObject'
# (should produce no output)
```

## Project Structure for a TinySwift Embedded Project

A typical TinySwift embedded project looks like:

```
my-project/
  src/
    main.swift          # Application entry point
    peripherals.swift   # Hardware abstraction (UART, GPIO, etc.)
  support/
    support.c           # Startup code, interrupt vectors
    linkerscript.ld     # Memory layout
  Makefile
```

### Minimal Makefile

```makefile
TARGET     = aarch64-none-elf
SWIFT      = swift-frontend
CLANG      = clang
LLD        = ld.lld
SWIFTFLAGS = -emit-object -target $(TARGET) \
             -enable-experimental-feature Embedded \
             -enable-experimental-feature TinySwift \
             -disable-objc-interop -parse-stdlib -parse-as-library -Osize

app.elf: main.o support.o
	$(LLD) -T support/linkerscript.ld -e start --gc-sections $^ -o $@

main.o: src/main.swift
	$(SWIFT) $(SWIFTFLAGS) -module-name app $< -o $@

support.o: support/support.c
	$(CLANG) -target $(TARGET) -ffreestanding -nostdlib -O2 -c $< -o $@

clean:
	rm -f *.o *.elf
```

## Quick Reference: C to TinySwift

| C Pattern | TinySwift Equivalent |
|-----------|---------------------|
| `#define CONST 42` | `let CONST: Int32 = 42` |
| `typedef struct { ... } Foo;` | `struct Foo { ... }` |
| `enum { A, B, C };` | `enum MyEnum { case a, b, c }` |
| `union { int i; float f; }` | `enum Value { case int(Int32); case float(Float) }` |
| `void foo(Foo *p)` | `func foo(_ p: inout Foo)` |
| `const Foo *p` | `func foo(_ p: Foo)` (borrowed by default) |
| `volatile uint32_t *reg` | `UnsafeMutablePointer<UInt32>` |
| `memcpy(dst, src, n)` | `UnsafeMutableRawPointer` methods, or `@_silgen_name("memcpy")` |
| `static int count = 0;` | `var count: Int32 = 0` (module-level) |
| `sizeof(Foo)` | `MemoryLayout<Foo>.size` |
| `__attribute__((section(".start")))` | Write in C support file, call Swift via `@_silgen_name` |
