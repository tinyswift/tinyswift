# Hello UART — TinySwift on AArch64 QEMU

The simplest TinySwift embedded example: prints "Hello, TinySwift!" to a serial
console via the PL011 UART on the QEMU `virt` machine.

## How it works

TinySwift programs have no standard library `print()` function. Instead, we use
`@_silgen_name("putchar")` to declare an external C function that writes a
single byte to the UART data register. The C implementation lives in
`utils/embedded-test-support/aarch64-qemu-virt/support.c` and performs a
volatile store to the PL011 UART0 address at `0x09000000`:

```c
int putchar(int c) {
    *(volatile uint8_t *)(0x09000000) = (uint8_t)c;
    return c;
}
```

On the Swift side, each ASCII character is emitted individually:

```swift
@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

func emit(_ byte: Builtin.Int8) {
    _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(byte))
}

emit(0x48 as Builtin.Int8)  // 'H'
```

The `@_silgen_name` attribute tells the Swift compiler to use the given symbol
name directly, bypassing Swift name mangling. This is the primary mechanism for
calling C code from TinySwift.

## Build

```bash
make
```

Requires `swift-frontend` (or `tinyswiftc`), `clang`, and `ld.lld` in your
PATH, or set via environment variables:

```bash
SWIFT_FRONTEND=/path/to/swift-frontend CLANG=clang LD=ld.lld make
```

## Run

```bash
make run
```

This launches QEMU with:
- Machine: `virt` (ARM64 virtual platform)
- CPU: `cortex-a57`
- Semihosting enabled (for clean exit via `halt`)
- No graphics (`-nographic`), output goes to terminal

Expected output:
```
Hello, TinySwift!
HALT
```

## Key concepts

- **@_silgen_name**: Links Swift functions to C symbols without mangling
- **Builtin.Int8 / Builtin.Int32**: Raw LLVM integer types (no standard library)
- **Builtin.zextOrBitCast_Int8_Int32**: Zero-extends an 8-bit value to 32-bit
- **MMIO via C**: Memory-mapped I/O is done in the C support layer
- **No heap, no ARC**: The entire program runs without dynamic allocation
