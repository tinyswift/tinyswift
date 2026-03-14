# Getting Started with TinySwift

TinySwift is a value-type-only Swift compiler fork for embedded systems. It compiles Swift to zero-overhead binaries for ARM64, RISC-V 32-bit, and WebAssembly targets with no ARC, no runtime metadata, and no heap allocation by default.

## Prerequisites

| Tool | Version | Purpose |
|------|---------|---------|
| CMake | 3.24+ | Build system |
| Ninja | 1.10+ | Build backend |
| Python | 3.8+ | Build scripts |
| LLVM/Clang | 17.x | C/C++ compiler and linker tools |

For cross-compilation and testing:

| Tool | Purpose |
|------|---------|
| `ld.lld` | Linker for ELF targets (ships with LLVM) |
| `wasm-ld` | Linker for WebAssembly targets (ships with LLVM) |
| QEMU | Running ARM64 and RISC-V binaries (`qemu-system-aarch64`, `qemu-system-riscv32`) |
| wasmtime | Running WebAssembly binaries |

### Installing Prerequisites

**macOS:**

```bash
brew install cmake ninja python3
# LLVM 17 — download pre-built from https://github.com/llvm/llvm-project/releases
```

**Ubuntu/Debian:**

```bash
sudo apt-get install cmake ninja-build python3 libncurses5-dev libedit-dev libxml2-dev
# For embedded testing:
sudo apt-get install qemu-system-arm qemu-system-misc lld
# wasmtime:
curl https://wasmtime.dev/install.sh -sSf | bash
```

## Building from Source

### 1. Clone the Repository

```bash
git clone https://github.com/tinyswift/tinyswift.git
cd tinyswift
```

### 2. Configure

TinySwift uses CMake presets. Point it at your LLVM installation:

```bash
cmake --preset tinyswift-debug \
  -DCMAKE_C_COMPILER=/path/to/clang \
  -DCMAKE_CXX_COMPILER=/path/to/clang++ \
  -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm
```

### 3. Build

```bash
cmake --build --preset tinyswift-debug
```

This produces `build/tinyswift-debug/bin/swift-frontend` and `build/tinyswift-debug/bin/tinyswiftc`.

### Available CMake Presets

| Preset | Description |
|--------|-------------|
| `tinyswift-debug` | Development build with assertions and debug info |
| `tinyswift-release` | Optimized release build for distribution |
| `tinyswift-embedded-arm64` | Cross-compilation for `aarch64-none-elf` |
| `tinyswift-embedded-riscv` | Cross-compilation for `riscv32-none-elf` |
| `tinyswift-embedded-wasm` | Cross-compilation for `wasm32-unknown-wasi` |

## Your First Program: Hello UART on QEMU ARM64

This walkthrough compiles a TinySwift program that prints "Hello, TinySwift!" via UART on a QEMU ARM64 virtual machine.

### 1. Write the Swift Source

Create `hello.swift`:

```swift
@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

@_silgen_name("halt")
func c_halt() -> Builtin.RawPointer

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x48 as Builtin.Int8)) // H
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x65 as Builtin.Int8)) // e
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6C as Builtin.Int8)) // l
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6C as Builtin.Int8)) // l
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6F as Builtin.Int8)) // o
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x2C as Builtin.Int8)) // ,
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x20 as Builtin.Int8)) //
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x54 as Builtin.Int8)) // T
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x69 as Builtin.Int8)) // i
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6E as Builtin.Int8)) // n
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x79 as Builtin.Int8)) // y
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x53 as Builtin.Int8)) // S
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x77 as Builtin.Int8)) // w
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x69 as Builtin.Int8)) // i
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x66 as Builtin.Int8)) // f
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x74 as Builtin.Int8)) // t
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x21 as Builtin.Int8)) // !
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x0A as Builtin.Int8)) // \n
  return Builtin.zeroInitializer()
}
```

### 2. Compile to Object File

```bash
build/tinyswift-debug/bin/swift-frontend -emit-object \
  -target aarch64-none-elf \
  -enable-experimental-feature Embedded \
  -enable-experimental-feature TinySwift \
  -disable-objc-interop -parse-stdlib -parse-as-library -Osize \
  -module-name hello \
  hello.swift -o hello.o
```

### 3. Compile C Support Code

TinySwift programs need a small C startup file that sets up the stack and provides platform I/O. Use the provided support code:

```bash
clang -target aarch64-none-elf -ffreestanding -nostdlib -O2 -c \
  utils/embedded-test-support/aarch64-qemu-virt/support.c -o support.o
```

### 4. Link

```bash
ld.lld -T utils/embedded-test-support/aarch64-qemu-virt/linkerscript.ld \
  -e start --gc-sections \
  support.o hello.o -o hello.elf
```

### 5. Run on QEMU

```bash
qemu-system-aarch64 -M virt -cpu cortex-a53 -nographic \
  -bios none -kernel hello.elf
```

You should see:

```
Hello, TinySwift!
HALT
```

Press `Ctrl-A X` to exit QEMU.

## Cross-Compilation for All Targets

### ARM64 Bare-Metal (`aarch64-none-elf`)

```bash
swift-frontend -emit-object -target aarch64-none-elf \
  -enable-experimental-feature Embedded \
  -enable-experimental-feature TinySwift \
  -disable-objc-interop -parse-stdlib -parse-as-library -Osize \
  -module-name myapp myapp.swift -o myapp.o

ld.lld -T linkerscript.ld -e start --gc-sections support.o myapp.o -o myapp.elf
```

### RISC-V 32-bit Bare-Metal (`riscv32-none-none-eabi`)

```bash
swift-frontend -emit-object -target riscv32-none-none-eabi \
  -enable-experimental-feature Embedded \
  -enable-experimental-feature TinySwift \
  -disable-objc-interop -parse-stdlib -parse-as-library -Osize \
  -module-name myapp myapp.swift -o myapp.o

ld.lld -T linkerscript.ld -e start --gc-sections support.o myapp.o -o myapp.elf
```

### WebAssembly WASI (`wasm32-unknown-wasi`)

```bash
swift-frontend -emit-object -target wasm32-unknown-wasi \
  -enable-experimental-feature Embedded \
  -enable-experimental-feature TinySwift \
  -disable-objc-interop -parse-stdlib -parse-as-library -Osize \
  -module-name myapp myapp.swift -o myapp.o

wasm-ld --no-entry --export-all --allow-undefined \
  --initial-memory=65536 --stack-first \
  support.o myapp.o -o myapp.wasm

wasmtime myapp.wasm
```

### Using `tinyswiftc` (Simplified Driver)

The `tinyswiftc` driver wraps `swift-frontend` with embedded-first defaults:

```bash
# Compile and link for ARM64
tinyswiftc --target aarch64-none-elf -o hello.elf hello.swift

# Compile only (emit object)
tinyswiftc --target aarch64-none-elf -emit-object -o hello.o hello.swift

# Emit LLVM IR for inspection
tinyswiftc --target wasm32-unknown-wasi -emit-ir hello.swift

# Default optimization is -Osize; override with:
tinyswiftc --target aarch64-none-elf -Onone hello.swift
```

## Toolchain Layout

When installed or extracted from a release tarball:

```
tinyswift-v0.1.0-<platform>/
  bin/
    tinyswiftc              # Simplified driver
    swift-frontend          # Full compiler frontend
  lib/swift/tinyswift/
    Builtins.swiftmodule    # TinySwift standard library module (when available)
    Runtime.o               # Per-target minimal C runtime
  share/tinyswift/
    linker-scripts/
      aarch64-qemu-virt/linkerscript.ld
      riscv32-qemu-virt/linkerscript.ld
    examples/               # Example projects
```

## Required Compiler Flags

Every TinySwift compilation requires these flags:

| Flag | Purpose |
|------|---------|
| `-enable-experimental-feature Embedded` | Enable Embedded Swift mode |
| `-enable-experimental-feature TinySwift` | Enable TinySwift value-type-only restrictions |
| `-disable-objc-interop` | No Objective-C runtime |
| `-parse-stdlib` | Use `Builtin.*` types directly |
| `-parse-as-library` | No implicit `@main` entry point |
| `-Osize` | Optimize for binary size (recommended) |
| `-target <triple>` | Target architecture |
| `-module-name <name>` | Module name for the compilation unit |

The `tinyswiftc` driver sets all of these automatically.

## Verifying Your Build

### Check for Forbidden Symbols

TinySwift binaries must contain zero ARC or runtime symbols:

```bash
./scripts/check-forbidden-symbols.sh myapp.o
```

### Run Binary Size Report

Verify benchmark binaries are within size budgets:

```bash
./scripts/binary-size-report.sh
```

Size budgets: empty < 200B, arithmetic < 500B, structs < 500B, enums < 800B, generics < 1000B.

## Troubleshooting

### `error: cannot find 'Builtin' in scope`

You forgot `-parse-stdlib`. TinySwift programs access hardware types via `Builtin.*` intrinsics.

### `undefined symbol: swift_retain` / `swift_release`

Your code is using a pattern that generates ARC calls. TinySwift eliminates ARC entirely. Ensure you are not using classes, existentials (`any Protocol`), or dynamic casts. Use structs, enums, and protocols-as-constraints instead.

### `error: classes are not supported in TinySwift`

TinySwift is value-type-only. Replace `class` with `struct`. If you need reference semantics, use `UnsafeMutablePointer`.

### Linker errors about missing `_start` or `main`

Your program needs a C support file that provides the entry point and stack setup. See `utils/embedded-test-support/` for platform-specific examples.

### QEMU hangs or produces no output

- Verify the UART address matches your QEMU machine model (PL011 at `0x09000000` for `-M virt`)
- Check the linker script RAM origin matches QEMU's memory map (`0x40000000` for virt)
- Ensure `--gc-sections` is not stripping your entry point (use `-e start`)

### Build takes too long

Use `sccache` for compiler caching:

```bash
cmake --preset tinyswift-debug \
  -DCMAKE_C_COMPILER_LAUNCHER=sccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER=sccache \
  ...
```

### Binary is too large

- Use `-Osize` (default for `tinyswiftc`)
- Check `--gc-sections` is enabled in your linker invocation
- Run `./scripts/binary-size-report.sh` to compare against budgets
- Avoid unnecessary generic instantiations — each monomorphized specialization adds code
