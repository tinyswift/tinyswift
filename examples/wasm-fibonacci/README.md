# Fibonacci — TinySwift on WebAssembly/WASI

Computes and prints the first 16 Fibonacci numbers, targeting WebAssembly with
WASI (WebAssembly System Interface) for I/O.

## How it works

### WebAssembly target

TinySwift can compile to `wasm32-unknown-wasi`, producing a `.wasm` module that
runs on any WASI-compatible runtime (wasmtime, wasmer, wazero, browser with
WASI polyfill, etc.).

The compilation pipeline is:

```
fibonacci.swift  -->  swift-frontend  -->  fibonacci.o  (wasm object)
support.c        -->  clang           -->  support.o    (wasm object)
                                      -->  wasm-ld      -->  fibonacci.wasm
```

### WASI I/O

Since WebAssembly has no direct access to stdout, the C support layer in
`utils/embedded-test-support/wasm32-wasi/support.c` imports WASI system calls:

```c
__attribute__((import_module("wasi_snapshot_preview1")))
__attribute__((import_name("fd_write")))
int __wasi_fd_write(int fd, const void *iovs, int iovs_len, int *nwritten);
```

The Swift code calls `putchar` via `@_silgen_name`, which delegates to
`__wasi_fd_write` with file descriptor 1 (stdout).

### Fibonacci algorithm

The iterative Fibonacci function uses only `Builtin` types:

```swift
func fibonacci(_ n: Builtin.Int32) -> Builtin.Int32 {
    var prev = zero     // fib(i-2)
    var curr = one      // fib(i-1)
    var i = Builtin.integerLiteral_Int32(2)

    while Builtin.cmp_sle_Int32(i, n) != (Builtin.zeroInitializer() as Builtin.Int1) {
        let next = Builtin.add_Int32(prev, curr)
        prev = curr
        curr = next
        i = Builtin.add_Int32(i, one)
    }
    return curr
}
```

No heap allocation, no reference counting, no runtime metadata. The entire
program compiles to a few hundred bytes of WebAssembly.

## Build

```bash
make
```

Requires `swift-frontend` (or `tinyswiftc`), `clang` (with wasm32 target
support), and `wasm-ld` in your PATH:

```bash
SWIFT_FRONTEND=/path/to/swift-frontend make
```

## Run

```bash
make run
```

Or directly with any WASI runtime:

```bash
wasmtime fibonacci.wasm
wasmer fibonacci.wasm
```

Expected output:
```
fib(0) = 0
fib(1) = 1
fib(2) = 1
fib(3) = 2
fib(4) = 3
fib(5) = 5
fib(6) = 8
fib(7) = 13
fib(8) = 21
fib(9) = 34
fib(10) = 55
fib(11) = 89
fib(12) = 144
fib(13) = 233
fib(14) = 377
fib(15) = 610
HALT
```

## Key concepts

- **wasm32-unknown-wasi target**: TinySwift compiles to WebAssembly
- **wasm-ld linker**: Links `.o` wasm objects into a final `.wasm` module
- **WASI imports**: System calls (fd_write, proc_exit) are resolved at runtime by the host
- **--no-entry + --export=_start**: The C support provides `_start` which calls `main`
- **Iterative algorithms**: No recursion needed (though TinySwift does support it)
- **Zero-overhead**: No GC, no ARC, no runtime metadata in the wasm binary
