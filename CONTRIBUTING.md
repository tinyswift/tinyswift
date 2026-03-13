# Contributing to TinySwift

TinySwift is a value-type-only Swift compiler fork for embedded systems, based on `swiftlang/swift` at `swift-6.1-RELEASE`.

## Building

### Prerequisites

- CMake 3.24+
- Ninja 1.10+
- Python 3.8+
- LLVM/Clang 17.x (pre-built or from source)

For cross-compilation targets:
- ARM GNU Toolchain (for `aarch64-none-elf`)
- xPack RISC-V Toolchain (for `riscv32-none-elf`)
- WASI SDK (for `wasm32-unknown-wasi`)

### Quick Start

```bash
# Configure (debug build)
cmake --preset tinyswift-debug \
  -DCMAKE_C_COMPILER=/path/to/clang \
  -DCMAKE_CXX_COMPILER=/path/to/clang++ \
  -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm

# Build
cmake --build --preset tinyswift-debug
```

### CMake Presets

| Preset | Description |
|--------|-------------|
| `tinyswift-debug` | Development build with assertions and debug info |
| `tinyswift-release` | Optimized release build for distribution |
| `tinyswift-embedded-arm64` | Cross-compilation for aarch64-none-elf bare-metal |
| `tinyswift-embedded-riscv` | Cross-compilation for riscv32-none-elf bare-metal |
| `tinyswift-embedded-wasm` | Cross-compilation for wasm32-unknown-wasi |

### TINYSWIFT_BUILD Flag

All TinySwift-specific changes are gated behind `TINYSWIFT_BUILD=ON` (set automatically by presets). This defines the following C preprocessor macros:

| Macro | Purpose |
|-------|---------|
| `TINYSWIFT=1` | Master guard for all TinySwift-specific code |
| `TINYSWIFT_NO_CLASSES=1` | Guards for class-related code removal |
| `TINYSWIFT_NO_OBJC=1` | Guards for Objective-C interop removal |
| `TINYSWIFT_NO_ARC=1` | Guards for ARC removal |
| `TINYSWIFT_NO_RUNTIME_METADATA=1` | Guards for runtime metadata removal |

In C++ code, use `#ifndef TINYSWIFT` / `#endif` to guard code that depends on excluded modules.

## Architecture

TinySwift excludes 21 `lib/` directories from stock Swift (ClangImporter, IDE, Immediate, etc.) via `#ifndef TINYSWIFT` compile-time guards and `if(NOT TINYSWIFT_BUILD)` in CMakeLists.txt. Stub implementations (e.g., `ClangImporterStubs.cpp`) provide linking symbols for excluded modules. This approach preserves the ability to diff against upstream Swift for future cherry-picks.

## Testing

### Test Directory Structure

```
test/TinySwift/
├── smoke/     # Minimal compilation smoke tests
├── reject/    # Negative tests (must produce expected errors)
├── valid/     # Positive tests (must compile clean)
└── ported/    # Tests ported from the upstream Swift test suite
```

### Running Tests

```bash
# Run TinySwift smoke tests
for f in test/TinySwift/smoke/*.swift; do
  build/tinyswift-debug/bin/swift-frontend -typecheck \
    -enable-experimental-feature Embedded "$f"
done

# Run with lit (when available)
python3 llvm-project/llvm/utils/lit/lit.py -sv test/TinySwift/
```

## Verifying Changes

After making changes, verify no forbidden runtime symbols appear in output:

```bash
./scripts/check-forbidden-symbols.sh output.o
```

Forbidden symbols include `swift_retain`, `swift_release`, `swift_allocObject`, `objc_msgSend`, and others listed in the script. This check runs automatically in CI on every PR.
