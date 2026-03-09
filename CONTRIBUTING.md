# Contributing to TinySwift

TinySwift is a value-type-only Swift compiler fork for embedded systems, based on `swiftlang/swift` at `swift-6.1-RELEASE`.

## Building

### Prerequisites

- CMake 3.24+
- Ninja 1.10+
- Python 3.8+
- LLVM/Clang 17.x (pre-built or from source)

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
| `tinyswift-debug` | Development build with assertions |
| `tinyswift-release` | Optimized release build |

### TINYSWIFT_BUILD Flag

All TinySwift-specific changes are gated behind `TINYSWIFT_BUILD=ON` (set automatically by presets). In C++ code, use `#ifndef TINYSWIFT` / `#endif` to guard code that depends on deleted modules.

## Architecture

TinySwift removes 21 `lib/` directories from stock Swift (ClangImporter, IDE, Immediate, etc.) and gates them behind `if(NOT TINYSWIFT_BUILD)` in CMakeLists.txt. Retained directories have `#include` guards for deleted headers.

## Verifying Changes

After making changes, verify no forbidden runtime symbols appear in output:

```bash
./scripts/check-forbidden-symbols.sh output.o
```

Forbidden symbols include `swift_retain`, `swift_release`, `swift_allocObject`, `objc_msgSend`, and others listed in the script.
