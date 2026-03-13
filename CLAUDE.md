# TinySwift

A value-type-only Swift compiler fork for embedded systems, based on `swiftlang/swift` at `swift-6.1-RELEASE`.

## Project Tracking

- **GitHub Project Board**: https://github.com/orgs/tinyswift/projects/1
- **Issues**: 63 total across 6 phases — use `gh issue list --label phase-N` to filter by phase
- Phase 0 (#1-12), Phase 1 (#13-18), and Phase 2 (#19-33) are complete; Phase 3-5 are open

## Research Documents

All design research lives in `../research/` (sibling to this repo):

| Doc | File | Purpose |
|-----|------|---------|
| 00 | `00_MASTER_REPORT.md` | Master synthesis — start here for full project context |
| 01 | `01_swift_compiler_audit.md` | Swift compiler audit: what to keep, modify, delete |
| 02 | `02_deletion_modification_plan.md` | File-by-file deletion & modification plan |
| 03 | `03_arc_removal_design.md` | ARC removal & ownership model design (Phase 2) |
| 04 | `04_existentials_metadata_builtins_design.md` | Existentials, metadata, builtins design (Phase 3) |
| 05 | `05_grammar_subset_spec.md` | Language subset grammar specification |
| 06 | `06_competitive_analysis.md` | Competitive analysis & prior art |
| 07 | `07_build_system_plan.md` | Build system & CMake plan |
| 08 | `08_risk_register.md` | Risk register with mitigations |
| 09 | `09_implementation_plan.md` | Full implementation plan & roadmap (all 6 phases, issue titles) |
| — | `tinyswift-builtins/` | Prototype Builtins.swift and Runtime.c |

## Architecture

- **Feature flag**: `EXPERIMENTAL_FEATURE(TinySwift, true)` in `Features.def` — implies Embedded mode
- **Sema rejections**: Classes, actors, existentials, async/await, indirect enums, bare throws, macros, dynamic casts, `open` access, ObjC attrs — all rejected in `TypeCheckDeclPrimary.cpp`, `TypeCheckAttr.cpp`, `TypeCheckType.cpp`, `MiscDiagnostics.cpp`
- **Diagnostics**: Defined in `DiagnosticsSema.def` and `DiagnosticsSIL.def` (search `tinyswift`)
- **Build guard**: `TINYSWIFT_BUILD=ON` CMake option; `#ifndef TINYSWIFT` guards in source instead of directory deletion
- **Compile flags**: `TINYSWIFT_NO_CLASSES`, `TINYSWIFT_NO_OBJC`, `TINYSWIFT_NO_ARC`, `TINYSWIFT_NO_RUNTIME_METADATA`
- **Phase 2 (ARC Removal)**: OSSA form preserved through IRGen, ARC passes guarded, MoveOnlyChecker extended to all non-trivial types, IRGen handles OSSA instructions directly

## Key Files

- `include/swift/Basic/Features.def` — TinySwift feature registration
- `include/swift/AST/DiagnosticsSema.def` — diagnostic messages
- `include/swift/AST/SILOptions.h` — `TinySwift` SIL flag
- `lib/Frontend/CompilerInvocation.cpp` — TinySwift→Embedded cascading
- `lib/Sema/TypeCheckDeclPrimary.cpp` — class/actor/enum/func/macro rejections
- `lib/Sema/TypeCheckAttr.cpp` — @objc, @IBOutlet, open access rejections
- `lib/Sema/TypeCheckType.cpp` — existential type rejection
- `lib/Sema/MiscDiagnostics.cpp` — dynamic cast rejection
- `lib/AST/FeatureSet.cpp` — `UNINTERESTING_FEATURE(TinySwift)`
- `CMakePresets.json` — 5 presets (debug, release, arm64, riscv, wasm)
- `CMakeLists.txt` — `TINYSWIFT_BUILD` option and compile flags
- `lib/SILOptimizer/Mandatory/TinySwiftVerifier.cpp` — safety net: asserts zero ARC/class instructions
- `lib/SILOptimizer/PassManager/PassPipeline.cpp` — ARC passes and OME guarded on `TinySwift`
- `lib/SILOptimizer/Mandatory/OwnershipModelEliminator.cpp` — bypassed in TinySwift (preserves OSSA)
- `lib/IRGen/IRGenSIL.cpp` — OSSA-aware visitors for TinySwift (begin_borrow, end_borrow, copy_value, etc.)
- `lib/SILOptimizer/Mandatory/MoveOnlyChecker.cpp` — extended to all non-trivial types in TinySwift
- `lib/SILOptimizer/SemanticARC/SemanticARCOpts.cpp` — restricted to safe peepholes in TinySwift
- `include/swift/Runtime/RuntimeFunctions.def` — ARC entries guarded with `TINYSWIFT_NO_ARC`
- `lib/IRGen/GenHeap.cpp` — retain/release guarded with `TINYSWIFT_NO_ARC`

## Build

```bash
cmake --preset tinyswift-debug \
  -DCMAKE_C_COMPILER=/path/to/clang \
  -DCMAKE_CXX_COMPILER=/path/to/clang++ \
  -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm
cmake --build --preset tinyswift-debug
```

## Tests

All tests are in `test/TinySwift/` with subdirectories: `smoke/`, `reject/`, `valid/`, `ported/`, `ownership/`.

Tests run with `-parse-stdlib` (no stdlib built), so only `Builtin.*` types are available. All test RUN lines must include:
```
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
```

Reject tests additionally need `-verify` and `// expected-error` annotations.

## Phase Overview

| Phase | Focus | Issues | Status |
|-------|-------|--------|--------|
| 0 | Fork & Foundation | #1-12 | Done |
| 1 | Language Subsetting (Sema rejections) | #13-18 | Done |
| 2 | ARC Removal & Ownership | #19-33 | Done |
| 3 | Metadata & Runtime Stripping | #34-43 | See `04_existentials_metadata_builtins_design.md` |
| 4 | Embedded Target Bringup | #44-53 | See `07_build_system_plan.md` |
| 5 | Polish & Release | #54-63 | See `09_implementation_plan.md` §3.6 |

## Conventions

- Gate all TinySwift-specific compiler changes on `Ctx.LangOpts.hasFeature(Feature::TinySwift)`
- Use `#ifndef TINYSWIFT` guards instead of deleting source directories
- Do NOT delete class-related constraint solver rules — reject `ClassDecl` early so they become dead code
- Reject constructs at the Sema level, not the parser level
- All new diagnostics use the `_in_tinyswift` suffix naming convention
