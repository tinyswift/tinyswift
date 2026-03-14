# TinySwift

A value-type-only Swift compiler fork for embedded systems, based on `swiftlang/swift` at `swift-6.1-RELEASE`.

## Project Tracking

- **GitHub Project Board**: https://github.com/orgs/tinyswift/projects/1
- **Issues**: 63 total across 6 phases — use `gh issue list --label phase-N` to filter by phase
- Phase 0 (#1-12), Phase 1 (#13-18), Phase 2 (#19-33), Phase 3 (#34-43), Phase 4 (#44-53), and Phase 5 (#54-63) are complete

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
- **Sema rejections** (25 diagnostics): Classes, actors, existentials, async/await, indirect enums, bare throws, macros, dynamic casts, `open` access, ObjC attrs, `try?`/`try!`, `is` operator, `super`, `#selector`, `.Type`/`.Protocol` metatypes, IUO, `rethrows`, `@convention(block)`, `@Sendable`, `@dynamicCallable`, `@dynamicMemberLookup`, `@globalActor`, weak/unowned, module imports — all rejected in `TypeCheckDeclPrimary.cpp`, `TypeCheckAttr.cpp`, `TypeCheckType.cpp`, `MiscDiagnostics.cpp`
- **Diagnostics**: 25 TinySwift-specific diagnostics in `DiagnosticsSema.def` plus SIL diagnostics in `DiagnosticsSIL.def` (search `tinyswift`)
- **Compilation condition**: `#if tinyswift` available in source code for conditional compilation
- **Build guard**: `TINYSWIFT_BUILD=ON` CMake option; `#ifndef TINYSWIFT` guards in source instead of directory deletion
- **Compile flags**: `TINYSWIFT_NO_CLASSES`, `TINYSWIFT_NO_OBJC`, `TINYSWIFT_NO_ARC`, `TINYSWIFT_NO_RUNTIME_METADATA`
- **Phase 2 (ARC Removal)**: OSSA form preserved through IRGen, ARC passes guarded, MoveOnlyChecker extended to all non-trivial types (including types with deinits), IRGen handles OSSA instructions directly, `TinySwiftOwnershipLowering` pass for drop flags, destroy ordering, and deinit resolution
- **Phase 3 (Metadata & Runtime Stripping)**: All metadata emission guarded (GenMeta, GenReflection, GenProto, GenExistential, GenDecl, GenValueWitness), Builtins.swift stdlib, Runtime.c, mandatory generic specialization, zero `__swift5_*` sections
- **Phase 4 (Embedded Target Bringup)**: tinyswiftc driver (`--version`, `-emit-bc`), ARM64/RISC-V(32+64)/Wasm/x86_64 targets, CI with QEMU and wasmtime, binary size budgets
- **Phase 5 (Polish & Release)**: Documentation (GettingStarted, LanguageGuide, EmbeddedCDevelopers), stress tests, cross-target test matrix, example projects, release packaging pipeline, v0.1.0-alpha.1

## Key Files

- `include/swift/Basic/Features.def` — TinySwift feature registration
- `include/swift/AST/DiagnosticsSema.def` — diagnostic messages
- `include/swift/AST/SILOptions.h` — `TinySwift` SIL flag
- `lib/Frontend/CompilerInvocation.cpp` — TinySwift→Embedded cascading, `#if tinyswift` condition
- `lib/Sema/TypeCheckDeclPrimary.cpp` — class/actor/enum/func/macro rejections
- `lib/Sema/TypeCheckAttr.cpp` — @objc, @IBOutlet, open access, rethrows, @Sendable, @dynamicCallable, @dynamicMemberLookup, @globalActor rejections
- `lib/Sema/TypeCheckType.cpp` — existential type, metatype, IUO, @convention(block) rejection
- `lib/Sema/MiscDiagnostics.cpp` — dynamic cast, try?/try!, is, super, #selector rejection
- `lib/AST/FeatureSet.cpp` — `UNINTERESTING_FEATURE(TinySwift)`
- `CMakePresets.json` — 8 presets (debug, release, arm64, riscv32, riscv64, wasm, x86_64)
- `CMakeLists.txt` — `TINYSWIFT_BUILD` option and compile flags
- `lib/SILOptimizer/Mandatory/TinySwiftOwnershipLowering.cpp` — ownership lowering: drop flags, destroy ordering, deinit resolution
- `lib/SILOptimizer/Mandatory/TinySwiftVerifier.cpp` — safety net: asserts zero ARC/class/existential instructions
- `lib/SILOptimizer/PassManager/PassPipeline.cpp` — ARC passes and OME guarded on `TinySwift`
- `lib/SILOptimizer/Mandatory/OwnershipModelEliminator.cpp` — bypassed in TinySwift (preserves OSSA)
- `lib/IRGen/IRGenSIL.cpp` — OSSA-aware visitors for TinySwift (begin_borrow, end_borrow, copy_value, etc.)
- `lib/SILOptimizer/Mandatory/MoveOnlyChecker.cpp` — extended to all non-trivial types in TinySwift
- `lib/SILOptimizer/SemanticARC/SemanticARCOpts.cpp` — restricted to safe peepholes in TinySwift
- `include/swift/Runtime/RuntimeFunctions.def` — ARC entries guarded with `TINYSWIFT_NO_ARC`
- `lib/IRGen/GenHeap.cpp` — retain/release guarded with `TINYSWIFT_NO_ARC`
- `lib/IRGen/GenMeta.cpp` — struct/enum/foreign type metadata emission guarded for TinySwift
- `lib/IRGen/GenReflection.cpp` — field descriptors, builtin metadata, reflection version guarded
- `lib/IRGen/GenProto.cpp` — witness table ref/accessor guarded with llvm_unreachable
- `lib/IRGen/GenExistential.cpp` — existential container init paths guarded with llvm_unreachable
- `lib/IRGen/GenDecl.cpp` — protocol conformances, type metadata records, lazy metadata guarded
- `lib/IRGen/GenValueWitness.cpp` — value witness table emission returns null in TinySwift
- `lib/SILOptimizer/Mandatory/TinySwiftGenericSpecializationVerifier.cpp` — verifies full monomorphization
- `stdlib/public/TinySwift/Builtins.swift` — TinySwift standard library (all primitive types, protocols, pointers)
- `stdlib/public/TinySwift/Runtime.c` — minimal C runtime (trap, memcpy, memset, alloc stubs, stack canary, optional bump allocator via `TINYSWIFT_ENABLE_BUMP_ALLOC`, debug poison bytes)
- `stdlib/public/TinySwift/CMakeLists.txt` — builds builtins module and runtime
- `lib/IRGen/NoMetadataVerifier.cpp` — post-IRGen verifier: asserts zero `__swift5_*` metadata sections
- `lib/IRGen/GenClass.cpp` — `emitClassDecl` guarded with llvm_unreachable
- `lib/IRGen/MetadataRequest.cpp` — `createDirectTypeMetadataAccessFunction` guarded with llvm_unreachable
- `lib/IRGen/GenArchetype.cpp` — `emitArchetypeTypeMetadataRef` guarded with llvm_unreachable
- `lib/Serialization/Serialization.cpp` — TinySwift module flag in serialized module metadata
- `scripts/run-cross-target-tests.sh` — cross-target test runner (ARM64, RISC-V 32/64, Wasm)
- `scripts/upstream-sync-check.sh` — upstream divergence monitoring
- `scripts/package-toolchain.sh` — release tarball packaging
- `.github/workflows/release.yml` — release pipeline triggered by v* tags

## Build

```bash
cmake --preset tinyswift-debug \
  -DCMAKE_C_COMPILER=/path/to/clang \
  -DCMAKE_CXX_COMPILER=/path/to/clang++ \
  -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm
cmake --build --preset tinyswift-debug
```

## Tests

103 test files in `test/TinySwift/` with subdirectories: `smoke/` (3), `reject/` (24), `valid/` (7), `ported/` (34), `ownership/` (12), `metadata/` (11), `embedded/` (8), `stress/` (4).

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
| 3 | Metadata & Runtime Stripping | #34-43 | Done |
| 4 | Embedded Target Bringup | #44-53 | Done |
| 5 | Polish & Release | #54-63 | Done |

## Conventions

- Gate all TinySwift-specific compiler changes on `Ctx.LangOpts.hasFeature(Feature::TinySwift)`
- Use `#ifndef TINYSWIFT` guards instead of deleting source directories
- Do NOT delete class-related constraint solver rules — reject `ClassDecl` early so they become dead code
- Reject constructs at the Sema level, not the parser level
- All new diagnostics use the `_in_tinyswift` suffix naming convention
