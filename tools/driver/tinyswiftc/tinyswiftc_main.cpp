//===--- tinyswiftc_main.cpp - TinySwift Compiler Driver ------------------===//
//
// This source file is part of the TinySwift open source project
//
// Copyright (c) 2024-2026 TinySwift contributors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//
//
// tinyswiftc is a standalone driver for compiling TinySwift programs targeting
// embedded platforms. It wraps swift-frontend with embedded-first defaults
// and handles the link step for bare-metal ELF and WebAssembly targets.
//
// This binary does NOT link against any Swift compiler libraries; it only
// depends on LLVMSupport for argument parsing and subprocess execution.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

#include <string>

using namespace llvm;

//===----------------------------------------------------------------------===//
// Command-line option definitions
//===----------------------------------------------------------------------===//

static cl::OptionCategory TinySwiftCategory("TinySwift Options");

static cl::list<std::string> InputFiles(cl::Positional,
                                        cl::desc("<input files>"),
                                        cl::cat(TinySwiftCategory));

// Target triple
static cl::opt<std::string>
    Target("target", cl::desc("Target triple (e.g. aarch64-none-elf)"),
           cl::cat(TinySwiftCategory));

// Sysroot
static cl::opt<std::string>
    Sysroot("sysroot", cl::desc("Sysroot path for cross-compilation"),
            cl::cat(TinySwiftCategory));

// Output modes
enum OutputMode {
  EmitExecutable,
  EmitObject,
  EmitIR,
  EmitSIL,
  EmitAssembly,
  EmitLibrary
};

static cl::opt<OutputMode> OutputModeFlag(
    cl::desc("Output mode:"),
    cl::values(
        clEnumValN(EmitExecutable, "emit-executable",
                   "Emit a linked executable (default)"),
        clEnumValN(EmitObject, "emit-object", "Emit object file(s)"),
        clEnumValN(EmitIR, "emit-ir", "Emit LLVM IR"),
        clEnumValN(EmitSIL, "emit-sil", "Emit Swift Intermediate Language"),
        clEnumValN(EmitAssembly, "emit-assembly", "Emit assembly"),
        clEnumValN(EmitLibrary, "emit-library", "Emit a shared library")),
    cl::init(EmitExecutable), cl::cat(TinySwiftCategory));

// Optimization level
enum OptLevel { ONone, O, OSize };

static cl::opt<OptLevel> OptimizationLevel(
    cl::desc("Optimization level:"),
    cl::values(clEnumValN(ONone, "Onone", "No optimization"),
               clEnumValN(O, "O", "Optimize for speed"),
               clEnumValN(OSize, "Osize", "Optimize for size")),
    cl::init(OSize), cl::cat(TinySwiftCategory));

// Output file
static cl::opt<std::string> OutputFile("o", cl::desc("Output file path"),
                                       cl::cat(TinySwiftCategory));

// Linking options
static cl::opt<std::string>
    LinkerScript("linker-script", cl::desc("Linker script path"),
                 cl::cat(TinySwiftCategory));

static cl::opt<std::string> Linker("linker",
                                   cl::desc("Linker executable path"),
                                   cl::cat(TinySwiftCategory));

static cl::list<std::string>
    XLinker("Xlinker", cl::desc("Pass argument to the linker"),
            cl::cat(TinySwiftCategory));

static cl::list<std::string>
    LibDirs("L", cl::desc("Add directory to library search path"),
            cl::Prefix, cl::cat(TinySwiftCategory));

static cl::list<std::string> Libs("l", cl::desc("Link library"), cl::Prefix,
                                  cl::cat(TinySwiftCategory));

static cl::opt<unsigned>
    StackSize("stack-size", cl::desc("Stack size in bytes"),
              cl::init(0), cl::cat(TinySwiftCategory));

static cl::opt<unsigned>
    HeapSize("heap-size", cl::desc("Heap size in bytes"),
             cl::init(0), cl::cat(TinySwiftCategory));

static cl::opt<std::string>
    EntryPoint("entry-point", cl::desc("Entry point symbol name"),
               cl::cat(TinySwiftCategory));

// Module options
static cl::opt<std::string>
    ModuleName("module-name", cl::desc("Module name"),
               cl::cat(TinySwiftCategory));

static cl::list<std::string>
    ImportPaths("I", cl::desc("Add directory to import search path"),
                cl::Prefix, cl::cat(TinySwiftCategory));

static cl::opt<bool> EmitModule("emit-module",
                                cl::desc("Emit Swift module file"),
                                cl::cat(TinySwiftCategory));

// Debug options
static cl::opt<bool> Verbose("v", cl::desc("Print commands before executing"),
                             cl::cat(TinySwiftCategory));

static cl::opt<bool>
    DryRun("###",
           cl::desc("Print commands without executing (dry-run)"),
           cl::cat(TinySwiftCategory));

static cl::opt<bool>
    PrintTargetInfo("print-target-info",
                    cl::desc("Print target information and exit"),
                    cl::cat(TinySwiftCategory));

static cl::opt<bool>
    PrintSysroot("print-sysroot",
                 cl::desc("Print resolved sysroot path and exit"),
                 cl::cat(TinySwiftCategory));

//===----------------------------------------------------------------------===//
// Helper functions
//===----------------------------------------------------------------------===//

/// Locate swift-frontend as a sibling of this executable.
static std::string findSwiftFrontend(const char *Argv0) {
  void *MainAddr = (void *)(intptr_t)findSwiftFrontend;
  std::string ExePath =
      sys::fs::getMainExecutable(Argv0, MainAddr);
  SmallString<256> Dir(ExePath);
  sys::path::remove_filename(Dir);
  sys::path::append(Dir, "swift-frontend");
  if (sys::fs::exists(Dir))
    return std::string(Dir);

  // Fallback: search PATH
  if (auto P = sys::findProgramByName("swift-frontend"))
    return *P;

  errs() << "tinyswiftc: error: cannot find swift-frontend\n";
  return "";
}

/// Locate lld (ld.lld or wasm-ld) as a sibling or in PATH.
static std::string findLinker(StringRef TripleStr) {
  // Determine which linker to look for
  std::string LinkerName;
  if (TripleStr.contains("wasm"))
    LinkerName = "wasm-ld";
  else
    LinkerName = "ld.lld";

  // If user specified --linker, use that
  if (!Linker.empty())
    return Linker;

  // Search PATH
  if (auto P = sys::findProgramByName(LinkerName))
    return *P;

  errs() << "tinyswiftc: error: cannot find " << LinkerName << "\n";
  return "";
}

/// Resolve sysroot using 4-tier strategy:
/// 1. --sysroot flag
/// 2. TINYSWIFT_SYSROOT_<TRIPLE> env var
/// 3. <toolchain>/sysroots/<triple>/
/// 4. Platform defaults (empty)
static std::string resolveSysroot(const char *Argv0, StringRef Triple) {
  // Tier 1: explicit flag
  if (!Sysroot.empty())
    return Sysroot;

  // Tier 2: environment variable
  std::string EnvName = "TINYSWIFT_SYSROOT_";
  for (char C : Triple) {
    if (C == '-' || C == '.')
      EnvName += '_';
    else
      EnvName += toupper(C);
  }
  if (auto Val = sys::Process::GetEnv(EnvName))
    return *Val;

  // Tier 3: <toolchain>/sysroots/<triple>/
  void *MainAddr = (void *)(intptr_t)findSwiftFrontend;
  std::string ExePath = sys::fs::getMainExecutable(Argv0, MainAddr);
  SmallString<256> SysrootDir(ExePath);
  sys::path::remove_filename(SysrootDir); // bin/
  sys::path::remove_filename(SysrootDir); // toolchain root
  sys::path::append(SysrootDir, "sysroots", Triple);
  if (sys::fs::is_directory(SysrootDir))
    return std::string(SysrootDir);

  // Tier 4: no sysroot
  return "";
}

/// Get the embedded test support directory for a given target.
static std::string getEmbeddedSupportDir(const char *Argv0, StringRef Triple) {
  void *MainAddr = (void *)(intptr_t)findSwiftFrontend;
  std::string ExePath = sys::fs::getMainExecutable(Argv0, MainAddr);
  SmallString<256> Dir(ExePath);
  sys::path::remove_filename(Dir);
  sys::path::remove_filename(Dir);
  sys::path::append(Dir, "utils", "embedded-test-support");

  // Map triple to directory name
  std::string DeviceDir;
  if (Triple.starts_with("aarch64"))
    DeviceDir = "aarch64-qemu-virt";
  else if (Triple.starts_with("riscv32"))
    DeviceDir = "riscv32-qemu-virt";
  else if (Triple.starts_with("armv7em"))
    DeviceDir = "arm-qemu-stm32f4";
  // wasm has no support directory with linkerscript

  if (!DeviceDir.empty()) {
    sys::path::append(Dir, DeviceDir);
    if (sys::fs::is_directory(Dir))
      return std::string(Dir);
  }
  return "";
}

/// Execute a command, printing it if verbose.
static int executeCommand(const std::vector<std::string> &Args, bool Print,
                          bool JustPrint) {
  if (Print || JustPrint) {
    for (size_t I = 0; I < Args.size(); ++I) {
      if (I > 0)
        errs() << " ";
      // Quote args with spaces
      if (Args[I].find(' ') != std::string::npos)
        errs() << "\"" << Args[I] << "\"";
      else
        errs() << Args[I];
    }
    errs() << "\n";
  }

  if (JustPrint)
    return 0;

  SmallVector<StringRef, 32> ArgsRef;
  for (const auto &A : Args)
    ArgsRef.push_back(A);

  std::string ErrMsg;
  int Result = sys::ExecuteAndWait(ArgsRef[0], ArgsRef, /*Env=*/std::nullopt,
                                   /*Redirects=*/{}, /*SecondsToWait=*/0,
                                   /*MemoryLimit=*/0, &ErrMsg);
  if (!ErrMsg.empty())
    errs() << "tinyswiftc: error: " << ErrMsg << "\n";

  return Result;
}

/// Get a default output filename based on input and output mode.
static std::string getDefaultOutputFile(StringRef InputFile,
                                        OutputMode Mode, bool IsWasm) {
  SmallString<256> Result(InputFile);
  switch (Mode) {
  case EmitExecutable:
    sys::path::replace_extension(Result, IsWasm ? "wasm" : "elf");
    break;
  case EmitObject:
    sys::path::replace_extension(Result, "o");
    break;
  case EmitIR:
    sys::path::replace_extension(Result, "ll");
    break;
  case EmitSIL:
    sys::path::replace_extension(Result, "sil");
    break;
  case EmitAssembly:
    sys::path::replace_extension(Result, "s");
    break;
  case EmitLibrary:
    sys::path::replace_extension(Result, "a");
    break;
  }
  return std::string(Result);
}

//===----------------------------------------------------------------------===//
// Main entry point
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  cl::HideUnrelatedOptions(TinySwiftCategory);
  cl::ParseCommandLineOptions(argc, argv,
                              "TinySwift Compiler Driver\n\n"
                              "  Compiles Swift programs for embedded targets\n"
                              "  with value-type-only semantics and zero runtime\n"
                              "  dependencies.\n");

  // Find swift-frontend
  std::string Frontend = findSwiftFrontend(argv[0]);
  if (Frontend.empty())
    return 1;

  // Handle info queries
  if (PrintTargetInfo) {
    outs() << "Target: " << Target << "\n";
    outs() << "Sysroot: " << resolveSysroot(argv[0], Target) << "\n";
    return 0;
  }

  if (PrintSysroot) {
    outs() << resolveSysroot(argv[0], Target) << "\n";
    return 0;
  }

  // Validate inputs
  if (InputFiles.empty()) {
    errs() << "tinyswiftc: error: no input files\n";
    return 1;
  }

  // Determine target
  std::string TripleStr = Target;
  if (TripleStr.empty()) {
    // Default to host target for non-embedded use
    errs() << "tinyswiftc: error: --target is required\n";
    return 1;
  }

  bool IsWasm = StringRef(TripleStr).contains("wasm");

  // Resolve sysroot
  std::string ResolvedSysroot = resolveSysroot(argv[0], TripleStr);

  // Determine output file
  std::string OutFile = OutputFile;
  if (OutFile.empty() && !InputFiles.empty())
    OutFile = getDefaultOutputFile(InputFiles[0], OutputModeFlag, IsWasm);

  //=== Build swift-frontend command ===//

  std::vector<std::string> FrontendArgs;
  FrontendArgs.push_back(Frontend);

  // Hardcoded TinySwift defaults
  FrontendArgs.push_back("-enable-experimental-feature");
  FrontendArgs.push_back("Embedded");
  FrontendArgs.push_back("-enable-experimental-feature");
  FrontendArgs.push_back("TinySwift");
  FrontendArgs.push_back("-disable-objc-interop");
  FrontendArgs.push_back("-parse-as-library");
  FrontendArgs.push_back("-parse-stdlib");
  FrontendArgs.push_back("-disable-reflection-metadata");

  // Target
  FrontendArgs.push_back("-target");
  FrontendArgs.push_back(TripleStr);

  // Optimization level
  switch (OptimizationLevel) {
  case ONone:
    FrontendArgs.push_back("-Onone");
    break;
  case O:
    FrontendArgs.push_back("-O");
    break;
  case OSize:
    FrontendArgs.push_back("-Osize");
    break;
  }

  // Module name
  if (!ModuleName.empty()) {
    FrontendArgs.push_back("-module-name");
    FrontendArgs.push_back(ModuleName);
  } else if (!InputFiles.empty()) {
    // Derive module name from first input file
    SmallString<64> DefaultModule(sys::path::stem(InputFiles[0]));
    FrontendArgs.push_back("-module-name");
    FrontendArgs.push_back(std::string(DefaultModule));
  }

  // Import paths
  for (const auto &P : ImportPaths) {
    FrontendArgs.push_back("-I");
    FrontendArgs.push_back(P);
  }

  // Sysroot
  if (!ResolvedSysroot.empty()) {
    FrontendArgs.push_back("-sdk");
    FrontendArgs.push_back(ResolvedSysroot);
  }

  // Determine frontend output mode
  bool NeedLink = false;
  std::string ObjectFile;
  switch (OutputModeFlag) {
  case EmitExecutable:
  case EmitLibrary:
    // Emit object, then link separately
    NeedLink = true;
    {
      SmallString<256> TmpObj;
      sys::fs::createTemporaryFile("tinyswiftc", "o", TmpObj);
      ObjectFile = std::string(TmpObj);
    }
    FrontendArgs.push_back("-emit-object");
    FrontendArgs.push_back("-o");
    FrontendArgs.push_back(ObjectFile);
    break;
  case EmitObject:
    FrontendArgs.push_back("-emit-object");
    FrontendArgs.push_back("-o");
    FrontendArgs.push_back(OutFile);
    break;
  case EmitIR:
    FrontendArgs.push_back("-emit-ir");
    FrontendArgs.push_back("-o");
    FrontendArgs.push_back(OutFile);
    break;
  case EmitSIL:
    FrontendArgs.push_back("-emit-sil");
    FrontendArgs.push_back("-o");
    FrontendArgs.push_back(OutFile);
    break;
  case EmitAssembly:
    FrontendArgs.push_back("-emit-assembly");
    FrontendArgs.push_back("-o");
    FrontendArgs.push_back(OutFile);
    break;
  }

  // Emit module if requested
  if (EmitModule)
    FrontendArgs.push_back("-emit-module");

  // Input files
  for (const auto &F : InputFiles)
    FrontendArgs.push_back(F);

  //=== Execute swift-frontend ===//

  int FrontendResult = executeCommand(FrontendArgs, Verbose, DryRun);
  if (FrontendResult != 0)
    return FrontendResult;

  //=== Link step (for -emit-executable / -emit-library) ===//

  if (NeedLink) {
    std::string LinkerPath = findLinker(TripleStr);
    if (LinkerPath.empty())
      return 1;

    std::vector<std::string> LinkArgs;
    LinkArgs.push_back(LinkerPath);

    if (IsWasm) {
      // WebAssembly linking via wasm-ld
      LinkArgs.push_back("--no-entry");
      LinkArgs.push_back("--export-all");
      LinkArgs.push_back("--allow-undefined");

      unsigned InitMem = HeapSize > 0 ? HeapSize : 65536;
      LinkArgs.push_back("--initial-memory=" + std::to_string(InitMem));
      LinkArgs.push_back("--stack-first");

      // If entry point specified
      if (!EntryPoint.empty()) {
        LinkArgs.push_back("--entry");
        LinkArgs.push_back(EntryPoint);
      }
    } else {
      // ELF linking via ld.lld

      // Linker script
      std::string LDScript = LinkerScript;
      if (LDScript.empty()) {
        // Try to find default linker script from embedded support dir
        std::string SupportDir = getEmbeddedSupportDir(argv[0], TripleStr);
        if (!SupportDir.empty()) {
          SmallString<256> DefaultScript(SupportDir);
          sys::path::append(DefaultScript, "linkerscript.ld");
          if (sys::fs::exists(DefaultScript))
            LDScript = std::string(DefaultScript);
        }
      }

      if (!LDScript.empty()) {
        LinkArgs.push_back("-T");
        LinkArgs.push_back(LDScript);
      }

      // Entry point
      if (!EntryPoint.empty()) {
        LinkArgs.push_back("-e");
        LinkArgs.push_back(EntryPoint);
      } else {
        LinkArgs.push_back("-e");
        LinkArgs.push_back("start");
      }

      // Garbage collect unused sections
      LinkArgs.push_back("--gc-sections");

      // Stack size
      if (StackSize > 0) {
        LinkArgs.push_back("-z");
        LinkArgs.push_back("stack-size=" + std::to_string(StackSize));
      }
    }

    // Library search paths
    for (const auto &D : LibDirs) {
      LinkArgs.push_back("-L");
      LinkArgs.push_back(D);
    }

    // Libraries
    for (const auto &L : Libs) {
      LinkArgs.push_back("-l");
      LinkArgs.push_back(L);
    }

    // Extra linker flags
    for (const auto &X : XLinker)
      LinkArgs.push_back(X);

    // Object file
    LinkArgs.push_back(ObjectFile);

    // Output
    LinkArgs.push_back("-o");
    LinkArgs.push_back(OutFile);

    int LinkResult = executeCommand(LinkArgs, Verbose, DryRun);

    // Clean up temp object file
    if (!DryRun)
      sys::fs::remove(ObjectFile);

    if (LinkResult != 0)
      return LinkResult;
  }

  if (!DryRun && Verbose)
    errs() << "tinyswiftc: output: " << OutFile << "\n";

  return 0;
}
