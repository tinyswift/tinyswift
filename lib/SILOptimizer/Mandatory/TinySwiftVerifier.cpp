//===--- TinySwiftVerifier.cpp - Verify No ARC Instructions in TinySwift --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Safety net pass for TinySwift mode. Walks all instructions in a function and
// diagnoses any ARC or class-related instructions that should never appear in
// TinySwift SIL. Runs early in the mandatory pipeline gated on
// SILOptions.TinySwift.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "tinyswift-verifier"

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

class TinySwiftVerifier : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // Only run in TinySwift mode.
    if (!fn->getModule().getOptions().TinySwift)
      return;

    for (auto &bb : *fn) {
      for (auto &inst : bb) {
        if (isForbiddenInstruction(&inst)) {
          // Emit an error: forbidden ARC/class instruction found.
          LLVM_DEBUG(llvm::dbgs()
                     << "TinySwiftVerifier: forbidden instruction in "
                     << fn->getName() << ": " << inst << "\n");
          // Use llvm_unreachable in debug builds to catch violations
          // immediately during development.
          llvm::report_fatal_error(
              "TinySwift: forbidden ARC or class instruction found in SIL: " +
              llvm::Twine(getSILInstructionName(inst.getKind())));
        }
      }
    }
  }

  /// Returns true if the instruction is forbidden in TinySwift mode.
  static bool isForbiddenInstruction(SILInstruction *inst) {
    switch (inst->getKind()) {
    // ARC instructions — should never be emitted in TinySwift.
    case SILInstructionKind::StrongRetainInst:
    case SILInstructionKind::StrongReleaseInst:
    case SILInstructionKind::RetainValueInst:
    case SILInstructionKind::ReleaseValueInst:
    case SILInstructionKind::RetainValueAddrInst:
    case SILInstructionKind::ReleaseValueAddrInst:
    case SILInstructionKind::UnmanagedRetainValueInst:
    case SILInstructionKind::UnmanagedReleaseValueInst:
    case SILInstructionKind::UnmanagedAutoreleaseValueInst:
    case SILInstructionKind::AutoreleaseValueInst:
    // Class allocation/deallocation — no classes in TinySwift.
    case SILInstructionKind::AllocRefInst:
    case SILInstructionKind::AllocRefDynamicInst:
    case SILInstructionKind::DeallocRefInst:
    case SILInstructionKind::DeallocPartialRefInst:
    // Class method dispatch — no classes in TinySwift.
    case SILInstructionKind::ClassMethodInst:
    case SILInstructionKind::SuperMethodInst:
    case SILInstructionKind::ObjCMethodInst:
    case SILInstructionKind::ObjCSuperMethodInst:
    // Existential instructions — no existential containers in TinySwift.
    case SILInstructionKind::InitExistentialAddrInst:
    case SILInstructionKind::OpenExistentialAddrInst:
    case SILInstructionKind::InitExistentialRefInst:
    case SILInstructionKind::OpenExistentialRefInst:
    case SILInstructionKind::InitExistentialMetatypeInst:
    case SILInstructionKind::OpenExistentialMetatypeInst:
    case SILInstructionKind::AllocExistentialBoxInst:
    case SILInstructionKind::OpenExistentialBoxInst:
    case SILInstructionKind::ProjectExistentialBoxInst:
    case SILInstructionKind::DeallocExistentialBoxInst:
      return true;
    default:
      return false;
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createTinySwiftVerifier() {
  return new TinySwiftVerifier();
}
