//===--- TinySwiftGenericSpecializationVerifier.cpp - Verify Full Monomorphization ===//
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
// Mandatory pass for TinySwift mode. Verifies that no unspecialized generic
// function calls remain after specialization. In TinySwift, all generic code
// must be fully monomorphized at compile time — no witness table dispatch,
// no runtime metadata.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "tinyswift-generic-specialization-verifier"

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

class TinySwiftGenericSpecializationVerifier : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // Only run in TinySwift mode.
    if (!fn->getModule().getOptions().TinySwift)
      return;

    // Skip generic functions themselves — they are templates that get
    // specialized. Only check non-generic functions for unspecialized calls.
    if (fn->getLoweredFunctionType()->getSubstGenericSignature())
      return;

    // Skip thunks and other compiler-generated functions that may
    // retain generic signatures.
    if (fn->isThunk() != IsNotThunk)
      return;

    for (auto &bb : *fn) {
      for (auto &inst : bb) {
        // Check all apply-site instructions (apply, try_apply, begin_apply).
        if (auto apply = FullApplySite::isa(&inst)) {
          if (apply.hasSubstitutions()) {
            // Check if any substitution contains an archetype (i.e., not
            // fully concrete). If so, this is an unspecialized generic call.
            auto subs = apply.getSubstitutionMap();
            bool hasArchetype = false;
            for (auto replacementType : subs.getReplacementTypes()) {
              if (replacementType->hasArchetype() ||
                  replacementType->hasTypeParameter()) {
                hasArchetype = true;
                break;
              }
            }
            if (hasArchetype) {
              auto calleeName = apply.getCalleeFunction()
                                    ? apply.getCalleeFunction()->getName()
                                    : "<unknown>";
              LLVM_DEBUG(llvm::dbgs()
                         << "TinySwift: unspecialized generic call in "
                         << fn->getName() << " to " << calleeName << "\n");
              fn->getModule().getASTContext().Diags.diagnose(
                  inst.getLoc().getSourceLoc(),
                  diag::tinyswift_unspecialized_generic, calleeName);
            }
          }
        }

        // Also check witness_method instructions — these should never
        // appear in TinySwift since we don't have witness tables.
        if (isa<WitnessMethodInst>(&inst)) {
          LLVM_DEBUG(llvm::dbgs()
                     << "TinySwift: witness_method instruction in "
                     << fn->getName() << "\n");
          llvm::report_fatal_error(
              "TinySwift: witness_method instruction found; all protocol "
              "dispatch must be statically resolved");
        }
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createTinySwiftGenericSpecializationVerifier() {
  return new TinySwiftGenericSpecializationVerifier();
}
