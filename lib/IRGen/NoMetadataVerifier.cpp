//===--- NoMetadataVerifier.cpp - Verify No Swift Metadata in IR ----------===//
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
// Post-IRGen LLVM module pass that scans for Swift metadata sections. In
// TinySwift mode, no __swift5_* sections should be emitted. This pass
// asserts that none exist.
//
//===----------------------------------------------------------------------===//

#include "swift/IRGen/NoMetadataVerifier.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "tinyswift-no-metadata-verifier"

using namespace swift;
using namespace irgen;

/// Forbidden metadata section name prefixes/substrings.
static const char *ForbiddenSections[] = {
    "__swift5_types",
    "__swift5_proto",
    "__swift5_fieldmd",
    "__swift5_assocty",
    "__swift5_builtin",
    "__swift5_capture",
    "__swift5_typeref",
    "__swift5_reflstr",
    "__swift5_mpenum",
    "swift5_type_metadata",
    "swift5_protocols",
    "swift5_protocol_conformances",
};

bool NoMetadataVerifier::verify(const llvm::Module &M) {
  bool clean = true;

  for (const auto &GV : M.globals()) {
    StringRef name = GV.getName();
    StringRef section = GV.getSection();

    for (const char *forbidden : ForbiddenSections) {
      if (name.contains(forbidden) || section.contains(forbidden)) {
        LLVM_DEBUG(llvm::dbgs()
                   << "NoMetadataVerifier: found forbidden metadata symbol: "
                   << name << " (section: " << section << ")\n");
        llvm::errs() << "TinySwift: forbidden metadata symbol found: " << name
                     << "\n";
        clean = false;
      }
    }
  }

  return clean;
}

void NoMetadataVerifier::verifyAndTrap(const llvm::Module &M) {
  if (!verify(M)) {
    llvm::report_fatal_error(
        "TinySwift: metadata sections found in LLVM IR; all metadata "
        "should have been stripped by TinySwift guards");
  }
}
