//===--- NoMetadataVerifier.h - Verify No Swift Metadata in IR --*- C++ -*-===//
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

#ifndef SWIFT_IRGEN_NOMETADATAVERIFIER_H
#define SWIFT_IRGEN_NOMETADATAVERIFIER_H

namespace llvm {
class Module;
} // end namespace llvm

namespace swift {
namespace irgen {

/// Verifies that no Swift runtime metadata sections exist in the given
/// LLVM module. Used as a post-IRGen safety net in TinySwift mode.
class NoMetadataVerifier {
public:
  /// Returns true if the module is clean (no forbidden metadata symbols).
  static bool verify(const llvm::Module &M);

  /// Verifies and traps with a fatal error if metadata is found.
  static void verifyAndTrap(const llvm::Module &M);
};

} // end namespace irgen
} // end namespace swift

#endif // SWIFT_IRGEN_NOMETADATAVERIFIER_H
