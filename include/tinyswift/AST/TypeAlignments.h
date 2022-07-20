//===--- TypeAlignments.h - Alignments of various Swift types ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
/// \file This file defines the alignment of various Swift AST classes.
///
/// It's useful to do this in a dedicated place to avoid recursive header
/// problems. To make sure we don't have any ODR violations, this header
/// should be included in every header that defines one of the forward-
/// declared types listed here.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPEALIGNMENTS_H
#define SWIFT_TYPEALIGNMENTS_H

#include "llvm/Support/AlignOf.h"

namespace tinyswift {
class AbstractStorageDecl;
class ArchetypeType;
class AssociatedTypeDecl;
class ASTContext;
class BraceStmt;
class Decl;
class DeclContext;
class Expr;
class ExtensionDecl;
class GenericEnvironment;
class GenericTypeParamDecl;
class NormalProtocolConformance;
class OperatorDecl;
class ProtocolDecl;
class ProtocolConformance;
class Stmt;
class Substitution;
class TypeVariableType;
class TypeBase;
class ValueDecl;

/// We frequently use three tag bits on all of these types.
constexpr size_t DeclAlignInBits = 3;
constexpr size_t DeclContextAlignInBits = 4;
constexpr size_t ExprAlignInBits = 3;
constexpr size_t StmtAlignInBits = 3;
constexpr size_t TypeAlignInBits = 3;
} // namespace tinyswift

namespace llvm {
/// Helper class for declaring the expected alignment of a pointer.
/// TODO: LLVM should provide this.
template <class T, size_t AlignInBits> struct MoreAlignedPointerTraits {
  enum { NumLowBitsAvailable = AlignInBits };
  static inline void *getAsVoidPointer(T *ptr) { return ptr; }
  static inline T *getFromVoidPointer(void *ptr) {
    return static_cast<T *>(ptr);
  }
};

template <class T> class PointerLikeTypeTraits;
} // namespace llvm

/// Declare the expected alignment of pointers to the given type.
/// This macro should be invoked from a top-level file context.
#define LLVM_DECLARE_TYPE_ALIGNMENT(CLASS, ALIGNMENT)                          \
  namespace llvm {                                                             \
  template <>                                                                  \
  class PointerLikeTypeTraits<CLASS *>                                         \
      : public MoreAlignedPointerTraits<CLASS, ALIGNMENT> {};                  \
  }

LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::Decl, tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::AbstractStorageDecl,
                            tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::AssociatedTypeDecl,
                            tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::GenericTypeParamDecl,
                            tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::OperatorDecl, tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::ProtocolDecl, tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::ValueDecl, tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::ExtensionDecl,
                            tinyswift::DeclAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::TypeBase, tinyswift::TypeAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::ArchetypeType,
                            tinyswift::TypeAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::TypeVariableType,
                            tinyswift::TypeAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::Stmt, tinyswift::StmtAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::BraceStmt, tinyswift::StmtAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::ASTContext, 2);
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::DeclContext,
                            tinyswift::DeclContextAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::Expr, tinyswift::ExprAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::ProtocolConformance,
                            tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::NormalProtocolConformance,
                            tinyswift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(tinyswift::GenericEnvironment,
                            tinyswift::DeclAlignInBits)

static_assert(alignof(void *) >= 2, "pointer alignment is too small");

#endif
