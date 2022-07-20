//
// Created by Satish on 20/07/22.
//

#ifndef TINYSWIFT_IDENTIFIER_H
#define TINYSWIFT_IDENTIFIER_H

#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/TrailingObjects.h"

namespace tinyswift {
class ASTContext;

class ParameterList;

/// DeclRefKind - The kind of reference to an identifier.
enum class DeclRefKind {
  /// An ordinary reference to an identifier, e.g. 'foo'.
  Ordinary,

  /// A reference to an identifier as a binary operator, e.g. '+' in 'a+b'.
  BinaryOperator,

  /// A reference to an identifier as a postfix unary operator, e.g. '++' in
  /// 'a++'.
  PostfixOperator,

  /// A reference to an identifier as a prefix unary operator, e.g. '--' in
  /// '--a'.
  PrefixOperator
};

/// Identifier - This is an instance of a uniqued identifier created by
/// ASTContext.  It just wraps a nul-terminated "const char*".
class Identifier {
  friend class ASTContext;

  const char *Pointer;

  /// Constructor, only accessible by ASTContext, which handles the uniquing.
  explicit Identifier(const char *Ptr) : Pointer(Ptr) {}

public:
  explicit Identifier() : Pointer(nullptr) {}

  const char *get() const { return Pointer; }

  llvm::StringRef str() const { return Pointer; }

  unsigned getLength() const {
    assert(Pointer != nullptr && "Tried getting length of empty identifier");
    return ::strlen(Pointer);
  }

  bool empty() const { return Pointer == nullptr; }

  /// isOperator - Return true if this identifier is an operator, false if it is
  /// a normal identifier.
  /// FIXME: We should maybe cache this.
  bool isOperator() const {
    if (empty())
      return false;
    if (isEditorPlaceholder())
      return false;
    if ((unsigned char)Pointer[0] < 0x80)
      return isOperatorStartCodePoint((unsigned char)Pointer[0]);

    // Handle the high unicode case out of line.
    return isOperatorSlow();
  }

  /// isOperatorStartCodePoint - Return true if the specified code point is a
  /// valid start of an operator.
  static bool isOperatorStartCodePoint(uint32_t C) {
    // ASCII operator chars.
    static const char OpChars[] = "/=-+*%<>!&|^~.?";
    if (C < 0x80)
      return memchr(OpChars, C, sizeof(OpChars) - 1) != 0;

    // Unicode math, symbol, arrow, dingbat, and line/box drawing chars.
    return (C >= 0x00A1 && C <= 0x00A7) || C == 0x00A9 || C == 0x00AB ||
           C == 0x00AC || C == 0x00AE || C == 0x00B0 || C == 0x00B1 ||
           C == 0x00B6 || C == 0x00BB || C == 0x00BF || C == 0x00D7 ||
           C == 0x00F7 || C == 0x2016 || C == 0x2017 ||
           (C >= 0x2020 && C <= 0x2027) || (C >= 0x2030 && C <= 0x203E) ||
           (C >= 0x2041 && C <= 0x2053) || (C >= 0x2055 && C <= 0x205E) ||
           (C >= 0x2190 && C <= 0x23FF) || (C >= 0x2500 && C <= 0x2775) ||
           (C >= 0x2794 && C <= 0x2BFF) || (C >= 0x2E00 && C <= 0x2E7F) ||
           (C >= 0x3001 && C <= 0x3003) || (C >= 0x3008 && C <= 0x3030);
  }

  /// isOperatorContinuationCodePoint - Return true if the specified code point
  /// is a valid operator code point.
  static bool isOperatorContinuationCodePoint(uint32_t C) {
    if (isOperatorStartCodePoint(C))
      return true;

    // Unicode combining characters and variation selectors.
    return (C >= 0x0300 && C <= 0x036F) || (C >= 0x1DC0 && C <= 0x1DFF) ||
           (C >= 0x20D0 && C <= 0x20FF) || (C >= 0xFE00 && C <= 0xFE0F) ||
           (C >= 0xFE20 && C <= 0xFE2F) || (C >= 0xE0100 && C <= 0xE01EF);
  }

  static bool isEditorPlaceholder(llvm::StringRef name) {
    return name.startswith("<#");
  }

  bool isEditorPlaceholder() const {
    return !empty() && isEditorPlaceholder(str());
  }

  const void *getAsOpaquePointer() const {
    return static_cast<const void *>(Pointer);
  }

  static Identifier getFromOpaquePointer(void *P) {
    return Identifier((const char *)P);
  }

  /// Compare two identifiers, producing -1 if \c *this comes before \c other,
  /// 1 if \c *this comes after \c other, and 0 if they are equal.
  ///
  /// Null identifiers come after all other identifiers.
  int compare(Identifier other) const;

  bool operator==(Identifier RHS) const { return Pointer == RHS.Pointer; }

  bool operator!=(Identifier RHS) const { return !(*this == RHS); }

  bool operator<(Identifier RHS) const { return Pointer < RHS.Pointer; }

  static Identifier getEmptyKey() {
    return Identifier(
        (const char *)llvm::DenseMapInfo<const void *>::getEmptyKey());
  }

  static Identifier getTombstoneKey() {
    return Identifier(
        (const char *)llvm::DenseMapInfo<const void *>::getTombstoneKey());
  }

private:
  bool isOperatorSlow() const;
};

class DeclName;

class ObjCSelector;

} // namespace tinyswift

#endif // TINYSWIFT_IDENTIFIER_H
