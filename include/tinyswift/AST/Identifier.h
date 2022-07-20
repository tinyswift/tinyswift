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

} // namespace tinyswift

namespace llvm {
raw_ostream &operator<<(raw_ostream &OS, tinyswift::Identifier I);
raw_ostream &operator<<(raw_ostream &OS, tinyswift::DeclName I);

// Identifiers hash just like pointers.
template <> struct DenseMapInfo<tinyswift::Identifier> {
  static tinyswift::Identifier getEmptyKey() {
    return tinyswift::Identifier::getEmptyKey();
  }
  static tinyswift::Identifier getTombstoneKey() {
    return tinyswift::Identifier::getTombstoneKey();
  }
  static unsigned getHashValue(tinyswift::Identifier Val) {
    return DenseMapInfo<const void *>::getHashValue(Val.get());
  }
  static bool isEqual(tinyswift::Identifier LHS, tinyswift::Identifier RHS) {
    return LHS == RHS;
  }
};

// An Identifier is "pointer like".
template <typename T> class PointerLikeTypeTraits;
template <> class PointerLikeTypeTraits<tinyswift::Identifier> {
public:
  static inline void *getAsVoidPointer(tinyswift::Identifier I) {
    return (void *)I.get();
  }
  static inline tinyswift::Identifier getFromVoidPointer(void *P) {
    return tinyswift::Identifier::getFromOpaquePointer(P);
  }
  enum { NumLowBitsAvailable = 2 };
};

} // end namespace llvm

namespace tinyswift {

/// A declaration name, which may comprise one or more identifier pieces.
class DeclName {
  friend class ASTContext;

  /// Represents a compound declaration name.
  struct alignas(Identifier) CompoundDeclName final
      : llvm::FoldingSetNode,
        private llvm::TrailingObjects<CompoundDeclName, Identifier> {
    friend TrailingObjects;

    friend class DeclName;

    Identifier BaseName;
    size_t NumArgs;

    explicit CompoundDeclName(Identifier BaseName, size_t NumArgs)
        : BaseName(BaseName), NumArgs(NumArgs) {
      assert(NumArgs > 0 && "Should use IdentifierAndCompound");
    }

    llvm::ArrayRef<Identifier> getArgumentNames() const {
      return {getTrailingObjects<Identifier>(), NumArgs};
    }

    llvm::MutableArrayRef<Identifier> getArgumentNames() {
      return {getTrailingObjects<Identifier>(), NumArgs};
    }

    /// Uniquing for the ASTContext.
    static void Profile(llvm::FoldingSetNodeID &id, Identifier baseName,
                        llvm::ArrayRef<Identifier> argumentNames);

    void Profile(llvm::FoldingSetNodeID &id) {
      Profile(id, BaseName, getArgumentNames());
    }
  };

  // A single stored identifier, along with a bit stating whether it is the
  // base name for a zero-argument compound name.
  typedef llvm::PointerIntPair<Identifier, 1, bool> IdentifierAndCompound;

  // Either a single identifier piece stored inline (with a bit to say whether
  // it is simple or compound), or a reference to a compound declaration name.
  llvm::PointerUnion<IdentifierAndCompound, CompoundDeclName *>
      SimpleOrCompound;

  DeclName(void *Opaque)
      : SimpleOrCompound(
            decltype(SimpleOrCompound)::getFromOpaqueValue(Opaque)) {}

  void initialize(ASTContext &C, Identifier baseName,
                  llvm::ArrayRef<Identifier> argumentNames);

public:
  /// Build a null name.
  DeclName() : SimpleOrCompound(IdentifierAndCompound()) {}

  /// Build a simple value name with one component.
  /*implicit*/ DeclName(Identifier simpleName)
      : SimpleOrCompound(IdentifierAndCompound(simpleName, false)) {}

  /// Build a compound value name given a base name and a set of argument names.
  DeclName(ASTContext &C, Identifier baseName,
           llvm::ArrayRef<Identifier> argumentNames) {
    initialize(C, baseName, argumentNames);
  }

  /// Build a compound value name given a base name and a set of argument names
  /// extracted from a parameter list.
  DeclName(ASTContext &C, Identifier baseName, ParameterList *paramList);

  /// Retrieve the 'base' name, i.e., the name that follows the introducer,
  /// such as the 'foo' in 'func foo(x:Int, y:Int)' or the 'bar' in
  /// 'var bar: Int'.
  Identifier getBaseName() const {
    if (auto compound = SimpleOrCompound.dyn_cast<CompoundDeclName *>())
      return compound->BaseName;

    return SimpleOrCompound.get<IdentifierAndCompound>().getPointer();
  }

  /// Retrieve the names of the arguments, if there are any.
  llvm::ArrayRef<Identifier> getArgumentNames() const {
    if (auto compound = SimpleOrCompound.dyn_cast<CompoundDeclName *>())
      return compound->getArgumentNames();

    return {};
  }

  explicit operator bool() const {
    if (SimpleOrCompound.dyn_cast<CompoundDeclName *>())
      return true;
    return !SimpleOrCompound.get<IdentifierAndCompound>().getPointer().empty();
  }

  /// True if this is a simple one-component name.
  bool isSimpleName() const {
    if (SimpleOrCompound.dyn_cast<CompoundDeclName *>())
      return false;

    return !SimpleOrCompound.get<IdentifierAndCompound>().getInt();
  }

  /// True if this is a compound name.
  bool isCompoundName() const {
    if (SimpleOrCompound.dyn_cast<CompoundDeclName *>())
      return true;

    return SimpleOrCompound.get<IdentifierAndCompound>().getInt();
  }

  /// True if this name is a simple one-component name identical to the
  /// given identifier.
  bool isSimpleName(Identifier name) const {
    return isSimpleName() && getBaseName() == name;
  }

  /// True if this name is a simple one-component name equal to the
  /// given string.
  bool isSimpleName(llvm::StringRef name) const {
    return isSimpleName() && getBaseName().str().equals(name);
  }

  /// True if this name is an operator.
  bool isOperator() const { return getBaseName().isOperator(); }

  /// True if this name should be found by a decl ref or member ref under the
  /// name specified by 'refName'.
  ///
  /// We currently match compound names either when their first component
  /// matches a simple name lookup or when the full compound name matches.
  bool matchesRef(DeclName refName) const {
    // Identical names always match.
    if (SimpleOrCompound == refName.SimpleOrCompound)
      return true;
    // If the reference is a simple name, try simple name matching.
    if (refName.isSimpleName())
      return refName.getBaseName() == getBaseName();
    // The names don't match.
    return false;
  }

  /// Add a DeclName to a lookup table so that it can be found by its simple
  /// name or its compound name.
  template <typename LookupTable, typename Element>
  void addToLookupTable(LookupTable &table, const Element &elt) {
    table[*this].push_back(elt);
    if (!isSimpleName()) {
      table[getBaseName()].push_back(elt);
    }
  }

  /// Compare two declaration names, producing -1 if \c *this comes before
  /// \c other,  1 if \c *this comes after \c other, and 0 if they are equal.
  ///
  /// Null declaration names come after all other declaration names.
  int compare(DeclName other) const;

  friend bool operator==(DeclName lhs, DeclName rhs) {
    return lhs.getOpaqueValue() == rhs.getOpaqueValue();
  }

  friend bool operator!=(DeclName lhs, DeclName rhs) { return !(lhs == rhs); }

  friend bool operator<(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) < 0;
  }

  friend bool operator<=(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) <= 0;
  }

  friend bool operator>(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) > 0;
  }

  friend bool operator>=(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) >= 0;
  }

  void *getOpaqueValue() const { return SimpleOrCompound.getOpaqueValue(); }

  static DeclName getFromOpaqueValue(void *p) { return DeclName(p); }

  /// Get a string representation of the name,
  ///
  /// \param scratch Scratch space to use.
  llvm::StringRef getString(llvm::SmallVectorImpl<char> &scratch,
                            bool skipEmptyArgumentNames = false) const;

  /// Print the representation of this declaration name to the given
  /// stream.
  ///
  /// \param skipEmptyArgumentNames When true, don't print the argument labels
  /// if they are all empty.
  llvm::raw_ostream &print(llvm::raw_ostream &os,
                           bool skipEmptyArgumentNames = false) const;

  /// Print a "pretty" representation of this declaration name to the given
  /// stream.
  ///
  /// This is the name used for diagnostics; it is not necessarily the
  /// fully-specified name that would be written in the source.
  llvm::raw_ostream &printPretty(llvm::raw_ostream &os) const;

  /// Dump this name to standard error.
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const,
                            "only for use within the debugger");
};
} // namespace tinyswift

namespace llvm {
// A DeclName is "pointer like".
template <typename T> class PointerLikeTypeTraits;
template <> class PointerLikeTypeTraits<tinyswift::DeclName> {
public:
  static inline void *getAsVoidPointer(tinyswift::DeclName name) {
    return name.getOpaqueValue();
  }
  static inline tinyswift::DeclName getFromVoidPointer(void *ptr) {
    return tinyswift::DeclName::getFromOpaqueValue(ptr);
  }
  enum { NumLowBitsAvailable = 0 };
};

// DeclNames hash just like pointers.
template <> struct DenseMapInfo<tinyswift::DeclName> {
  static tinyswift::DeclName getEmptyKey() {
    return tinyswift::Identifier::getEmptyKey();
  }
  static tinyswift::DeclName getTombstoneKey() {
    return tinyswift::Identifier::getTombstoneKey();
  }
  static unsigned getHashValue(tinyswift::DeclName Val) {
    return DenseMapInfo<void *>::getHashValue(Val.getOpaqueValue());
  }
  static bool isEqual(tinyswift::DeclName LHS, tinyswift::DeclName RHS) {
    return LHS.getOpaqueValue() == RHS.getOpaqueValue();
  }
};
} // end namespace llvm

#endif // TINYSWIFT_IDENTIFIER_H
