//
// Created by Satish on 20/07/22.
//

#ifndef TINYSWIFT_SOURCELOC_H
#define TINYSWIFT_SOURCELOC_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>

namespace tinyswift {
class SourceManager;

/// SourceLoc in swift is just an SMLoc.  We define it as a different type
/// (instead of as a typedef) just to remove the "getFromPointer" methods and
/// enforce purity in the Swift codebase.
class SourceLoc {
  friend class SourceManager;

  friend class SourceRange;

  friend class CharSourceRange;

  friend class DiagnosticConsumer;

  llvm::SMLoc Value;

public:
  SourceLoc() {}

  explicit SourceLoc(llvm::SMLoc Value) : Value(Value) {}

  bool isValid() const { return Value.isValid(); }

  bool isInvalid() const { return !isValid(); }

  bool operator==(const SourceLoc &RHS) const { return RHS.Value == Value; }

  bool operator!=(const SourceLoc &RHS) const { return !operator==(RHS); }

  /// Return a source location advanced a specified number of bytes.
  SourceLoc getAdvancedLoc(int ByteOffset) const {
    assert(isValid() && "Can't advance an invalid location");
    return SourceLoc(
        llvm::SMLoc::getFromPointer(Value.getPointer() + ByteOffset));
  }

  SourceLoc getAdvancedLocOrInvalid(int ByteOffset) const {
    if (isValid())
      return getAdvancedLoc(ByteOffset);
    return SourceLoc();
  }

  const void *getOpaquePointerValue() const { return Value.getPointer(); }

  /// Print out the SourceLoc.  If this location is in the same buffer
  /// as specified by \c LastBufferID, then we don't print the filename.  If
  /// not, we do print the filename, and then update \c LastBufferID with the
  /// BufferID printed.
  void print(llvm::raw_ostream &OS, const SourceManager &SM,
             unsigned &LastBufferID) const;

  void printLineAndColumn(llvm::raw_ostream &OS, const SourceManager &SM) const;

  void print(llvm::raw_ostream &OS, const SourceManager &SM) const {
    unsigned Tmp = ~0U;
    print(OS, SM, Tmp);
  }

  void dump(const SourceManager &SM) const;
};

/// SourceRange in swift is a pair of locations.  However, note that the end
/// location is the start of the last token in the range, not the last character
/// in the range.  This is unlike SMRange, so we use a distinct type to make
/// sure that proper conversions happen where important.
class SourceRange {
public:
  SourceLoc Start, End;

  SourceRange() {}

  SourceRange(SourceLoc Loc) : Start(Loc), End(Loc) {}

  SourceRange(SourceLoc Start, SourceLoc End) : Start(Start), End(End) {
    assert(Start.isValid() == End.isValid() &&
           "Start and end should either both be valid or both be invalid!");
  }

  bool isValid() const { return Start.isValid(); }

  bool isInvalid() const { return !isValid(); }

  bool operator==(const SourceRange &other) const {
    return Start == other.Start && End == other.End;
  }

  bool operator!=(const SourceRange &other) const { return !operator==(other); }

  /// Print out the SourceRange.  If the locations are in the same buffer
  /// as specified by LastBufferID, then we don't print the filename.  If not,
  /// we do print the filename, and then update LastBufferID with the BufferID
  /// printed.
  void print(llvm::raw_ostream &OS, const SourceManager &SM,
             unsigned &LastBufferID, bool PrintText = true) const;

  void print(llvm::raw_ostream &OS, const SourceManager &SM,
             bool PrintText = true) const {
    unsigned Tmp = ~0U;
    print(OS, SM, Tmp, PrintText);
  }

  void dump(const SourceManager &SM) const;
};

/// A half-open character-based source range.
class CharSourceRange {
  SourceLoc Start;
  unsigned ByteLength;

public:
  /// \brief Constructs an invalid range.
  CharSourceRange() {}

  CharSourceRange(SourceLoc Start, unsigned ByteLength)
      : Start(Start), ByteLength(ByteLength) {}

  /// \brief Constructs a character range which starts and ends at the
  /// specified character locations.
  CharSourceRange(const SourceManager &SM, SourceLoc Start, SourceLoc End);

  /// Use Lexer::getCharSourceRangeFromSourceRange() instead.
  CharSourceRange(const SourceManager &SM, SourceRange Range) = delete;

  bool isValid() const { return Start.isValid(); }

  bool isInvalid() const { return !isValid(); }

  bool operator==(const CharSourceRange &other) const {
    return Start == other.Start && ByteLength == other.ByteLength;
  }

  bool operator!=(const CharSourceRange &other) const {
    return !operator==(other);
  }

  SourceLoc getStart() const { return Start; }

  SourceLoc getEnd() const { return Start.getAdvancedLocOrInvalid(ByteLength); }

  /// Returns true if the given source location is contained in the range.
  bool contains(SourceLoc loc) const {
    auto less = std::less<const char *>();
    auto less_equal = std::less_equal<const char *>();
    return less_equal(getStart().Value.getPointer(), loc.Value.getPointer()) &&
           less(loc.Value.getPointer(), getEnd().Value.getPointer());
  }

  bool contains(CharSourceRange Other) const {
    auto less_equal = std::less_equal<const char *>();
    return contains(Other.getStart()) &&
           less_equal(Other.getEnd().Value.getPointer(),
                      getEnd().Value.getPointer());
  }

  /// \brief expands *this to cover Other
  void widen(CharSourceRange Other) {
    auto Diff = Other.getEnd().Value.getPointer() - getEnd().Value.getPointer();
    if (Diff > 0) {
      ByteLength += Diff;
    }
    const auto MyStartPtr = getStart().Value.getPointer();
    Diff = MyStartPtr - Other.getStart().Value.getPointer();
    if (Diff > 0) {
      ByteLength += Diff;
      Start = SourceLoc(llvm::SMLoc::getFromPointer(MyStartPtr - Diff));
    }
  }

  bool overlaps(CharSourceRange Other) const {
    return contains(Other.getStart()) || contains(Other.getEnd());
  }

  llvm::StringRef str() const {
    return llvm::StringRef(Start.Value.getPointer(), ByteLength);
  }

  /// \brief Return the length of this valid range in bytes.  Can be zero.
  unsigned getByteLength() const {
    assert(isValid() && "length does not make sense for an invalid range");
    return ByteLength;
  }

  /// Print out the CharSourceRange.  If the locations are in the same buffer
  /// as specified by LastBufferID, then we don't print the filename.  If not,
  /// we do print the filename, and then update LastBufferID with the BufferID
  /// printed.
  void print(llvm::raw_ostream &OS, const SourceManager &SM,
             unsigned &LastBufferID, bool PrintText = true) const;

  void print(llvm::raw_ostream &OS, const SourceManager &SM,
             bool PrintText = true) const {
    unsigned Tmp = ~0U;
    print(OS, SM, Tmp, PrintText);
  }

  void dump(const SourceManager &SM) const;
};

} // namespace tinyswift

#endif // TINYSWIFT_SOURCELOC_H
