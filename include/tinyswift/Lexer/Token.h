//
// Created by Satish on 16/07/22.
//

#ifndef TINYSWIFT_TOKEN_H
#define TINYSWIFT_TOKEN_H

#include "tinyswift/Source/SourceLoc.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace tinyswift {

enum class tok {
  unknown = 0,
  eof,
  code_complete,
  identifier,
  oper_binary_unspaced, // "x+y"
  oper_binary_spaced,   // "x + y"
  oper_postfix,
  oper_prefix,
  dollarident,
  integer_literal,
  floating_literal,
  string_literal,
  sil_local_name, // %42 in SIL mode.
  comment,

#define KEYWORD(X) kw_##X,
#define PUNCTUATOR(X, Y) X,
//#define POUND_KEYWORD(X) pound_##X,
#include "TokenKinds.def"

  NUM_TOKENS
};

/// Token - This structure provides full information about a lexed token.
/// It is not intended to be space efficient, it is intended to return as much
/// information as possible about each returned token.  This is expected to be
/// compressed into a smaller form if memory footprint is important.
///
class Token {
  /// Kind - The actual flavor of token this is.
  ///
  tok Kind;

  /// \brief Whether this token is the first token on the line.
  unsigned AtStartOfLine : 1;

  /// \brief The length of the comment that precedes the token.
  ///
  /// Hopefully 128 Mib is enough.
  unsigned CommentLength : 27;

  /// \brief Whether this token is an escaped `identifier` token.
  unsigned EscapedIdentifier : 1;

  /// Text - The actual string covered by the token in the source buffer.
  llvm::StringRef Text;

  llvm::StringRef trimComment() const {
    assert(hasComment() && "Has no comment to trim.");
    llvm::StringRef Raw(Text.begin() - CommentLength, CommentLength);
    return Raw.trim();
  }

public:
  Token()
      : Kind(tok::NUM_TOKENS), AtStartOfLine(false), CommentLength(0),
        EscapedIdentifier(false) {}

  tok getKind() const { return Kind; }

  void setKind(tok K) { Kind = K; }

  /// is/isNot - Predicates to check if this token is a specific kind, as in
  /// "if (Tok.is(tok::l_brace)) {...}".
  bool is(tok K) const { return Kind == K; }

  bool isNot(tok K) const { return Kind != K; }

  // Predicates to check to see if the token is any of a list of tokens.

  bool isAny(tok K1) const { return is(K1); }

  template <typename... T> bool isAny(tok K1, tok K2, T... K) const {
    if (is(K1))
      return true;
    return isAny(K2, K...);
  }

  // Predicates to check to see if the token is not the same as any of a list.
  template <typename... T> bool isNot(tok K1, T... K) const {
    return !isAny(K1, K...);
  }

  bool isBinaryOperator() const {
    return Kind == tok::oper_binary_spaced || Kind == tok::oper_binary_unspaced;
  }

  bool isAnyOperator() const {
    return isBinaryOperator() || Kind == tok::oper_postfix ||
           Kind == tok::oper_prefix;
  }

  bool isNotAnyOperator() const { return !isAnyOperator(); }

  bool isEllipsis() const { return isAnyOperator() && Text == "..."; }

  bool isNotEllipsis() const { return !isEllipsis(); }

  /// \brief Determine whether this token occurred at the start of a line.
  bool isAtStartOfLine() const { return AtStartOfLine; }

  /// \brief Set whether this token occurred at the start of a line.
  void setAtStartOfLine(bool value) { AtStartOfLine = value; }

  /// \brief True if this token is an escaped identifier token.
  bool isEscapedIdentifier() const { return EscapedIdentifier; }

  /// \brief Set whether this token is an escaped identifier token.
  void setEscapedIdentifier(bool value) {
    assert((!value || Kind == tok::identifier) &&
           "only identifiers can be escaped identifiers");
    EscapedIdentifier = value;
  }

  bool isContextualKeyword(llvm::StringRef ContextKW) const {
    return is(tok::identifier) && !isEscapedIdentifier() && Text == ContextKW;
  }

  /// Return true if this is a contextual keyword that could be the start of a
  /// decl.
  bool isContextualDeclKeyword() const {
    if (isNot(tok::identifier) || isEscapedIdentifier() || Text.empty())
      return false;

    switch (Text[0]) {
    case 'c':
      return Text == "convenience";
    case 'd':
      return Text == "dynamic";
    case 'f':
      return Text == "final";
    case 'i':
      return Text == "indirect" || Text == "infix";
    case 'l':
      return Text == "lazy";
    case 'm':
      return Text == "mutating";
    case 'n':
      return Text == "nonmutating";
    case 'o':
      return Text == "open" || Text == "override" || Text == "optional";
    case 'p':
      return Text == "prefix" || Text == "postfix";
    case 'r':
      return Text == "required";
    case 'u':
      return Text == "unowned";
    case 'w':
      return Text == "weak";
    default:
      return false;
    }
  }

  bool isContextualPunctuator(llvm::StringRef ContextPunc) const {
    return isAnyOperator() && Text == ContextPunc;
  }

  /// Determine whether the token can be an argument label.
  ///
  /// This covers all identifiers and keywords except those keywords
  /// used
  bool canBeArgumentLabel() const {
    // Identifiers, escaped identifiers, and '_' can be argument labels.
    if (is(tok::identifier) || isEscapedIdentifier() || is(tok::kw__))
      return true;

    // 'let', 'var', and 'inout' cannot be argument labels.
    if (isAny(tok::kw_let, tok::kw_var, tok::kw_inout))
      return false;

    // All other keywords can be argument labels.
    return isKeyword();
  }

  /// True if the token is an identifier or '_'.
  bool isIdentifierOrUnderscore() const {
    return isAny(tok::identifier, tok::kw__);
  }

  /// True if the token is an l_paren token that does not start a new line.
  bool isFollowingLParen() const {
    return !isAtStartOfLine() && Kind == tok::l_paren;
  }

  /// True if the token is an l_square token that does not start a new line.
  bool isFollowingLSquare() const {
    return !isAtStartOfLine() && Kind == tok::l_square;
  }

  /// True if the token is any keyword.
  bool isKeyword() const {
    switch (Kind) {
#define KEYWORD(X)                                                             \
  case tok::kw_##X:                                                            \
    return true;

#include "TokenKinds.def"

    default:
      return false;
    }
  }

  /// getLoc - Return a source location identifier for the specified
  /// offset in the current file.
  SourceLoc getLoc() const {
    return SourceLoc(llvm::SMLoc::getFromPointer(Text.begin()));
  }

  unsigned getLength() const { return Text.size(); }

  CharSourceRange getRange() const {
    return CharSourceRange(getLoc(), getLength());
  }

  bool hasComment() const { return CommentLength != 0; }

  CharSourceRange getCommentRange() const {
    if (CommentLength == 0)
      return CharSourceRange(
          SourceLoc(llvm::SMLoc::getFromPointer(Text.begin())), 0);
    auto TrimedComment = trimComment();
    return CharSourceRange(
        SourceLoc(llvm::SMLoc::getFromPointer(TrimedComment.begin())),
        TrimedComment.size());
  }

  SourceLoc getCommentStart() const {
    if (CommentLength == 0)
      return SourceLoc();
    return SourceLoc(llvm::SMLoc::getFromPointer(trimComment().begin()));
  }

  llvm::StringRef getRawText() const { return Text; }

  llvm::StringRef getText() const {
    if (EscapedIdentifier) {
      // Strip off the backticks on either side.
      assert(Text.front() == '`' && Text.back() == '`');
      return Text.slice(1, Text.size() - 1);
    }
    return Text;
  }

  void setText(llvm::StringRef T) { Text = T; }

  /// \brief Set the token to the specified kind and source range.
  void setToken(tok K, llvm::StringRef T, unsigned CommentLength = 0) {
    Kind = K;
    Text = T;
    this->CommentLength = CommentLength;
    EscapedIdentifier = false;
  }
};
} // namespace tinyswift

#endif // TINYSWIFT_TOKEN_H
