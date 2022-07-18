//
// Created by Satish on 16/07/22.
//

#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/raw_ostream.h>
#include "tinyswift/Lexer/Lexer.h"
#include "tinyswift/Lexer/StringHelper.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Regex.h"

using namespace tinyswift;

Lexer::Lexer(const llvm::SourceMgr &sourceMgr) : sourceMgr(sourceMgr) {
    auto bufferID = sourceMgr.getMainFileID();
    curBuffer = sourceMgr.getMemoryBuffer(bufferID)->getBuffer();
    curPtr = curBuffer.begin();
}

/// emitError - Emit an error message and return an Token::error token.
Token Lexer::emitError(const char *loc, const llvm::Twine &message) {

    // get source location
    llvm::SMLoc location = llvm::SMLoc::getFromPointer(loc);
    // get line number and column number
    auto lineAndColumn = sourceMgr.getLineAndColumn(location);
    // emit error message
    llvm::errs() << "Error: " << lineAndColumn.first << ":" << lineAndColumn.second << ": " << message << "\n";

    return formToken(Token::error, loc);
}

/// Is the operator beginning at the given character "left-bound"?
static bool isLeftBound(const char *tokBegin, const char *bufferBegin) {
    // The first character in the file is not left-bound.
    if (tokBegin == bufferBegin) return false;

    switch (tokBegin[-1]) {
        case ' ':
        case '\r':
        case '\n':
        case '\t': // whitespace
        case '(':
        case '[':
        case '{':              // opening delimiters
        case ',':
        case ';':
        case ':':              // expression separators
        case '\0':                                 // whitespace / last char in file
            return false;

        case '/':
            if (tokBegin - 1 != bufferBegin && tokBegin[-2] == '*')
                return false; // End of a slash-star comment, so whitespace.
            else
                return true;

        default:
            return true;
    }
}

/// Is the operator ending at the given character (actually one past the end)
/// "right-bound"?
///
/// The code-completion point is considered right-bound.
static bool isRightBound(const char *tokEnd, bool isLeftBound,
                         const char *codeCompletionPtr) {
    switch (*tokEnd) {
        case ' ':
        case '\r':
        case '\n':
        case '\t': // whitespace
        case ')':
        case ']':
        case '}':              // closing delimiters
        case ',':
        case ';':
        case ':':              // expression separators
            return false;

        case '\0':
            if (tokEnd == codeCompletionPtr)         // code-completion
                return true;
            return false;                            // whitespace / last char in file

        case '.':
            // Prefer the '^' in "x^.y" to be a postfix op, not binary, but the '^' in
            // "^.y" to be a prefix op, not binary.
            return !isLeftBound;

        case '/':
            // A following comment counts as whitespace, so this token is not right bound.
            if (tokEnd[1] == '/' || tokEnd[1] == '*')
                return false;
            else
                return true;

        default:
            return true;
    }
}

/// Skip a comment line, starting with a '//'.
void Lexer::skipComment() {
    // Advance over the second '/' in a '//' comment.
    assert(*curPtr == '/');
    ++curPtr;

    while (true) {
        switch (*curPtr++) {
            case '\n':
            case '\r':
                // Newline is end of comment.
                return;
            case 0:
                // If this is the end of the buffer, end the comment.
                if (curPtr - 1 == curBuffer.end()) {
                    --curPtr;
                    return;
                }
                LLVM_FALLTHROUGH;
            default:
                // Skip over other characters.
                break;
        }
    }
}

/// Lex a number literal.
///
///   integer-literal ::= digit+ | `0x` hex_digit+
///   float-literal ::= [-+]?[0-9]+[.][0-9]*([eE][-+]?[0-9]+)?
///
Token Lexer::lexNumber(const char *tokStart) {
    assert(isdigit(curPtr[-1]));

    // Handle the hexadecimal case.
    if (curPtr[-1] == '0' && *curPtr == 'x') {
        // If we see stuff like 0xi32, this is a literal `0` followed by an
        // identifier `xi32`, stop after `0`.
        if (!isxdigit(curPtr[1]))
            return formToken(Token::integer_literal, tokStart);

        curPtr += 2;
        while (isxdigit(*curPtr))
            ++curPtr;

        return formToken(Token::integer_literal, tokStart);
    }

    // Handle the normal decimal case.
    while (isdigit(*curPtr))
        ++curPtr;

    if (*curPtr != '.')
        return formToken(Token::integer_literal, tokStart);
    ++curPtr;

    // Skip over [0-9]*([eE][-+]?[0-9]+)?
    while (isdigit(*curPtr))
        ++curPtr;

    if (*curPtr == 'e' || *curPtr == 'E') {
        if (isdigit(static_cast<unsigned char>(curPtr[1])) ||
            ((curPtr[1] == '-' || curPtr[1] == '+') &&
             isdigit(static_cast<unsigned char>(curPtr[2])))) {
            curPtr += 2;
            while (isdigit(*curPtr))
                ++curPtr;
        }
    }
    return formToken(Token::floating_literal, tokStart);
}

/// Lex a string literal.
///
///   string_literal ::= ["]([^"\\\n\r]|character_escape)*["]
///
Token Lexer::lexString(const char *tokStart) {
    assert(curPtr[-1] == '"' || curPtr[-1] == '\'');

    bool wasErroneous = false;
    while (true) {
        if (*curPtr == '\\' && *(curPtr + 1) == '(') {
            // Consume tokens until we hit the corresponding ')'.
            curPtr += 2;
            const char *endPtr =
                    skipToEndOfInterpolatedExpression(curPtr, getBufferEnd());

            if (*endPtr == ')') {
                // Successfully scanned the body of the expression literal.
                curPtr = endPtr + 1;
            } else {
                curPtr = endPtr;
                wasErroneous = true;
            }
            continue;
        }

        // String literals cannot have \n or \r in them.
        if (*curPtr == '\r' || *curPtr == '\n' || curPtr == getBufferEnd()) {
            return formToken(Token::unknown, tokStart);
        }

        unsigned CharValue = lexCharacter(curPtr, *tokStart);
        wasErroneous |= CharValue == ~1U;

        // If this is the end of string, we are done.  If it is a normal character
        // or an already-diagnosed error, just munch it.
        if (CharValue == ~0U) {
            curPtr++;
            if (wasErroneous)
                return formToken(Token::unknown, tokStart);

            if (*curPtr == '\'') {
                // Complain about single-quote string and suggest replacement with
                // double-quoted equivalent.
                llvm::StringRef orig(tokStart, curPtr - tokStart);
                llvm::SmallString<32> replacement;
                replacement += '"';
                std::string str = orig.slice(1, orig.size() - 1).str();
                std::string quot = "\"";
                size_t pos = 0;
                while (pos != str.length()) {
                    if (str.at(pos) == '\\') {
                        if (str.at(pos + 1) == '\'') {
                            // Un-escape escaped single quotes.
                            str.replace(pos, 2, "'");
                            ++pos;
                        } else {
                            // Skip over escaped characters.
                            pos += 2;
                        }
                    } else if (str.at(pos) == '"') {
                        str.replace(pos, 1, "\\\"");
                        // Advance past the newly added ["\""].
                        pos += 2;
                    } else {
                        ++pos;
                    }
                }
                replacement += llvm::StringRef(str);
                replacement += '"';
            }
            return formToken(Token::string_literal, tokStart);
        }
    }
}

/// lexCharacter - Read a character and return its UTF32 code.  If this is the
/// end of enclosing string/character sequence (i.e. the character is equal to
/// 'StopQuote'), this returns ~0U and leaves 'CurPtr' pointing to the terminal
/// quote.  If this is a malformed character sequence, it emits a diagnostic
/// (when EmitDiagnostics is true) and returns ~1U.
///
///   character_escape  ::= [\][\] | [\]t | [\]n | [\]r | [\]" | [\]' | [\]0
///   character_escape  ::= unicode_character_escape
unsigned Lexer::lexCharacter(const char *&CurPtr, char StopQuote) {
    const char *CharStart = CurPtr;

    switch (*CurPtr++) {
        default: {// Normal characters are part of the string.
            // If this is a "high" UTF-8 character, validate it.
            if ((signed char) (CurPtr[-1]) >= 0) {
                return CurPtr[-1];
            }
            --CurPtr;
            unsigned CharValue = validateUTF8CharacterAndAdvance(CurPtr, getBufferEnd());
            if (CharValue != ~0U) return CharValue;
            return ~1U;
        }
        case '"':
        case '\'':
            // If we found a closing quote character, we're done.
            if (CurPtr[-1] == StopQuote) {
                --CurPtr;
                return ~0U;
            }
            // Otherwise, this is just a character.
            return CurPtr[-1];

        case 0:
            if (CurPtr - 1 != getBufferEnd()) {
                // If we found a null character, we're done.
                return CurPtr[-1];
            }
            // Move the pointer back to EOF.
            --CurPtr;
            LLVM_FALLTHROUGH;
        case '\n':  // String literals cannot have \n or \r in them.
        case '\r':
            return ~1U;
        case '\\':  // Escapes.
            break;
    }

    unsigned CharValue = 0;
    // Escape processing.  We already ate the "\".
    switch (*CurPtr) {
        default:  // Invalid escape.
            llvm::errs() << "Lexer::lexCharacter: Invalid escape.\n";
            // If this looks like a plausible escape character, recover as though this
            // is an invalid escape.
            if (llvm::isAlnum(*CurPtr)) ++CurPtr;
            return ~1U;

            // Simple single-character escapes.
        case '0':
            ++CurPtr;
            return '\0';
        case 'n':
            ++CurPtr;
            return '\n';
        case 'r':
            ++CurPtr;
            return '\r';
        case 't':
            ++CurPtr;
            return '\t';
        case '"':
            ++CurPtr;
            return '"';
        case '\'':
            ++CurPtr;
            return '\'';
        case '\\':
            ++CurPtr;
            return '\\';
        case 'u': {  //  \u HEX HEX HEX HEX
            ++CurPtr;
            if (*CurPtr != '{') {
                llvm::errs() << "Lexer::lexCharacter: Invalid unicode escape.\n";
                return ~1U;
            }

            CharValue = lexUnicodeEscape(CurPtr);
            if (CharValue == ~1U) return ~1U;
            break;
        }
    }

    // Check to see if the encoding is valid.
    llvm::SmallString<64> TempString;
    if (CharValue >= 0x80 && EncodeToUTF8(CharValue, TempString)) {
        llvm::errs() << "Lexer::lexCharacter: Invalid unicode scalar.\n";
        return ~1U;
    }

    return CharValue;
}

///   unicode_character_escape ::= [\]u{hex+}
///   hex                      ::= [0-9a-fA-F]
unsigned Lexer::lexUnicodeEscape(const char *&CurPtr) {
    assert(CurPtr[0] == '{' && "Invalid unicode escape");
    ++CurPtr;

    const char *DigitStart = CurPtr;

    unsigned NumDigits = 0;
    for (; llvm::isHexDigit(CurPtr[0]); ++NumDigits)
        ++CurPtr;

    if (CurPtr[0] != '}') {
        llvm::errs() << "Lexer::lexUnicodeEscape: Invalid unicode escape.\n";
        return ~1U;
    }
    ++CurPtr;

    if (NumDigits < 1 || NumDigits > 8) {
        llvm::errs() << "Lexer::lexUnicodeEscape: Invalid unicode escape.\n";
        return ~1U;
    }

    unsigned CharValue = 0;
    llvm::StringRef(DigitStart, NumDigits).getAsInteger(16, CharValue);
    return CharValue;
}


static bool isValidIdentifierContinuationCodePoint(uint32_t c) {
//    if (c < 0x80)
//        return clang::isIdentifierBody(c, /*dollar*/true);

    // N1518: Recommendations for extended identifier characters for C and C++
    // Proposed Annex X.1: Ranges of characters allowed
    return c == 0x00A8 || c == 0x00AA || c == 0x00AD || c == 0x00AF
           || (c >= 0x00B2 && c <= 0x00B5) || (c >= 0x00B7 && c <= 0x00BA)
           || (c >= 0x00BC && c <= 0x00BE) || (c >= 0x00C0 && c <= 0x00D6)
           || (c >= 0x00D8 && c <= 0x00F6) || (c >= 0x00F8 && c <= 0x00FF)

           || (c >= 0x0100 && c <= 0x167F)
           || (c >= 0x1681 && c <= 0x180D)
           || (c >= 0x180F && c <= 0x1FFF)

           || (c >= 0x200B && c <= 0x200D)
           || (c >= 0x202A && c <= 0x202E)
           || (c >= 0x203F && c <= 0x2040)
           || c == 0x2054
           || (c >= 0x2060 && c <= 0x206F)

           || (c >= 0x2070 && c <= 0x218F)
           || (c >= 0x2460 && c <= 0x24FF)
           || (c >= 0x2776 && c <= 0x2793)
           || (c >= 0x2C00 && c <= 0x2DFF)
           || (c >= 0x2E80 && c <= 0x2FFF)

           || (c >= 0x3004 && c <= 0x3007)
           || (c >= 0x3021 && c <= 0x302F)
           || (c >= 0x3031 && c <= 0x303F)

           || (c >= 0x3040 && c <= 0xD7FF)

           || (c >= 0xF900 && c <= 0xFD3D)
           || (c >= 0xFD40 && c <= 0xFDCF)
           || (c >= 0xFDF0 && c <= 0xFE44)
           || (c >= 0xFE47 && c <= 0xFFF8)

           || (c >= 0x10000 && c <= 0x1FFFD)
           || (c >= 0x20000 && c <= 0x2FFFD)
           || (c >= 0x30000 && c <= 0x3FFFD)
           || (c >= 0x40000 && c <= 0x4FFFD)
           || (c >= 0x50000 && c <= 0x5FFFD)
           || (c >= 0x60000 && c <= 0x6FFFD)
           || (c >= 0x70000 && c <= 0x7FFFD)
           || (c >= 0x80000 && c <= 0x8FFFD)
           || (c >= 0x90000 && c <= 0x9FFFD)
           || (c >= 0xA0000 && c <= 0xAFFFD)
           || (c >= 0xB0000 && c <= 0xBFFFD)
           || (c >= 0xC0000 && c <= 0xCFFFD)
           || (c >= 0xD0000 && c <= 0xDFFFD)
           || (c >= 0xE0000 && c <= 0xEFFFD);
}

static bool isValidIdentifierStartCodePoint(uint32_t c) {
    if (!isValidIdentifierContinuationCodePoint(c))
        return false;
    if (c < 0x80 && (llvm::isDigit(c) || c == '$'))
        return false;

    // N1518: Recommendations for extended identifier characters for C and C++
    // Proposed Annex X.2: Ranges of characters disallowed initially
    if ((c >= 0x0300 && c <= 0x036F) ||
        (c >= 0x1DC0 && c <= 0x1DFF) ||
        (c >= 0x20D0 && c <= 0x20FF) ||
        (c >= 0xFE20 && c <= 0xFE2F))
        return false;

    return true;
}

static bool advanceIf(char const *&ptr, char const *end,
                      bool (*predicate)(uint32_t)) {
    char const *next = ptr;
    uint32_t c = validateUTF8CharacterAndAdvance(next, end);
    if (c == ~0U)
        return false;
    if (predicate(c)) {
        ptr = next;
        return true;
    }
    return false;

}

/// isOperatorStartCodePoint - Return true if the specified code point is a
/// valid start of an operator.
static bool isOperatorStartCodePoint(uint32_t C) {
    // ASCII operator chars.
    static const char OpChars[] = "/=-+*%<>!&|^~.?";
    if (C < 0x80)
        return memchr(OpChars, C, sizeof(OpChars) - 1) != 0;

    // Unicode math, symbol, arrow, dingbat, and line/box drawing chars.
    return (C >= 0x00A1 && C <= 0x00A7)
           || C == 0x00A9 || C == 0x00AB || C == 0x00AC || C == 0x00AE
           || C == 0x00B0 || C == 0x00B1 || C == 0x00B6 || C == 0x00BB
           || C == 0x00BF || C == 0x00D7 || C == 0x00F7
           || C == 0x2016 || C == 0x2017 || (C >= 0x2020 && C <= 0x2027)
           || (C >= 0x2030 && C <= 0x203E) || (C >= 0x2041 && C <= 0x2053)
           || (C >= 0x2055 && C <= 0x205E) || (C >= 0x2190 && C <= 0x23FF)
           || (C >= 0x2500 && C <= 0x2775) || (C >= 0x2794 && C <= 0x2BFF)
           || (C >= 0x2E00 && C <= 0x2E7F) || (C >= 0x3001 && C <= 0x3003)
           || (C >= 0x3008 && C <= 0x3030);
}

/// isOperatorContinuationCodePoint - Return true if the specified code point
/// is a valid operator code point.
static bool isOperatorContinuationCodePoint(uint32_t C) {
    if (isOperatorStartCodePoint(C))
        return true;

    // Unicode combining characters and variation selectors.
    return (C >= 0x0300 && C <= 0x036F)
           || (C >= 0x1DC0 && C <= 0x1DFF)
           || (C >= 0x20D0 && C <= 0x20FF)
           || (C >= 0xFE00 && C <= 0xFE0F)
           || (C >= 0xFE20 && C <= 0xFE2F)
           || (C >= 0xE0100 && C <= 0xE01EF);
}

static bool advanceIfValidStartOfIdentifier(char const *&ptr,
                                            char const *end) {
    return advanceIf(ptr, end, isValidIdentifierStartCodePoint);
}

static bool advanceIfValidContinuationOfIdentifier(char const *&ptr,
                                                   char const *end) {
    return advanceIf(ptr, end, isValidIdentifierContinuationCodePoint);
}

static bool advanceIfValidStartOfOperator(char const *&ptr,
                                          char const *end) {
    return advanceIf(ptr, end, isOperatorStartCodePoint);
}

static bool advanceIfValidContinuationOfOperator(char const *&ptr,
                                                 char const *end) {
    return advanceIf(ptr, end, isOperatorContinuationCodePoint);
}

/// lexOperatorIdentifier - Match identifiers formed out of punctuation.
Token Lexer::lexOperatorIdentifier(const char *tokStart) {
    const char *TokStart = curPtr - 1;
    curPtr = TokStart;
    bool didStart = advanceIfValidStartOfOperator(curPtr, getBufferEnd());
    assert(didStart && "unexpected operator start");
    (void) didStart;

    do {

        // '.' cannot appear in the middle of an operator unless the operator
        // started with a '.'.
        if (*curPtr == '.' && *TokStart != '.')
            break;
    } while (advanceIfValidContinuationOfOperator(curPtr, getBufferEnd()));

    if (curPtr - TokStart > 2) {
        // If there is a "//" or "/*" in the middle of an identifier token,
        // it starts a comment.
        for (auto Ptr = TokStart + 1; Ptr != curPtr - 1; ++Ptr) {
            if (Ptr[0] == '/' && (Ptr[1] == '/' || Ptr[1] == '*')) {
                curPtr = Ptr;
                break;
            }
        }
    }

    // Decide between the binary, prefix, and postfix cases.
    // It's binary if either both sides are bound or both sides are not bound.
    // Otherwise, it's postfix if left-bound and prefix if right-bound.
    bool leftBound = isLeftBound(TokStart, getBufferBegin());
    bool rightBound = isRightBound(curPtr, leftBound, codeCompleteLoc);

    // Match various reserved words.
    if (curPtr - TokStart == 1) {
        switch (TokStart[0]) {
            case '=':
                if (leftBound != rightBound) {
                    return emitError(tokStart, "invalid operator '='");
                }
                // always emit 'tok::equal' to avoid trickle down parse errors
                return formToken(Token::equal, TokStart);
            case '&':
                if (leftBound == rightBound || leftBound)
                    break;
                return formToken(Token::amp_prefix, TokStart);
            case '.': {
                if (leftBound == rightBound)
                    return formToken(Token::period, TokStart);
                if (rightBound)
                    return formToken(Token::period_prefix, TokStart);

                // If left bound but not right bound, handle some likely situations.

                // If there is just some horizontal whitespace before the next token, its
                // addition is probably incorrect.
                const char *AfterHorzWhitespace = curPtr;
                while (*AfterHorzWhitespace == ' ' || *AfterHorzWhitespace == '\t')
                    ++AfterHorzWhitespace;

                // First, when we are code completing "x. <ESC>", then make sure to return
                // a tok::period, since that is what the user is wanting to know about.
                if (*AfterHorzWhitespace == '\0' &&
                    AfterHorzWhitespace == codeCompleteLoc) {

                    return formToken(Token::period, TokStart);
                }

                if (isRightBound(AfterHorzWhitespace, leftBound, codeCompleteLoc) &&
                    // Don't consider comments to be this.  A leading slash is probably
                    // either // or /* and most likely occurs just in our testsuite for
                    // expected-error lines.
                    *AfterHorzWhitespace != '/') {
                    return formToken(Token::period, TokStart);
                }

                // Otherwise, it is probably a missing member.
                return formToken(Token::unknown, TokStart);
            }
            case '?':
                if (leftBound)
                    return formToken(Token::question_postfix, TokStart);
                return formToken(Token::question_infix, TokStart);
        }
    } else if (curPtr - TokStart == 2) {
        switch ((TokStart[0] << 8) | TokStart[1]) {
            case ('-' << 8) | '>': // ->
                return formToken(Token::arrow, TokStart);
            case ('*' << 8) | '/': // */
                return formToken(Token::unknown, TokStart);
        }
    } else {
        // Verify there is no "*/" in the middle of the identifier token, we reject
        // it as potentially ending a block comment.
        auto Pos = llvm::StringRef(TokStart, curPtr - TokStart).find("*/");
        if (Pos != llvm::StringRef::npos) {
            return formToken(Token::unknown, TokStart);
        }
    }

    if (leftBound == rightBound)
        return formToken(leftBound ? Token::oper_binary_unspaced :
                         Token::oper_binary_spaced, TokStart);

    return formToken(leftBound ? Token::oper_postfix : Token::oper_prefix, TokStart);
}

/// lexIdentifier - Match [a-zA-Z_][a-zA-Z_$0-9]*
Token Lexer::lexIdentifier(const char *tokStart) {

    // Match the rest of the identifier regex: [a-zA-Z_][a-zA-Z_$0-9]*
    llvm::Regex IdentifierRegex("[a-zA-Z_][a-zA-Z_$0-9]*");

    while (isalpha(*curPtr) || *curPtr == '_' ||
           *curPtr == '$' || isdigit(*curPtr))
        ++curPtr;

    // Check to see if this identifier is a keyword.
    llvm::StringRef spelling(tokStart, curPtr - tokStart);

    Token::Kind kind = llvm::StringSwitch<Token::Kind>(spelling)
#define KEYWORD(SPELLING) .Case(#SPELLING, Token::kw_##SPELLING)

#include "tinyswift/Lexer/TokenKinds.def"

            .Default(Token::identifier);

    if (!IdentifierRegex.match(spelling) && kind == Token::Kind::identifier) {
        return formToken(Token::unknown, tokStart);
    }

    return Token(kind, spelling);
}

Token Lexer::lexToken() {
    while (true) {
        const char *tokStart = curPtr;

        // Check to see if the current token is at the code completion location.
        if (tokStart == codeCompleteLoc)
            return formToken(Token::code_complete, tokStart);

        // Lex the next token.
        switch (*curPtr++) {
            default: {
                // Unknown character, emit an error.
                return emitError(tokStart, "unexpected character");
            }
            case ' ':
            case '\n':
            case '\r':
            case '\t':
            case '\f':
            case '\v':
                // Handle whitespace.
                continue;

            case -1:
            case -2:
                llvm::errs() << "UTF-16 BOM marker found\n";
                curPtr = getBufferEnd();
                return formToken(Token::eof, tokStart);
            case 0:
                // This may either be a nul character in the source file or may be the EOF
                // marker that llvm::MemoryBuffer guarantees will be there.
                if (curPtr - 1 == curBuffer.end())
                    return formToken(Token::eof, tokStart);
                continue;

            case '/':
                if (*curPtr == '/') {
                    skipComment();
                    continue;
                }
                return emitError(tokStart, "unexpected character");

            case '!':
                if (isLeftBound(tokStart, getBufferBegin()))
                    return formToken(Token::exclaim_postfix, tokStart);
                return lexOperatorIdentifier(tokStart);

            case '?':
                if (isLeftBound(tokStart, getBufferBegin()))
                    return formToken(Token::question_postfix, tokStart);
                return lexOperatorIdentifier(tokStart);

            case '%':
            case '<':
            case '>':
            case '=':
            case '-':
            case '+':
            case '*':
            case '&':
            case '|':
            case '^':
            case '~':
            case '.':
                return lexOperatorIdentifier(tokStart);

            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
            case 'G':
            case 'H':
            case 'I':
            case 'J':
            case 'K':
            case 'L':
            case 'M':
            case 'N':
            case 'O':
            case 'P':
            case 'Q':
            case 'R':
            case 'S':
            case 'T':
            case 'U':
            case 'V':
            case 'W':
            case 'X':
            case 'Y':
            case 'Z':
            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'e':
            case 'f':
            case 'g':
            case 'h':
            case 'i':
            case 'j':
            case 'k':
            case 'l':
            case 'm':
            case 'n':
            case 'o':
            case 'p':
            case 'q':
            case 'r':
            case 's':
            case 't':
            case 'u':
            case 'v':
            case 'w':
            case 'x':
            case 'y':
            case 'z':
            case '_':
                return lexIdentifier(tokStart);

            case '@':
                return formToken(Token::at_sign, tokStart);
            case '{':
                return formToken(Token::l_brace, tokStart);
            case '[':
                return formToken(Token::l_square, tokStart);
            case '(':
                return formToken(Token::l_paren, tokStart);
            case '}':
                return formToken(Token::r_brace, tokStart);
            case ']':
                return formToken(Token::r_square, tokStart);
            case ')':
                return formToken(Token::r_paren, tokStart);
            case ',':
                return formToken(Token::comma, tokStart);
            case ';':
                return formToken(Token::semi, tokStart);
            case ':':
                return formToken(Token::colon, tokStart);

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                return lexNumber(tokStart);

            case '"':
            case '\'':
                return lexString(tokStart);
        }
    }
}
