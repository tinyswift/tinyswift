//
// Created by Satish on 16/07/22.
//

#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/raw_ostream.h>
#include "tinyswift/Lexer/Lexer.h"
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
/// Lex a string literal.
///
///   string-literal ::= '"' [^"\n\f\v\r]* '"' MLIR
///
/// TODO: define escaping rules.
Token Lexer::lexString(const char *tokStart) {
    assert(curPtr[-1] == '"');

    while (true) {
        // Check to see if there is a code completion location within the string. In
        // these cases we generate a completion location and place the currently
        // lexed string within the token. This allows for the parser to use the
        // partially lexed string when computing the completion results.
        if (curPtr == codeCompleteLoc)
            return formToken(Token::code_complete, tokStart);

        switch (*curPtr++) {
            case '"':
                return formToken(Token::string_literal, tokStart);
            case 0:
                // If this is a random nul character in the middle of a string, just
                // include it.  If it is the end of file, then it is an error.
                if (curPtr - 1 != curBuffer.end())
                    continue;
                LLVM_FALLTHROUGH;
            case '\n':
            case '\v':
            case '\f':
                return emitError(curPtr - 1, "expected '\"' in string literal");
            case '\\':
                // Handle explicitly a few escapes.
                if (*curPtr == '"' || *curPtr == '\\' || *curPtr == 'n' || *curPtr == 't')
                    ++curPtr;
                else if (llvm::isHexDigit(*curPtr) && llvm::isHexDigit(curPtr[1]))
                    // Support \xx for two hex digits.
                    curPtr += 2;
                else
                    return emitError(curPtr - 1, "unknown escape in string literal");
                continue;

            default:
                continue;
        }
    }
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

/// lexOperatorIdentifier - Match identifiers formed out of punctuation.
Token Lexer::lexOperatorIdentifier(const char *tokStart) {

    // Decide between the binary, prefix, and postfix cases.
    // It's binary if either both sides are bound or both sides are not bound.
    // Otherwise, it's postfix if left-bound and prefix if right-bound.

    return formToken(Token::error, tokStart);
}

Token Lexer::lexEscapedIdentifier(const char *start) {
    return Token(Token::error, llvm::StringRef());
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

            case '`':
                return lexEscapedIdentifier(tokStart);
        }
    }
}