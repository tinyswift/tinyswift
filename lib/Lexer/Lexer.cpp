//
// Created by Satish on 16/07/22.
//

#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/raw_ostream.h>
#include "tinyswift/Lexer/Lexer.h"
#include "tinyswift/Lexer/StringHelper.h"

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

Token Lexer::lexToken() {
    while (true) {
        const char *tokStart = curPtr;

        // Check to see if the current token is at the code completion location.
        if (tokStart == codeCompleteLoc)
            return formToken(Token::code_complete, tokStart);

        // Lex the next token.
        switch (*curPtr++) {
            default:
                // Unknown character, emit an error.
                return emitError(tokStart, "unexpected character");
            case ' ':
            case '\t':
            case '\n':
            case '\r':
                // Handle whitespace.
                continue;
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

            case '"':
            case '\'':
                return lexString(tokStart);

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
        }
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
//                llvm_unreachable("lex_unprintable_ascii_character");
                return CurPtr[-1];
            }
            --CurPtr;
            unsigned CharValue = validateUTF8CharacterAndAdvance(CurPtr, getBufferEnd());
            if (CharValue != ~0U) return CharValue;
//            llvm_unreachable("Invalid UTF8 character\n");
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
//            llvm_unreachable("unterminated string literal");
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
