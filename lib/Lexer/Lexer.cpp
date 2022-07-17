//
// Created by Satish on 16/07/22.
//

#include <llvm/ADT/StringExtras.h>
#include "tinyswift/Lexer/Lexer.h"

using namespace tinyswift;

Lexer::Lexer(const llvm::SourceMgr &sourceMgr) : sourceMgr(sourceMgr) {
    auto bufferID = sourceMgr.getMainFileID();
    curBuffer = sourceMgr.getMemoryBuffer(bufferID)->getBuffer();
    curPtr = curBuffer.begin();
}

/// emitError - Emit an error message and return an Token::error token.
Token Lexer::emitError(const char *loc, const llvm::Twine &message) {
//    TODO: Emit error
//    tinyswift::emitError(getSourceLocation(llvm::SMLoc::getFromPointer(loc)),
//                    message);
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

//===----------------------------------------------------------------------===//
// UTF8 Validation/Encoding/Decoding helper functions
//===----------------------------------------------------------------------===//

/// EncodeToUTF8 - Encode the specified code point into a UTF8 stream.  Return
/// true if it is an erroneous code point.
static bool EncodeToUTF8(unsigned CharValue,
                         llvm::SmallVectorImpl<char> &Result) {
    assert(CharValue >= 0x80 && "Single-byte encoding should be already handled");
    // Number of bits in the value, ignoring leading zeros.
    unsigned NumBits = 32 - llvm::countLeadingZeros(CharValue);

    // Handle the leading byte, based on the number of bits in the value.
    unsigned NumTrailingBytes;
    if (NumBits <= 5 + 6) {
        // Encoding is 0x110aaaaa 10bbbbbb
        Result.push_back(char(0xC0 | (CharValue >> 6)));
        NumTrailingBytes = 1;
    } else if (NumBits <= 4 + 6 + 6) {
        // Encoding is 0x1110aaaa 10bbbbbb 10cccccc
        Result.push_back(char(0xE0 | (CharValue >> (6 + 6))));
        NumTrailingBytes = 2;

        // UTF-16 surrogate pair values are not valid code points.
        if (CharValue >= 0xD800 && CharValue <= 0xDFFF)
            return true;
        // U+FDD0...U+FDEF are also reserved
        if (CharValue >= 0xFDD0 && CharValue <= 0xFDEF)
            return true;
    } else if (NumBits <= 3 + 6 + 6 + 6) {
        // Encoding is 0x11110aaa 10bbbbbb 10cccccc 10dddddd
        Result.push_back(char(0xF0 | (CharValue >> (6 + 6 + 6))));
        NumTrailingBytes = 3;
        // Reject over-large code points.  These cannot be encoded as UTF-16
        // surrogate pairs, so UTF-32 doesn't allow them.
        if (CharValue > 0x10FFFF)
            return true;
    } else {
        return true;  // UTF8 can encode these, but they aren't valid code points.
    }

    // Emit all of the trailing bytes.
    while (NumTrailingBytes--)
        Result.push_back(char(0x80 | (0x3F & (CharValue >> (NumTrailingBytes * 6)))));
    return false;
}


/// CLO8 - Return the number of leading ones in the specified 8-bit value.
static unsigned CLO8(unsigned char C) {
    return llvm::countLeadingOnes(uint32_t(C) << 24);
}

/// isStartOfUTF8Character - Return true if this isn't a UTF8 continuation
/// character, which will be of the form 0b10XXXXXX
static bool isStartOfUTF8Character(unsigned char C) {
    // RFC 2279: The octet values FE and FF never appear.
    // RFC 3629: The octet values C0, C1, F5 to FF never appear.
    return C <= 0x80 || (C >= 0xC2 && C < 0xF5);
}

/// validateUTF8CharacterAndAdvance - Given a pointer to the starting byte of a
/// UTF8 character, validate it and advance the lexer past it.  This returns the
/// encoded character or ~0U if the encoding is invalid.
static uint32_t validateUTF8CharacterAndAdvance(const char *&Ptr,
                                                const char *End) {
    if (Ptr >= End)
        return ~0U;

    unsigned char CurByte = *Ptr++;
    if (CurByte < 0x80)
        return CurByte;

    // Read the number of high bits set, which indicates the number of bytes in
    // the character.
    unsigned EncodedBytes = CLO8(CurByte);

    // If this is 0b10XXXXXX, then it is a continuation character.
    if (EncodedBytes == 1 ||
        !isStartOfUTF8Character(CurByte)) {
        // Skip until we get the start of another character.  This is guaranteed to
        // at least stop at the nul at the end of the buffer.
        while (Ptr < End && !isStartOfUTF8Character(*Ptr))
            ++Ptr;
        return ~0U;
    }

    // Drop the high bits indicating the # bytes of the result.
    unsigned CharValue = (unsigned char) (CurByte << EncodedBytes) >> EncodedBytes;

    // Read and validate the continuation bytes.
    for (unsigned i = 1; i != EncodedBytes; ++i) {
        if (Ptr >= End)
            return ~0U;
        CurByte = *Ptr;
        // If the high bit isn't set or the second bit isn't clear, then this is not
        // a continuation byte!
        if (CurByte < 0x80 || CurByte >= 0xC0) return ~0U;

        // Accumulate our result.
        CharValue <<= 6;
        CharValue |= CurByte & 0x3F;
        ++Ptr;
    }

    // UTF-16 surrogate pair values are not valid code points.
    if (CharValue >= 0xD800 && CharValue <= 0xDFFF)
        return ~0U;

    // If we got here, we read the appropriate number of accumulated bytes.
    // Verify that the encoding was actually minimal.
    // Number of bits in the value, ignoring leading zeros.
    unsigned NumBits = 32 - llvm::countLeadingZeros(CharValue);

    if (NumBits <= 5 + 6)
        return EncodedBytes == 2 ? CharValue : ~0U;
    if (NumBits <= 4 + 6 + 6)
        return EncodedBytes == 3 ? CharValue : ~0U;
    return EncodedBytes == 4 ? CharValue : ~0U;
}

/// skipToEndOfInterpolatedExpression - Given the first character after a \(
/// sequence in a string literal (the start of an interpolated expression),
/// scan forward to the end of the interpolated expression and return the end.
/// On success, the returned pointer will point to the ')' at the end of the
/// interpolated expression.  On failure, it will point to the first character
/// that cannot be lexed as part of the interpolated expression; this character
/// will never be ')'.
///
/// This function performs brace and quote matching, keeping a stack of
/// outstanding delimiters as it scans the string.
static const char *skipToEndOfInterpolatedExpression(const char *CurPtr, const char *EndPtr) {
    llvm::SmallVector<char, 4> OpenDelimiters;
    auto inStringLiteral = [&]() {
        return !OpenDelimiters.empty() &&
               (OpenDelimiters.back() == '"' || OpenDelimiters.back() == '\'');
    };

    while (true) {
        // This is a simple scanner, capable of recognizing nested parentheses and
        // string literals but not much else.  The implications of this include not
        // being able to break an expression over multiple lines in an interpolated
        // string.  This limitation allows us to recover from common errors though.
        //
        // On success scanning the expression body, the real lexer will be used to
        // relex the body when parsing the expressions.  We let it diagnose any
        // issues with malformed tokens or other problems.
        switch (*CurPtr++) {
            // String literals in general cannot be split across multiple lines;
            // interpolated ones are no exception.
            case '\n':
            case '\r':
                // Will be diagnosed as an unterminated string literal.
                return CurPtr - 1;

            case '"':
            case '\'':
                if (inStringLiteral()) {
                    // Is it the closing quote?
                    if (OpenDelimiters.back() == CurPtr[-1]) {
                        OpenDelimiters.pop_back();
                    }
                    // Otherwise it's an ordinary character; treat it normally.
                } else {
                    OpenDelimiters.push_back(CurPtr[-1]);
                }
                continue;
            case '\\':
                if (inStringLiteral()) {
                    char escapedChar = *CurPtr++;
                    switch (escapedChar) {
                        case '(':
                            // Entering a recursive interpolated expression
                            OpenDelimiters.push_back('(');
                            continue;
                        case '\n':
                        case '\r':
                        case 0:
                            // Don't jump over newline/EOF due to preceding backslash!
                            return CurPtr - 1;
                        default:
                            continue;
                    }
                }
                continue;
            case 0:
                // If we hit EOF, we fail.
                if (CurPtr - 1 == EndPtr) {
                    return CurPtr - 1;
                }
                continue;

                // Paren nesting deeper to support "foo = \((a+b)-(c*d)) bar".
            case '(':
                if (!inStringLiteral()) {
                    OpenDelimiters.push_back('(');
                }
                continue;
            case ')':
                if (OpenDelimiters.empty()) {
                    // No outstanding open delimiters; we're done.
                    return CurPtr - 1;
                } else if (OpenDelimiters.back() == '(') {
                    // Pop the matching bracket and keep going.
                    OpenDelimiters.pop_back();
                    continue;
                } else {
                    // It's a right parenthesis in a string literal.
                    assert(inStringLiteral());
                    continue;
                }
            default:
                // Normal token character.
                continue;
        }
    }

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
//        if (Diags)
//            Diags->diagnose(CurPtr, diag::lex_invalid_u_escape_rbrace);
        return ~1U;
    }
    ++CurPtr;

    if (NumDigits < 1 || NumDigits > 8) {
//        if (Diags)
//            Diags->diagnose(CurPtr, diag::lex_invalid_u_escape);
        return ~1U;
    }

    unsigned CharValue = 0;
    llvm::StringRef(DigitStart, NumDigits).getAsInteger(16, CharValue);
    return CharValue;
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
//                if (isPrintable(CurPtr[-1]) == 0)
//                    if (EmitDiagnostics)
//                        diagnose(CharStart, diag::lex_unprintable_ascii_character);
                return CurPtr[-1];
            }
            --CurPtr;
            unsigned CharValue = validateUTF8CharacterAndAdvance(CurPtr, getBufferEnd());
            if (CharValue != ~0U) return CharValue;
//            if (EmitDiagnostics)
//                diagnose(CharStart, diag::lex_invalid_utf8);
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
//                if (EmitDiagnostics)
//                    diagnose(CurPtr-1, diag::lex_nul_character);
                return CurPtr[-1];
            }
            // Move the pointer back to EOF.
            --CurPtr;
            LLVM_FALLTHROUGH;
        case '\n':  // String literals cannot have \n or \r in them.
        case '\r':
//            if (EmitDiagnostics)
//                diagnose(CurPtr-1, diag::lex_unterminated_string);
            return ~1U;
        case '\\':  // Escapes.
            break;
    }

    unsigned CharValue = 0;
    // Escape processing.  We already ate the "\".
    switch (*CurPtr) {
        default:  // Invalid escape.
//            if (EmitDiagnostics)
//                diagnose(CurPtr, diag::lex_invalid_escape);
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
//                if (EmitDiagnostics)
//                    diagnose(CurPtr - 1, diag::lex_unicode_escape_braces);
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
//        if (EmitDiagnostics)
//            diagnose(CharStart, diag::lex_invalid_unicode_scalar);
        return ~1U;
    }

    return CharValue;
}


/// Lex a string literal.
///
///   string_literal ::= ["]([^"\\\n\r]|character_escape)*["]
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