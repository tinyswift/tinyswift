//
// Created by Satish on 17/07/22.
//

#ifndef TINYSWIFT_STRINGHELPER_H
#define TINYSWIFT_STRINGHELPER_H

#include <llvm/ADT/StringExtras.h>

namespace tinyswift {
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
}

#endif //TINYSWIFT_STRINGHELPER_H
