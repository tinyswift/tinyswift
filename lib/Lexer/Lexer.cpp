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

/// Lex a string literal.
///
///   string-literal ::= '"' [^"\n\f\v\r]* '"'
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

            default:
                continue;
        }
    }
}
