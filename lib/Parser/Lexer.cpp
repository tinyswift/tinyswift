//
// Created by Satish on 16/07/22.
//

#include "tinyswift/Parser/Lexer.h"

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
