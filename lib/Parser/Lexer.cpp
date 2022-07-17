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
        }
    }
}