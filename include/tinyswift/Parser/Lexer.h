//
// Created by Satish on 16/07/22.
//

#ifndef TINYSWIFT_LEXER_H
#define TINYSWIFT_LEXER_H

#include "Token.h"
#include "llvm/Support/SourceMgr.h"

namespace tinyswift {

    class Lexer {

    public:
        explicit Lexer(const llvm::SourceMgr &sourceMgr);

        const llvm::SourceMgr &getSourceMgr() { return sourceMgr; }

        Token lexToken();

        /// Change the position of the lexer cursor.  The next token we lex will start
        /// at the designated point in the input.
        void resetPointer(const char *newPointer) { curPtr = newPointer; }

        /// Returns the start of the buffer.
        const char *getBufferBegin() { return curBuffer.data(); }

    private:
        // Helpers.
        Token formToken(Token::Kind kind, const char *tokStart) {
            return Token(kind, llvm::StringRef(tokStart, curPtr - tokStart));
        }

        Token emitError(const char *loc, const llvm::Twine &message);

        /// Skip a comment line, starting with a '//'.
        void skipComment();

        const llvm::SourceMgr &sourceMgr;

        llvm::StringRef curBuffer;
        const char *curPtr;

        Lexer(const Lexer &) = delete;

        void operator=(const Lexer &) = delete;
    };

}

#endif //TINYSWIFT_LEXER_H
