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

        /// Constructor for Lexer
        explicit Lexer(const llvm::SourceMgr &sourceMgr);

        const llvm::SourceMgr &getSourceMgr() { return sourceMgr; }

        /// Lex the next token
        Token lexToken();

        /// Return the code completion location of the lexer, or nullptr if there is
        /// none.
        const char *getCodeCompleteLoc() const { return codeCompleteLoc; }

        /// Returns the start of the buffer.
        const char *getBufferBegin() { return curBuffer.data(); }

        /// Returns the end of the buffer.
        const char *getBufferEnd() { return curBuffer.data() + curBuffer.size(); }

    private:

        /// SourceManager for the source file.
        const llvm::SourceMgr &sourceMgr;

        /// Current buffer. This is the buffer that is being lexed.
        llvm::StringRef curBuffer;

        /// Current position in the buffer.
        const char *curPtr;

        /// Skip a comment line, starting with a '//'.
        void skipComment();

        /// Initialize Token from kind and spelling.
        Token formToken(Token::Kind kind, const char *tokStart) {
            return Token(kind, llvm::StringRef(tokStart, curPtr - tokStart));
        }

        /// Emit error message.
        Token emitError(const char *loc, const llvm::Twine &message);

        /// An optional code completion point within the input file, used to indicate
        /// the position of a code completion token.
        const char *codeCompleteLoc;

        Lexer(const Lexer &) = delete;

        void operator=(const Lexer &) = delete;

        Token lexNumber(const char *tokStart);

        Token lexString(const char *tokStart);

        unsigned int lexCharacter(const char *&CurPtr, char StopQuote);

        unsigned int lexUnicodeEscape(const char *&CurPtr);
    };

}

#endif //TINYSWIFT_LEXER_H
