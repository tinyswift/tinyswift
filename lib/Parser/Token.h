//
// Created by Satish on 16/07/22.
//

#ifndef TINYSWIFT_TOKEN_H
#define TINYSWIFT_TOKEN_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace tinyswift {
    class Token {

    public:
        enum Kind {
            unknown = 0,
            eof,
            code_complete,
            identifier,
            oper_binary_unspaced,   // "x+y"
            oper_binary_spaced,     // "x + y"
            oper_postfix,
            oper_prefix,
            dollarident,
            integer_literal,
            floating_literal,
            string_literal,
            sil_local_name,      // %42 in SIL mode.
            comment,

#define KEYWORD(X) kw_ ## X,
#define PUNCTUATOR(X, Y) X,
#include "TokenKinds.def"

            NUM_TOKENS
        };

        Token(Kind kind, llvm::StringRef spelling) : kind(kind), spelling(spelling) {}
    private:
        /// Discriminator that indicates the sort of token this is.
        Kind kind;

        /// A reference to the entire token contents; this is always a pointer into
        /// a memory buffer owned by the source manager.
        llvm::StringRef spelling;
    };
}

#endif //TINYSWIFT_TOKEN_H
