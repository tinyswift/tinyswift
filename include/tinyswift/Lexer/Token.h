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
            error,
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

        // Token classification.
        Kind getKind() const { return kind; }

        bool is(Kind k) const { return kind == k; }

        bool isAny(Kind k1, Kind k2) const { return is(k1) || is(k2); }

        /// Return true if this token is one of the specified kinds.
        template<typename... T>
        bool isAny(Kind k1, Kind k2, Kind k3, T... others) const {
            if (is(k1))
                return true;
            return isAny(k2, k3, others...);
        }

        bool isNot(Kind k) const { return kind != k; }

        /// Return true if this token isn't one of the specified kinds.
        template<typename... T>
        bool isNot(Kind k1, Kind k2, T... others) const {
            return !isAny(k1, k2, others...);
        }

        // Location processing.
        llvm::SMLoc getLoc() const;

        llvm::SMLoc getEndLoc() const;

        llvm::SMRange getLocRange() const;

        /// Given a punctuation or keyword token kind, return the spelling of the
        /// token as a string.  Warning: This will abort on markers, identifiers and
        /// literal tokens since they have no fixed spelling.
//        static llvm::StringRef getTokenSpelling(Kind kind);

        /// Return the spelling of this token.
        llvm::StringRef getSpelling() const { return spelling; };

        llvm::StringRef getTokenSpelling();

    private :
        /// Discriminator that indicates the sort of token this is.
        Kind kind;

        /// A reference to the entire token contents; this is always a pointer into
        /// a memory buffer owned by the source manager.
        llvm::StringRef spelling;
    };
}

#endif //TINYSWIFT_TOKEN_H
