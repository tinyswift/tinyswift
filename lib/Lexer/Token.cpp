//
// Created by Satish on 16/07/22.
//

#include "tinyswift/Lexer//Token.h"
#include "llvm/ADT/StringExtras.h"

using namespace tinyswift;

llvm::SMLoc Token::getLoc() const {
    return llvm::SMLoc::getFromPointer(spelling.data());
}

llvm::SMLoc Token::getEndLoc() const {
    return llvm::SMLoc::getFromPointer(spelling.data() + spelling.size());
}

llvm::SMRange Token::getLocRange() const {
    return llvm::SMRange(getLoc(), getEndLoc());
}

/// Given a punctuation or keyword token kind, return the spelling of the
/// token as a string.  Warning: This will abort on markers, identifiers and
/// literal tokens since they have no fixed spelling.
llvm::StringRef Token::getTokenSpelling() {
    switch (kind) {
        case Kind::unknown:
            return "unknown";
        case Kind::eof:
            return "eof";
        case Kind::error:
            return "error";
        case Kind::integer_literal:
            return "integer_literal";
        case Kind::floating_literal:
            return "floating_literal";

        case Kind::string_literal:
            return "string_literal";

#define KEYWORD(X) case kw_ ## X: return #X;
#define PUNCTUATOR(X, Y) case X: return Y;

#include "tinyswift/Lexer/TokenKinds.def"

        default :
            llvm_unreachable("unexpected token kind");
    }
}