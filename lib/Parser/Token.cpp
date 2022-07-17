//
// Created by Satish on 16/07/22.
//

#include "tinyswift/Parser/Token.h"
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