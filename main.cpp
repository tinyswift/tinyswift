#include <iostream>
#include "tinyswift/Parser/Lexer.h"
#include "llvm/Support/raw_ostream.h"

int main() {
    std::cout << "tinyswift!" << std::endl;

    llvm::StringRef Buffer = "var x = 1; func main() { return 1; }";
    llvm::SourceMgr sourceMgr = llvm::SourceMgr();
    std::unique_ptr<llvm::MemoryBuffer> memBuffer = llvm::MemoryBuffer::getMemBuffer(Buffer);
    sourceMgr.AddNewSourceBuffer(std::move(memBuffer), llvm::SMLoc());

    tinyswift::Lexer lexer(sourceMgr);
    tinyswift::Token token = lexer.lexToken();

    llvm::raw_ostream &output = llvm::outs();
    while (token.isNot(tinyswift::Token::eof)) {
        output << token.getKind() << "\n";
        token = lexer.lexToken();
    }

    output << token.getKind() << "\n";
    output << "Done\n";

    return 0;
}
