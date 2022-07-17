#include <iostream>
#include "tinyswift/Parser/Lexer.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

namespace cl = llvm::cl;

static cl::opt<std::string> inputFilename(cl::Positional,
                                          cl::desc("<input swift file>"),
                                          cl::init(""),
                                          cl::value_desc("filename"));

int main(int argc, char **argv) {
    cl::ParseCommandLineOptions(argc, argv, "tinyswift\n");
    // llvm::StringRef Buffer = "var x = 1; func main() { return 1; }";

    // if no input file is given, throw an error
    if (inputFilename == "") {
        std::cerr << "No input file given.\n";
        return 1;
    }

    // print the input file name
    std::cout << "Input file: " << inputFilename << "\n";

    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrErr =
            llvm::MemoryBuffer::getFile(inputFilename);
    if (std::error_code ec = fileOrErr.getError()) {
        llvm::errs() << "Could not open input file: " << ec.message() << "\n";
        return 1;
    }
    llvm::StringRef Buffer = fileOrErr.get()->getBuffer();

    llvm::SourceMgr sourceMgr = llvm::SourceMgr();
    std::unique_ptr<llvm::MemoryBuffer> memBuffer = llvm::MemoryBuffer::getMemBuffer(Buffer);
    sourceMgr.AddNewSourceBuffer(std::move(memBuffer), llvm::SMLoc());

    tinyswift::Lexer lexer(sourceMgr);
    tinyswift::Token token = lexer.lexToken();

    llvm::raw_ostream &output = llvm::outs();
    while (token.isNot(tinyswift::Token::eof)) {
        output << token.getTokenSpelling() + " " + token.getSpelling() << "\n";
        token = lexer.lexToken();
    }

    output << token.getTokenSpelling() + " " + token.getSpelling() << "\n";
    output << "Done\n";

    return 0;
}
