//
// Created by Satish on 20/07/22.
//


#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "tinyswift/Lexer/DiagnosticConsumer.h"

using namespace tinyswift;

DiagnosticConsumer::~DiagnosticConsumer() {}

void NullDiagnosticConsumer::handleDiagnostic(SourceManager &SM,
                                              SourceLoc Loc,
                                              DiagnosticKind Kind,
                                              llvm::StringRef Text,
                                              const DiagnosticInfo &Info) {
    llvm::dbgs() << "NullDiagnosticConsumer received diagnostic: "
                 << Text << "\n";
}