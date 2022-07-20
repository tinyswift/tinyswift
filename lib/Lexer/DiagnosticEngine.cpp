//
// Created by Satish on 20/07/22.
//

#include "tinyswift/AST/DiagnosticEngine.h"
#include "tinyswift/AST/DiagnosticsCommon.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace tinyswift;


static CharSourceRange toCharSourceRange(SourceManager &SM, SourceLoc Start,
                                         SourceLoc End) {
    return CharSourceRange(SM, Start, End);
}

InFlightDiagnostic &InFlightDiagnostic::fixItReplaceChars(SourceLoc Start,
                                                          SourceLoc End,
                                                          llvm::StringRef Str) {
    assert(IsActive && "Cannot modify an inactive diagnostic");
    if (Engine && Start.isValid())
        Engine->getActiveDiagnostic().addFixIt(Diagnostic::FixIt(
                toCharSourceRange(Engine->SourceMgr, Start, End), Str));
    return *this;
}


void InFlightDiagnostic::flush() {
    if (!IsActive)
        return;

    IsActive = false;
    if (Engine)
        Engine->flushActiveDiagnostic();
}

void DiagnosticEngine::flushActiveDiagnostic() {
    assert(ActiveDiagnostic && "No active diagnostic to flush");
    if (TransactionCount == 0) {
        emitDiagnostic(*ActiveDiagnostic);
    } else {
        TentativeDiagnostics.emplace_back(std::move(*ActiveDiagnostic));
    }
    ActiveDiagnostic.reset();
}

void DiagnosticEngine::emitDiagnostic(const Diagnostic &diagnostic) {


    return;
}

enum class tinyswift::DiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) ID,

#include "tinyswift/AST/DiagnosticsAll.def"
};

// Define all of the diagnostic objects and initialize them with their
// diagnostic IDs.
namespace tinyswift {
    namespace diag {
#define DIAG(KIND, ID, Options, Text, Signature) \
    detail::DiagWithArguments<void Signature>::type ID = { DiagID::ID };

#include "tinyswift/AST/DiagnosticsAll.def"

    } // end namespace diag
} // end namespace swift
