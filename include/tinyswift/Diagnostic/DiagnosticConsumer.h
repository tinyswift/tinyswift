//
// Created by Satish on 20/07/22.
//

#ifndef TINYSWIFT_DIAGNOSTICCONSUMER_H
#define TINYSWIFT_DIAGNOSTICCONSUMER_H

#include "tinyswift/Lexer/SourceLoc.h"
#include "llvm/Support/SourceMgr.h"

namespace tinyswift {
    class SourceManager;

    enum class DiagID : uint32_t;

/// \brief Describes the kind of diagnostic.
///
    enum class DiagnosticKind : uint8_t {
        Error,
        Warning,
        Note
    };

/// \brief Extra information carried along with a diagnostic, which may or
/// may not be of interest to a given diagnostic consumer.
    struct DiagnosticInfo {
        DiagID ID = DiagID(0);

        /// Represents a fix-it, a replacement of one range of text with another.
        class FixIt {
            CharSourceRange Range;
            std::string Text;

        public:
            FixIt(CharSourceRange R, llvm::StringRef Str)
                    : Range(R), Text(Str) {}

            CharSourceRange getRange() const { return Range; }

            llvm::StringRef getText() const { return Text; }
        };

        /// \brief Extra source ranges that are attached to the diagnostic.
        llvm::ArrayRef<CharSourceRange> Ranges;

        /// \brief Extra source ranges that are attached to the diagnostic.
        llvm::ArrayRef<FixIt> FixIts;
    };

/// \brief Abstract interface for classes that present diagnostics to the user.
    class DiagnosticConsumer {
    protected:
        static llvm::SMLoc getRawLoc(SourceLoc Loc);

        static llvm::SMRange getRawRange(SourceManager &SM, CharSourceRange R) {
            return llvm::SMRange(getRawLoc(R.getStart()), getRawLoc(R.getEnd()));
        }

        static llvm::SMFixIt getRawFixIt(SourceManager &SM, DiagnosticInfo::FixIt F) {
            // FIXME: It's unfortunate that we have to copy the replacement text.
            return llvm::SMFixIt(getRawRange(SM, F.getRange()), F.getText());
        }

    public:
        virtual ~DiagnosticConsumer();

        /// \brief Invoked whenever the frontend emits a diagnostic.
        ///
        /// \param SM The source manager associated with the source locations in
        /// this diagnostic.
        ///
        /// \param Loc The source location associated with this diagnostic. This
        /// location may be invalid, if the diagnostic is not directly related to
        /// the source (e.g., if it comes from command-line parsing).
        ///
        /// \param Kind The severity of the diagnostic (error, warning, note).
        ///
        /// \param Text The diagnostic text.
        ///
        /// \param Info Extra information associated with the diagnostic.
        virtual void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                                      DiagnosticKind Kind, llvm::StringRef Text,
                                      const DiagnosticInfo &Info) = 0;
    };

/// \brief DiagnosticConsumer that discards all diagnostics.
    class NullDiagnosticConsumer : public DiagnosticConsumer {
    public:
        void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                              DiagnosticKind Kind, llvm::StringRef Text,
                              const DiagnosticInfo &Info) override;
    };

} // end namespace swift


#endif //TINYSWIFT_DIAGNOSTICCONSUMER_H
