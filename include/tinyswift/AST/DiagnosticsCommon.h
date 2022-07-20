//
// Created by Satish on 20/07/22.
//

#ifndef TINYSWIFT_DIAGNOSTICSCOMMON_H
#define TINYSWIFT_DIAGNOSTICSCOMMON_H

#include "DiagnosticEngine.h"


namespace tinyswift {
    template<typename ...ArgTypes>
    struct Diag;

    namespace detail {
        template<typename T>
        struct DiagWithArguments;

        template<typename ...ArgTypes>
        struct DiagWithArguments<void(ArgTypes...)> {
            typedef Diag<ArgTypes...> type;
        };
    }

    enum class StaticSpellingKind : uint8_t;

    namespace diag {

        enum class RequirementKind : uint8_t;

//        using DeclAttribute = const DeclAttribute *;

        // Declare common diagnostics objects with their appropriate types.
#define DIAG(KIND, ID, Options, Text, Signature) \
    extern detail::DiagWithArguments<void Signature>::type ID;

#include "DiagnosticsCommon.def"

    }
}


#endif //TINYSWIFT_DIAGNOSTICSCOMMON_H
