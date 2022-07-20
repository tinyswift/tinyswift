//
// Created by Satish on 20/07/22.
//

#ifndef TINYSWIFT_DIAGNOSTICSPARSE_H
#define TINYSWIFT_DIAGNOSTICSPARSE_H

#include "DiagnosticsCommon.h"

namespace tinyswift {
namespace diag {
// Declare common diagnostics objects with their appropriate types.
#define DIAG(KIND, ID, Options, Text, Signature)                               \
  extern detail::DiagWithArguments<void Signature>::type ID;

#include "DiagnosticsParse.def"

} // namespace diag
} // namespace tinyswift

#endif // TINYSWIFT_DIAGNOSTICSPARSE_H
