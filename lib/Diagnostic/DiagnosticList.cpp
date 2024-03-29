//===--- DiagnosticList.cpp - Diagnostic Definitions ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines all of the diagnostics emitted by Swift.
//
//===----------------------------------------------------------------------===//

#include "tinyswift/Diagnostic/DiagnosticsCommon.h"
using namespace tinyswift;

enum class tinyswift::DiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) ID,
#include "tinyswift/Diagnostic/DiagnosticsAll.def"
};

// Define all of the diagnostic objects and initialize them with their
// diagnostic IDs.
namespace tinyswift {
namespace diag {
#define DIAG(KIND, ID, Options, Text, Signature)                               \
  detail::DiagWithArguments<void Signature>::type ID = {DiagID::ID};
#include "tinyswift/Diagnostic/DiagnosticsAll.def"
} // end namespace diag
} // namespace tinyswift
