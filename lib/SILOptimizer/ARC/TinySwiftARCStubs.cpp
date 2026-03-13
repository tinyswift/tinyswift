//===--- TinySwiftARCStubs.cpp - Stub ARC passes for TinySwift ------------===//
//
// Provides stub implementations of ARC pass factory functions when
// TINYSWIFT_BUILD is enabled. The actual ARC source files are excluded
// from compilation, but PassManager still references these factories.
//
//===----------------------------------------------------------------------===//

#ifdef TINYSWIFT

#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {
class StubArcPass : public SILFunctionTransform {
  void run() override {
    // Never runs — ARC passes are skipped in TinySwift pipeline.
  }
};
} // end anonymous namespace

SILTransform *swift::createARCSequenceOpts() { return new StubArcPass(); }
SILTransform *swift::createARCLoopOpts() { return new StubArcPass(); }

#endif // TINYSWIFT
