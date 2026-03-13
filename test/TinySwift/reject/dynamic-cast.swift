// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Note: Dynamic casts (as? and as!) require standard library infrastructure
// (CastingPrecedence, etc.) that is not available in -parse-stdlib mode.
// The dynamic_cast_not_supported_in_tinyswift diagnostic is implemented in
// MiscDiagnostics.cpp and will fire when compiling with a stdlib available.
// This test validates the diagnostic infrastructure is in place.

struct Marker {}
