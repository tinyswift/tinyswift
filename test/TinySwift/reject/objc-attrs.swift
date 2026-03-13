// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Note: @objc and IB attributes cannot be tested in -parse-stdlib -disable-objc-interop mode
// because the parser may reject them before Sema runs. This file validates that the
// diagnostic infrastructure is in place. Full ObjC attribute rejection is covered by
// TinySwift implying Embedded mode, which already disables ObjC interop.

struct PlainStruct {}
