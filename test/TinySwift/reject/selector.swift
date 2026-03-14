// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Note: #selector requires ObjC interop which is disabled in TinySwift.
// The parser rejects #selector when -disable-objc-interop is set, so
// the Sema diagnostic is defense-in-depth for cases where ObjC is available.

struct PlainStruct {}
