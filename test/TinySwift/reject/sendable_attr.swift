// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

@Sendable func doWork() {} // expected-error {{'@Sendable' is not supported in TinySwift; no concurrency}}
