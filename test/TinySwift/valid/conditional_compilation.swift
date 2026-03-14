// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Test that #if tinyswift is available as a compilation condition.

#if tinyswift
struct TinySwiftOnly {
    var value: Builtin.Int64
}
#else
// This should not be compiled.
struct ShouldNotExist {
    var invalid: DOES_NOT_EXIST
}
#endif

func useTinySwiftOnly() -> TinySwiftOnly {
    return TinySwiftOnly(value: Builtin.zeroInitializer())
}
