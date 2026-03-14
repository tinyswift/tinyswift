// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

@dynamicCallable // expected-error {{'@dynamicCallable' is not supported in TinySwift}}
struct Callable {
    func dynamicallyCall(withArguments args: Builtin.Int64) -> Builtin.Int64 {
        return args
    }
}
