// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

enum MyError {
    case failed
}

func canFail() throws(MyError) -> Builtin.Int64 {
    return Builtin.zeroInitializer()
}

func testTryOptional() {
    let _ = try? canFail() // expected-error {{'try?' and 'try!' are not supported in TinySwift; use typed throws}}
}

func testTryForce() {
    let _ = try! canFail() // expected-error {{'try?' and 'try!' are not supported in TinySwift; use typed throws}}
}
