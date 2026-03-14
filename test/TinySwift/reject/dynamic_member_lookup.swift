// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

@dynamicMemberLookup // expected-error {{'@dynamicMemberLookup' is not supported in TinySwift}}
struct DynLookup {
    subscript(dynamicMember name: Builtin.Int64) -> Builtin.Int64 {
        return name
    }
}
