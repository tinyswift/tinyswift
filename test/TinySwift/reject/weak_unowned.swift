// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

struct Ref {}

struct Container {
    weak var a: Ref     // expected-error {{weak references are not supported in TinySwift}} expected-error {{attribute 'weak' cannot be used in embedded Swift}} expected-error {{'weak' variable should have optional type 'Ref?'}} expected-error {{'weak' may only be applied to class and class-bound protocol types, not 'Ref'}}
    unowned var b: Ref  // expected-error {{unowned references are not supported in TinySwift}} expected-error {{attribute 'unowned' cannot be used in embedded Swift}} expected-error {{'unowned' may only be applied to class and class-bound protocol types, not 'Ref'}}
}
