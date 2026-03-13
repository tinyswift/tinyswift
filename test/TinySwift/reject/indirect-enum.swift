// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

indirect enum Tree { // expected-error {{indirect enums are not supported in TinySwift; use an explicit Box<T>}}
    case leaf
    case node(Tree, Tree)
}

enum List {
    case end
    indirect case cons(Builtin.Int64, List) // expected-error {{indirect enums are not supported in TinySwift; use an explicit Box<T>}}
}
