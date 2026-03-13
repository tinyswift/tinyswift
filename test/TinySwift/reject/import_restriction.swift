// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s 2>&1 | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Foundation import should be rejected in TinySwift mode.
import Foundation
// CHECK: error: no such module 'Foundation'
