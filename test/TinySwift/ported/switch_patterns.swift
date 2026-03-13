// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift
// Ported from: test/stmt/switch_stmt1.swift — enum switch exhaustiveness

enum Color {
    case red
    case green
    case blue
}

func describeColor(_ c: Color, _ r: Builtin.Int1, _ g: Builtin.Int1, _ b: Builtin.Int1) -> Builtin.Int1 {
    switch c {
    case .red:
        return r
    case .green:
        return g
    case .blue:
        return b
    }
}

enum Result<T, E> {
    case success(T)
    case failure(E)
}

struct ErrorInfo {
    var code: Builtin.Int32
}

func handleResult<T>(_ r: Result<T, ErrorInfo>, _ yes: Builtin.Int1, _ no: Builtin.Int1) -> Builtin.Int1 {
    switch r {
    case .success:
        return yes
    case .failure:
        return no
    }
}

func extractValue<T, E>(_ r: Result<T, E>, _ fallback: T) -> T {
    switch r {
    case .success(let val):
        return val
    case .failure:
        return fallback
    }
}

enum Weekday {
    case mon, tue, wed, thu, fri, sat, sun
}

func isWeekend(_ d: Weekday, _ yes: Builtin.Int1, _ no: Builtin.Int1) -> Builtin.Int1 {
    switch d {
    case .sat, .sun:
        return yes
    case .mon, .tue, .wed, .thu, .fri:
        return no
    }
}
