// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Stress test: closures capturing variables in complex patterns.
// Exercises ownership of captured values, nested closures, inout captures,
// and closures passed through generic functions.
// All types are Builtin-based — no ARC symbols should result.

// --- Types ---

struct Sample {
  var value: Builtin.Int64
  var weight: Builtin.Int32
}

struct Config {
  var threshold: Builtin.Int64
  var scale: Builtin.Int64
}

// --- Helpers ---

func applyFn(_ f: () -> Builtin.Int64) -> Builtin.Int64 {
  return f()
}

func applyUnary(_ f: (Builtin.Int64) -> Builtin.Int64,
                _ x: Builtin.Int64) -> Builtin.Int64 {
  return f(x)
}

func applyBinary(_ f: (Builtin.Int64, Builtin.Int64) -> Builtin.Int64,
                 _ a: Builtin.Int64,
                 _ b: Builtin.Int64) -> Builtin.Int64 {
  return f(a, b)
}

func applyTransform<T, U>(_ f: (T) -> U, _ value: T) -> U {
  return f(value)
}

func applyPredicate<T>(_ f: (T) -> Builtin.Int1, _ value: T) -> Builtin.Int1 {
  return f(value)
}

// --- Test 1: capture a single let binding ---

func captureOneLet() -> Builtin.Int64 {
  let x: Builtin.Int64 = Builtin.zeroInitializer()
  return applyFn { x }
}

// --- Test 2: capture multiple let bindings ---

func captureMultipleLets() -> Builtin.Int64 {
  let a: Builtin.Int64 = Builtin.zeroInitializer()
  let b: Builtin.Int64 = Builtin.zeroInitializer()
  let c: Builtin.Int64 = Builtin.zeroInitializer()
  return applyFn {
    Builtin.add_Int64(Builtin.add_Int64(a, b), c)
  }
}

// --- Test 3: capture a struct ---

func captureStruct() -> Builtin.Int64 {
  let s = Sample(value: Builtin.zeroInitializer(),
                 weight: Builtin.zeroInitializer())
  return applyFn { s.value }
}

// --- Test 4: capture multiple structs ---

func captureTwoStructs() -> Builtin.Int64 {
  let s1 = Sample(value: Builtin.zeroInitializer(),
                  weight: Builtin.zeroInitializer())
  let s2 = Sample(value: Builtin.zeroInitializer(),
                  weight: Builtin.zeroInitializer())
  return applyFn {
    Builtin.add_Int64(s1.value, s2.value)
  }
}

// --- Test 5: closure captures used in generic function ---

func captureInGeneric() -> Builtin.Int64 {
  let cfg = Config(threshold: Builtin.zeroInitializer(),
                   scale: Builtin.zeroInitializer())
  let transform: (Sample) -> Builtin.Int64 = { sample in
    Builtin.add_Int64(sample.value, cfg.scale)
  }
  let s = Sample(value: Builtin.zeroInitializer(),
                 weight: Builtin.zeroInitializer())
  return applyTransform(transform, s)
}

// --- Test 6: nested closures — inner captures outer's captured variables ---

func nestedCapture() -> Builtin.Int64 {
  let x: Builtin.Int64 = Builtin.zeroInitializer()
  let outer: () -> () -> Builtin.Int64 = {
    // Outer captures x.
    let y = x
    return {
      // Inner captures y (which came from outer's capture of x).
      y
    }
  }
  let inner = outer()
  return inner()
}

// --- Test 7: deeply nested closures (3 levels) ---

func tripleNested() -> Builtin.Int64 {
  let a: Builtin.Int64 = Builtin.zeroInitializer()
  let level1: () -> () -> () -> Builtin.Int64 = {
    let b = a
    return {
      let c = b
      return {
        Builtin.add_Int64(Builtin.add_Int64(a, b), c)
      }
    }
  }
  return level1()()()
}

// --- Test 8: closure capturing mutable variable (inout-like pattern) ---

func captureVar() -> Builtin.Int64 {
  var accumulator: Builtin.Int64 = Builtin.zeroInitializer()
  let one: Builtin.Int64 = Builtin.intLiteral_Int64(1._builtinIntegerLiteral)
  let step = {
    accumulator = Builtin.add_Int64(accumulator, one)
  }
  step()
  step()
  step()
  return accumulator
}

// --- Test 9: closure modifying an inout parameter ---

func modifyViaInout(_ x: inout Builtin.Int64) {
  let one: Builtin.Int64 = Builtin.intLiteral_Int64(1._builtinIntegerLiteral)
  x = Builtin.add_Int64(x, one)
}

func inoutWithClosure() -> Builtin.Int64 {
  var val: Builtin.Int64 = Builtin.zeroInitializer()
  modifyViaInout(&val)
  modifyViaInout(&val)
  return val
}

// --- Test 10: closure passed as argument then called ---

func callWithClosure(_ f: (Sample) -> Sample,
                     _ s: Sample) -> Sample {
  return f(s)
}

func passingClosures() -> Sample {
  let cfg = Config(threshold: Builtin.zeroInitializer(),
                   scale: Builtin.zeroInitializer())
  let s = Sample(value: Builtin.zeroInitializer(),
                 weight: Builtin.zeroInitializer())
  // Closure captures cfg from enclosing scope.
  return callWithClosure({ sample in
    Sample(value: Builtin.add_Int64(sample.value, cfg.threshold),
           weight: sample.weight)
  }, s)
}

// --- Test 11: returning a closure that captures local state ---

func makeAdder(_ amount: Builtin.Int64) -> (Builtin.Int64) -> Builtin.Int64 {
  return { x in Builtin.add_Int64(x, amount) }
}

func useAdder() -> Builtin.Int64 {
  let add5 = makeAdder(Builtin.intLiteral_Int64(5._builtinIntegerLiteral))
  return add5(Builtin.zeroInitializer())
}

// --- Test 12: closure stored in a struct ---

struct Operation {
  var execute: (Builtin.Int64) -> Builtin.Int64
}

func operationCapture() -> Builtin.Int64 {
  let offset: Builtin.Int64 = Builtin.zeroInitializer()
  let op = Operation(execute: { x in
    Builtin.add_Int64(x, offset)
  })
  return op.execute(Builtin.zeroInitializer())
}

// --- Test 13: multiple closures sharing the same captured variable ---

func sharedCapture() -> (Builtin.Int64, Builtin.Int64) {
  var shared: Builtin.Int64 = Builtin.zeroInitializer()
  let one: Builtin.Int64 = Builtin.intLiteral_Int64(1._builtinIntegerLiteral)
  let increment = {
    shared = Builtin.add_Int64(shared, one)
  }
  let read = { () -> Builtin.Int64 in
    shared
  }
  increment()
  increment()
  let result = read()
  return (result, shared)
}

// --- Test 14: generic closure composition ---

func composeFns<A, B, C>(_ f: @escaping (A) -> B,
                         _ g: @escaping (B) -> C) -> (A) -> C {
  return { a in g(f(a)) }
}

func genericClosureCompose() -> Builtin.Int64 {
  let step1: (Sample) -> Builtin.Int64 = { $0.value }
  let step2: (Builtin.Int64) -> Builtin.Int64 = { x in
    Builtin.add_Int64(x, x)
  }
  let pipeline = composeFns(step1, step2)
  let s = Sample(value: Builtin.zeroInitializer(),
                 weight: Builtin.zeroInitializer())
  return pipeline(s)
}

// --- Test 15: closure capturing closure ---

func closureCapturingClosure() -> Builtin.Int64 {
  let base: Builtin.Int64 = Builtin.zeroInitializer()
  let innerFn: () -> Builtin.Int64 = { base }
  let outerFn: () -> Builtin.Int64 = {
    let captured = innerFn()
    return Builtin.add_Int64(captured, captured)
  }
  return outerFn()
}
