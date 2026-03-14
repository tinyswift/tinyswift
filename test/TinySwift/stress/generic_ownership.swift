// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Stress test: generic functions with ownership-sensitive types.
// Exercises generic specialization with struct ownership, protocol
// constraints, generic nesting, and associated types.
// All types are Builtin-based — no ARC symbols should result.

// --- Protocols ---

protocol Measurable {
  func measure() -> Builtin.Int64
}

protocol Resettable {
  func reset() -> Self
}

protocol Combinable {
  func combine(_ other: Self) -> Self
}

protocol Convertible {
  associatedtype Target
  func convert() -> Target
}

// --- Concrete types conforming to protocols ---

struct Gauge {
  var value: Builtin.Int64
}

extension Gauge: Measurable {
  func measure() -> Builtin.Int64 {
    return value
  }
}

extension Gauge: Resettable {
  func reset() -> Gauge {
    return Gauge(value: Builtin.zeroInitializer())
  }
}

extension Gauge: Combinable {
  func combine(_ other: Gauge) -> Gauge {
    return Gauge(value: Builtin.add_Int64(value, other.value))
  }
}

struct Counter {
  var count: Builtin.Int32
}

extension Counter: Measurable {
  func measure() -> Builtin.Int64 {
    return Builtin.zextOrBitCast_Int32_Int64(count)
  }
}

extension Counter: Resettable {
  func reset() -> Counter {
    return Counter(count: Builtin.zeroInitializer())
  }
}

// --- Test 1: single-constraint generic functions ---

func readMeasurement<T: Measurable>(_ item: T) -> Builtin.Int64 {
  return item.measure()
}

func resetItem<T: Resettable>(_ item: T) -> T {
  return item.reset()
}

func combineTwo<T: Combinable>(_ a: T, _ b: T) -> T {
  return a.combine(b)
}

// --- Test 2: multi-constraint generic functions ---

func measureAndReset<T: Measurable & Resettable>(_ item: T) -> (Builtin.Int64, T) {
  let reading = item.measure()
  let cleared = item.reset()
  return (reading, cleared)
}

func combineAndMeasure<T: Combinable & Measurable>(_ a: T, _ b: T) -> Builtin.Int64 {
  let combined = a.combine(b)
  return combined.measure()
}

// --- Test 3: generic wrapper struct ---

struct Box<T> {
  var contents: T
}

func unbox<T>(_ b: Box<T>) -> T {
  return b.contents
}

func rebox<T>(_ value: T) -> Box<T> {
  return Box(contents: value)
}

func mapBox<T, U>(_ b: Box<T>, _ f: (T) -> U) -> Box<U> {
  return Box(contents: f(b.contents))
}

// --- Test 4: nested generic wrappers ---

struct Pair<A, B> {
  var first: A
  var second: B
}

func boxedPair<A, B>(_ a: A, _ b: B) -> Box<Pair<A, B>> {
  return Box(contents: Pair(first: a, second: b))
}

func unboxFirst<A, B>(_ bp: Box<Pair<A, B>>) -> A {
  return bp.contents.first
}

func unboxSecond<A, B>(_ bp: Box<Pair<A, B>>) -> B {
  return bp.contents.second
}

// --- Test 5: doubly-nested generics ---

func doubleBox<T>(_ value: T) -> Box<Box<T>> {
  return Box(contents: Box(contents: value))
}

func flattenBox<T>(_ bb: Box<Box<T>>) -> T {
  return bb.contents.contents
}

func doubleBoxRoundTrip<T>(_ value: T) -> T {
  return flattenBox(doubleBox(value))
}

// --- Test 6: generic function composition ---

func compose<A, B, C>(_ f: (A) -> B, _ g: (B) -> C) -> (A) -> C {
  return { a in g(f(a)) }
}

func applyComposed(_ g: Gauge) -> Builtin.Int64 {
  let toBox: (Gauge) -> Box<Gauge> = { rebox($0) }
  let toReading: (Box<Gauge>) -> Builtin.Int64 = { unbox($0).measure() }
  let pipeline = compose(toBox, toReading)
  return pipeline(g)
}

// --- Test 7: generic with associated type ---

struct GaugeToCounter: Convertible {
  typealias Target = Counter
  var gauge: Gauge
  func convert() -> Counter {
    return Counter(count: Builtin.truncOrBitCast_Int64_Int32(gauge.value))
  }
}

func performConversion<T: Convertible>(_ item: T) -> T.Target {
  return item.convert()
}

func chainConversion(_ g: Gauge) -> Counter {
  let converter = GaugeToCounter(gauge: g)
  return performConversion(converter)
}

// --- Test 8: generic where clauses ---

func matchAndCombine<T>(_ a: T, _ b: T) -> T where T: Combinable {
  return a.combine(b)
}

func pairOfMeasurables<T, U>(_ a: T, _ b: U) -> (Builtin.Int64, Builtin.Int64)
  where T: Measurable, U: Measurable {
  return (a.measure(), b.measure())
}

// --- Test 9: concrete instantiation of all generic paths ---

func exerciseAllPaths() {
  let g = Gauge(value: Builtin.zeroInitializer())
  let c = Counter(count: Builtin.zeroInitializer())

  // Single constraint
  let _ = readMeasurement(g)
  let _ = readMeasurement(c)
  let _ = resetItem(g)
  let _ = resetItem(c)
  let _ = combineTwo(g, g)

  // Multi constraint
  let _ = measureAndReset(g)
  let _ = combineAndMeasure(g, g)

  // Box operations
  let bg = rebox(g)
  let _ = unbox(bg)
  let _ = mapBox(bg) { $0.measure() }

  // Nested generics
  let bp = boxedPair(g, c)
  let _ = unboxFirst(bp)
  let _ = unboxSecond(bp)

  // Double box
  let _ = doubleBoxRoundTrip(g)

  // Associated type conversion
  let _ = chainConversion(g)

  // Where clauses
  let _ = matchAndCombine(g, g)
  let _ = pairOfMeasurables(g, c)
}
