// Benchmark: generic functions (monomorphized)
// Budget: <1000B .text section

struct Pair<T, U> {
  var first: T
  var second: U
}

func makePair<T, U>(_ a: T, _ b: U) -> Pair<T, U> {
  return Pair(first: a, second: b)
}

func swap_pair<T, U>(_ p: Pair<T, U>) -> Pair<U, T> {
  return Pair(first: p.second, second: p.first)
}

struct Wrapper<T> {
  var value: T

  func get() -> T {
    return value
  }
}

func identity<T>(_ x: T) -> T {
  return x
}

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  // Monomorphized at Int32 x Int32
  let p1 = makePair(
    Builtin.integerLiteral_Int32(1),
    Builtin.integerLiteral_Int32(2)
  )
  let p2 = swap_pair(p1)

  // Monomorphized at different type
  let w = Wrapper<Builtin.Int32>(value: Builtin.integerLiteral_Int32(42))
  let v = w.get()

  // Identity
  let x = identity(Builtin.integerLiteral_Int32(7))

  return Builtin.zeroInitializer()
}
