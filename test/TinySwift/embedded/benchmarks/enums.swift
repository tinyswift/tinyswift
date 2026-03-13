// Benchmark: enum operations
// Budget: <800B .text section

enum Direction {
  case north
  case south
  case east
  case west
}

enum Result {
  case ok(Builtin.Int32)
  case error
}

func opposite(_ d: Direction) -> Direction {
  switch d {
  case .north: return .south
  case .south: return .north
  case .east: return .west
  case .west: return .east
  }
}

func unwrapOr(_ r: Result, _ default_: Builtin.Int32) -> Builtin.Int32 {
  switch r {
  case .ok(let val): return val
  case .error: return default_
  }
}

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  let d = Direction.north
  let d2 = opposite(d)
  let d3 = opposite(d2)

  let r = Result.ok(Builtin.integerLiteral_Int32(42))
  let v = unwrapOr(r, Builtin.zeroInitializer())

  let e = Result.error
  let v2 = unwrapOr(e, Builtin.integerLiteral_Int32(99))

  return Builtin.zeroInitializer()
}
