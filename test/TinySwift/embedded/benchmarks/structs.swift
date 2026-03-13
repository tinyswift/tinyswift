// Benchmark: struct operations
// Budget: <500B .text section

struct Point {
  var x: Builtin.Int32
  var y: Builtin.Int32
}

struct Rect {
  var origin: Point
  var size: Point
}

func makePoint(_ x: Builtin.Int32, _ y: Builtin.Int32) -> Point {
  return Point(x: x, y: y)
}

func area(_ r: Rect) -> Builtin.Int32 {
  return Builtin.mul_Int32(r.size.x, r.size.y)
}

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  let origin = makePoint(Builtin.zeroInitializer(), Builtin.zeroInitializer())
  let size = makePoint(
    Builtin.integerLiteral_Int32(10),
    Builtin.integerLiteral_Int32(20)
  )
  let rect = Rect(origin: origin, size: size)
  let a = area(rect)
  return Builtin.zeroInitializer()
}
