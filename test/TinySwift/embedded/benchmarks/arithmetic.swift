// Benchmark: arithmetic operations
// Budget: <500B .text section

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  let a: Builtin.Int32 = Builtin.integerLiteral_Int32(42)
  let b: Builtin.Int32 = Builtin.integerLiteral_Int32(17)

  let sum = Builtin.add_Int32(a, b)
  let diff = Builtin.sub_Int32(a, b)
  let prod = Builtin.mul_Int32(a, b)
  let quot = Builtin.sdiv_Int32(a, b)
  let rem = Builtin.srem_Int32(a, b)

  // Shift operations
  let shl = Builtin.shl_Int32(a, Builtin.integerLiteral_Int32(2))
  let shr = Builtin.ashr_Int32(a, Builtin.integerLiteral_Int32(1))

  // Bitwise
  let and_ = Builtin.and_Int32(a, b)
  let or_ = Builtin.or_Int32(a, b)
  let xor_ = Builtin.xor_Int32(a, b)

  return Builtin.zeroInitializer()
}
