// Benchmark: empty program (measures baseline overhead)
// Budget: <200B .text section

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  return Builtin.zeroInitializer()
}
