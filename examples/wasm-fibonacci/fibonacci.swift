//===----------------------------------------------------------------------===//
//
// TinySwift Example: Fibonacci via WebAssembly/WASI
//
// Computes Fibonacci numbers and outputs them via WASI file descriptors.
// Demonstrates the WebAssembly target for TinySwift.
//
// Target: wasm32-unknown-wasi
//
//===----------------------------------------------------------------------===//

@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

@_silgen_name("halt")
func c_halt()

// --- Helper: emit a single ASCII character ---
func emit(_ byte: Builtin.Int8) {
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(byte))
}

func newline() {
  emit(0x0A as Builtin.Int8)
}

// --- Print a decimal Int32 to stdout ---
func printInt32(_ value: Builtin.Int32) {
  let zero: Builtin.Int32 = Builtin.zeroInitializer()
  let ten: Builtin.Int32 = Builtin.integerLiteral_Int32(10)
  let neg: Builtin.Int32 = Builtin.integerLiteral_Int32(-1)

  // Handle zero
  if Builtin.cmp_eq_Int32(value, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    emit(0x30 as Builtin.Int8) // '0'
    return
  }

  var val = value

  // Handle negative
  if Builtin.cmp_slt_Int32(val, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    emit(0x2D as Builtin.Int8) // '-'
    val = Builtin.mul_Int32(val, neg)
  }

  // Extract digits into fixed slots (max 10 digits for Int32)
  var d0: Builtin.Int8 = Builtin.zeroInitializer()
  var d1: Builtin.Int8 = Builtin.zeroInitializer()
  var d2: Builtin.Int8 = Builtin.zeroInitializer()
  var d3: Builtin.Int8 = Builtin.zeroInitializer()
  var d4: Builtin.Int8 = Builtin.zeroInitializer()
  var d5: Builtin.Int8 = Builtin.zeroInitializer()
  var d6: Builtin.Int8 = Builtin.zeroInitializer()
  var d7: Builtin.Int8 = Builtin.zeroInitializer()
  var d8: Builtin.Int8 = Builtin.zeroInitializer()
  var d9: Builtin.Int8 = Builtin.zeroInitializer()
  var count: Builtin.Int32 = Builtin.zeroInitializer()
  let one: Builtin.Int32 = Builtin.integerLiteral_Int32(1)
  let asciiZero: Builtin.Int8 = 0x30 as Builtin.Int8

  while Builtin.cmp_sgt_Int32(val, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    let remainder = Builtin.srem_Int32(val, ten)
    let digit = Builtin.add_Int8(asciiZero, Builtin.trunc_Int32_Int8(remainder))
    val = Builtin.sdiv_Int32(val, ten)

    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(0)) != (Builtin.zeroInitializer() as Builtin.Int1) { d0 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(1)) != (Builtin.zeroInitializer() as Builtin.Int1) { d1 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(2)) != (Builtin.zeroInitializer() as Builtin.Int1) { d2 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(3)) != (Builtin.zeroInitializer() as Builtin.Int1) { d3 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(4)) != (Builtin.zeroInitializer() as Builtin.Int1) { d4 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(5)) != (Builtin.zeroInitializer() as Builtin.Int1) { d5 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(6)) != (Builtin.zeroInitializer() as Builtin.Int1) { d6 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(7)) != (Builtin.zeroInitializer() as Builtin.Int1) { d7 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(8)) != (Builtin.zeroInitializer() as Builtin.Int1) { d8 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(9)) != (Builtin.zeroInitializer() as Builtin.Int1) { d9 = digit }

    count = Builtin.add_Int32(count, one)
  }

  // Print digits in reverse order
  var i = Builtin.sub_Int32(count, one)
  while Builtin.cmp_sge_Int32(i, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    var ch: Builtin.Int8 = Builtin.zeroInitializer()
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(0)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d0 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(1)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d1 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(2)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d2 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(3)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d3 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(4)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d4 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(5)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d5 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(6)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d6 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(7)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d7 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(8)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d8 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(9)) != (Builtin.zeroInitializer() as Builtin.Int1) { ch = d9 }
    emit(ch)
    i = Builtin.sub_Int32(i, one)
  }
}

// ============================================================================
// MARK: - Fibonacci
// ============================================================================

/// Compute the n-th Fibonacci number iteratively.
/// Uses only Builtin types and operations — no standard library.
///
/// fib(0) = 0, fib(1) = 1, fib(n) = fib(n-1) + fib(n-2)
func fibonacci(_ n: Builtin.Int32) -> Builtin.Int32 {
  let zero: Builtin.Int32 = Builtin.zeroInitializer()
  let one: Builtin.Int32 = Builtin.integerLiteral_Int32(1)

  // fib(0) = 0
  if Builtin.cmp_eq_Int32(n, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    return zero
  }
  // fib(1) = 1
  if Builtin.cmp_eq_Int32(n, one) != (Builtin.zeroInitializer() as Builtin.Int1) {
    return one
  }

  // Iterative computation
  var prev = zero     // fib(i-2)
  var curr = one      // fib(i-1)
  var i = Builtin.integerLiteral_Int32(2)

  while Builtin.cmp_sle_Int32(i, n) != (Builtin.zeroInitializer() as Builtin.Int1) {
    let next = Builtin.add_Int32(prev, curr)
    prev = curr
    curr = next
    i = Builtin.add_Int32(i, one)
  }

  return curr
}

// Print "fib(" prefix
func printFibPrefix() {
  emit(0x66 as Builtin.Int8) // f
  emit(0x69 as Builtin.Int8) // i
  emit(0x62 as Builtin.Int8) // b
  emit(0x28 as Builtin.Int8) // (
}

// Print ") = "
func printFibInfix() {
  emit(0x29 as Builtin.Int8) // )
  emit(0x20 as Builtin.Int8) // (space)
  emit(0x3D as Builtin.Int8) // =
  emit(0x20 as Builtin.Int8) // (space)
}

// ============================================================================
// MARK: - Main
// ============================================================================

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  // Compute and print the first 16 Fibonacci numbers: fib(0) through fib(15)
  let limit: Builtin.Int32 = Builtin.integerLiteral_Int32(16)
  let zero: Builtin.Int32 = Builtin.zeroInitializer()
  let one: Builtin.Int32 = Builtin.integerLiteral_Int32(1)

  var n = zero
  while Builtin.cmp_slt_Int32(n, limit) != (Builtin.zeroInitializer() as Builtin.Int1) {
    let result = fibonacci(n)

    // Print "fib(N) = R\n"
    printFibPrefix()
    printInt32(n)
    printFibInfix()
    printInt32(result)
    newline()

    n = Builtin.add_Int32(n, one)
  }

  return Builtin.zeroInitializer()
}
