//===----------------------------------------------------------------------===//
//
// TinySwift Example: Stack-Based Calculator
//
// Demonstrates structs, enums, and generics working together in a real
// program. Implements a simple stack-based (RPN) calculator with four
// operations: add, subtract, multiply, divide.
//
// The stack is implemented as a fixed-size struct with Builtin fields,
// avoiding any heap allocation. Operations are modeled as an enum.
// A generic swap function shows monomorphization.
//
// Target: aarch64-none-elf (QEMU virt)
//
//===----------------------------------------------------------------------===//

@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

@_silgen_name("halt")
func c_halt() -> Builtin.RawPointer

// --- Helper: emit a single ASCII character ---
func emit(_ byte: Builtin.Int8) {
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(byte))
}

func newline() {
  emit(0x0A as Builtin.Int8)
}

// --- Print a decimal Int32 to UART ---
// Handles negative numbers and zero.
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

  // Extract digits into a fixed buffer (max 10 digits for Int32)
  var digits_0: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_1: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_2: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_3: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_4: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_5: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_6: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_7: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_8: Builtin.Int8 = Builtin.zeroInitializer()
  var digits_9: Builtin.Int8 = Builtin.zeroInitializer()
  var count: Builtin.Int32 = Builtin.zeroInitializer()
  let one: Builtin.Int32 = Builtin.integerLiteral_Int32(1)
  let asciiZero: Builtin.Int8 = 0x30 as Builtin.Int8

  while Builtin.cmp_sgt_Int32(val, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    let remainder = Builtin.srem_Int32(val, ten)
    let digit = Builtin.add_Int8(asciiZero, Builtin.trunc_Int32_Int8(remainder))
    val = Builtin.sdiv_Int32(val, ten)

    // Store digit at position `count`
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(0)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_0 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(1)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_1 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(2)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_2 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(3)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_3 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(4)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_4 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(5)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_5 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(6)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_6 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(7)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_7 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(8)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_8 = digit }
    if Builtin.cmp_eq_Int32(count, Builtin.integerLiteral_Int32(9)) != (Builtin.zeroInitializer() as Builtin.Int1) { digits_9 = digit }

    count = Builtin.add_Int32(count, one)
  }

  // Print digits in reverse order
  var i = Builtin.sub_Int32(count, one)
  while Builtin.cmp_sge_Int32(i, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    var d: Builtin.Int8 = Builtin.zeroInitializer()
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(0)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_0 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(1)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_1 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(2)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_2 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(3)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_3 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(4)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_4 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(5)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_5 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(6)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_6 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(7)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_7 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(8)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_8 }
    if Builtin.cmp_eq_Int32(i, Builtin.integerLiteral_Int32(9)) != (Builtin.zeroInitializer() as Builtin.Int1) { d = digits_9 }
    emit(d)
    i = Builtin.sub_Int32(i, one)
  }
}

// ============================================================================
// MARK: - Enum: Operation
// ============================================================================

/// Represents the four basic arithmetic operations.
/// Demonstrates TinySwift enums with associated values.
enum Operation {
  case add
  case sub
  case mul
  case div
}

/// Applies an operation to two operands.
func apply(_ op: Operation, _ a: Builtin.Int32, _ b: Builtin.Int32) -> Builtin.Int32 {
  switch op {
  case .add: return Builtin.add_Int32(a, b)
  case .sub: return Builtin.sub_Int32(a, b)
  case .mul: return Builtin.mul_Int32(a, b)
  case .div: return Builtin.sdiv_Int32(a, b)
  }
}

// ============================================================================
// MARK: - Struct: Fixed-size stack
// ============================================================================

/// A fixed-capacity stack of 8 Int32 values.
/// Uses individual struct fields since we have no arrays or heap.
struct Stack {
  var s0: Builtin.Int32
  var s1: Builtin.Int32
  var s2: Builtin.Int32
  var s3: Builtin.Int32
  var s4: Builtin.Int32
  var s5: Builtin.Int32
  var s6: Builtin.Int32
  var s7: Builtin.Int32
  var top: Builtin.Int32  // number of elements

  /// Create an empty stack
  static func empty() -> Stack {
    return Stack(
      s0: Builtin.zeroInitializer(), s1: Builtin.zeroInitializer(),
      s2: Builtin.zeroInitializer(), s3: Builtin.zeroInitializer(),
      s4: Builtin.zeroInitializer(), s5: Builtin.zeroInitializer(),
      s6: Builtin.zeroInitializer(), s7: Builtin.zeroInitializer(),
      top: Builtin.zeroInitializer()
    )
  }

  /// Read the value at slot index
  func get(_ index: Builtin.Int32) -> Builtin.Int32 {
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(0)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s0 }
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(1)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s1 }
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(2)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s2 }
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(3)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s3 }
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(4)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s4 }
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(5)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s5 }
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(6)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s6 }
    if Builtin.cmp_eq_Int32(index, Builtin.integerLiteral_Int32(7)) != (Builtin.zeroInitializer() as Builtin.Int1) { return s7 }
    return Builtin.zeroInitializer()
  }

  /// Push a value onto the stack (returns new stack)
  func push(_ value: Builtin.Int32) -> Stack {
    var new = self
    let idx = self.top
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(0)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s0 = value }
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(1)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s1 = value }
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(2)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s2 = value }
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(3)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s3 = value }
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(4)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s4 = value }
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(5)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s5 = value }
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(6)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s6 = value }
    if Builtin.cmp_eq_Int32(idx, Builtin.integerLiteral_Int32(7)) != (Builtin.zeroInitializer() as Builtin.Int1) { new.s7 = value }
    new.top = Builtin.add_Int32(self.top, Builtin.integerLiteral_Int32(1))
    return new
  }

  /// Peek at the top value
  func peek() -> Builtin.Int32 {
    let idx = Builtin.sub_Int32(self.top, Builtin.integerLiteral_Int32(1))
    return self.get(idx)
  }

  /// Pop the top value (returns new stack and popped value)
  func pop() -> (Stack, Builtin.Int32) {
    let value = self.peek()
    var new = self
    new.top = Builtin.sub_Int32(self.top, Builtin.integerLiteral_Int32(1))
    return (new, value)
  }
}

// ============================================================================
// MARK: - Generics: Pair and swap
// ============================================================================

/// A generic pair of two values.
/// Demonstrates that TinySwift fully monomorphizes generics at compile time.
struct Pair<A, B> {
  var first: A
  var second: B
}

/// Swap the elements of a pair (generic function).
func swapPair<A, B>(_ p: Pair<A, B>) -> Pair<B, A> {
  return Pair(first: p.second, second: p.first)
}

/// Print an operation result: "a op b = result\n"
func printResult(_ a: Builtin.Int32, _ opChar: Builtin.Int8, _ b: Builtin.Int32, _ result: Builtin.Int32) {
  printInt32(a)
  emit(0x20 as Builtin.Int8) // space
  emit(opChar)
  emit(0x20 as Builtin.Int8) // space
  printInt32(b)
  emit(0x20 as Builtin.Int8) // space
  emit(0x3D as Builtin.Int8) // =
  emit(0x20 as Builtin.Int8) // space
  printInt32(result)
  newline()
}

// ============================================================================
// MARK: - Main
// ============================================================================

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  // Build a calculation using the stack: (10 + 20) * 3

  // Step 1: Push 10 and 20, then add
  var stack = Stack.empty()
  stack = stack.push(Builtin.integerLiteral_Int32(10))
  stack = stack.push(Builtin.integerLiteral_Int32(20))

  // Pop two values and apply add
  let (stack2, b1) = stack.pop()
  let (stack3, a1) = stack2.pop()
  let sum = apply(.add, a1, b1)
  printResult(a1, 0x2B as Builtin.Int8, b1, sum) // "10 + 20 = 30"

  // Step 2: Push result and 3, then multiply
  var stack4 = stack3.push(sum)
  stack4 = stack4.push(Builtin.integerLiteral_Int32(3))
  let (stack5, b2) = stack4.pop()
  let (stack6, a2) = stack5.pop()
  let product = apply(.mul, a2, b2)
  printResult(a2, 0x2A as Builtin.Int8, b2, product) // "30 * 3 = 90"

  // Step 3: Push result and 7, then subtract
  var stack7 = stack6.push(product)
  stack7 = stack7.push(Builtin.integerLiteral_Int32(7))
  let (stack8, b3) = stack7.pop()
  let (stack9, a3) = stack8.pop()
  let diff = apply(.sub, a3, b3)
  printResult(a3, 0x2D as Builtin.Int8, b3, diff) // "90 - 7 = 83"

  // Step 4: Push result and 4, then divide
  var stack10 = stack9.push(diff)
  stack10 = stack10.push(Builtin.integerLiteral_Int32(4))
  let (stack11, b4) = stack10.pop()
  let (_, a4) = stack11.pop()
  let quotient = apply(.div, a4, b4)
  printResult(a4, 0x2F as Builtin.Int8, b4, quotient) // "83 / 4 = 20"

  // Demonstrate generics: create and swap a Pair
  let pair = Pair<Builtin.Int32, Builtin.Int32>(
    first: Builtin.integerLiteral_Int32(42),
    second: Builtin.integerLiteral_Int32(99)
  )
  let swapped = swapPair(pair)
  // swapped.first should be 99, swapped.second should be 42

  return Builtin.zeroInitializer()
}
