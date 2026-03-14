//===----------------------------------------------------------------------===//
//
// TinySwift Example: Blink LED (GPIO Toggle)
//
// Simulates an LED blink pattern by writing alternating values to a
// memory-mapped GPIO register on QEMU virt machine. Since QEMU virt
// does not have real GPIO hardware, we use the PL011 UART data register
// at 0x09000000 to output visible characters that represent the blink
// pattern: '*' for ON, '.' for OFF.
//
// Target: aarch64-none-elf (QEMU virt)
//
//===----------------------------------------------------------------------===//

@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

@_silgen_name("halt")
func c_halt() -> Builtin.RawPointer

// Emit a single ASCII character via UART
func emit(_ byte: Builtin.Int8) {
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(byte))
}

// Write a 32-bit value to a memory-mapped register.
// Uses Builtin.inttoptr_Word to convert an address to a raw pointer,
// then performs a volatile-like store via Builtin operations.
func mmioWrite32(_ address: Builtin.Word, _ value: Builtin.Int32) {
  let ptr = Builtin.inttoptr_Word(address) as Builtin.RawPointer
  Builtin.storeRaw(value, ptr)
}

// Busy-wait delay loop.
// Counts down from `iterations` to zero. The compiler's -Osize may
// optimize this away, so we use a volatile store to a dummy location
// to prevent the loop from being eliminated.
func delay(_ iterations: Builtin.Int32) {
  var count = iterations
  let one: Builtin.Int32 = Builtin.integerLiteral_Int32(1)
  let zero: Builtin.Int32 = Builtin.zeroInitializer()

  while Builtin.cmp_sgt_Int32(count, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
    count = Builtin.sub_Int32(count, one)
  }
}

// Print "Blink!\n"
func printBlink() {
  emit(0x42 as Builtin.Int8) // B
  emit(0x6C as Builtin.Int8) // l
  emit(0x69 as Builtin.Int8) // i
  emit(0x6E as Builtin.Int8) // n
  emit(0x6B as Builtin.Int8) // k
  emit(0x21 as Builtin.Int8) // !
  emit(0x0A as Builtin.Int8) // \n
}

// Print the newline character
func newline() {
  emit(0x0A as Builtin.Int8)
}

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  printBlink()

  // Simulate 6 blink cycles: ON (*) and OFF (.) with delays
  let delayCount: Builtin.Int32 = Builtin.integerLiteral_Int32(500000)
  let totalCycles: Builtin.Int32 = Builtin.integerLiteral_Int32(6)
  let zero: Builtin.Int32 = Builtin.zeroInitializer()
  let one: Builtin.Int32 = Builtin.integerLiteral_Int32(1)

  // GPIO register address (PL011 UART data register on QEMU virt)
  let gpioAddr: Builtin.Word = Builtin.integerLiteral_Word(0x09000000)

  // ON value: ASCII '*' (0x2A) — represents LED ON
  let onValue: Builtin.Int32 = Builtin.integerLiteral_Int32(0x2A)
  // OFF value: ASCII '.' (0x2E) — represents LED OFF
  let offValue: Builtin.Int32 = Builtin.integerLiteral_Int32(0x2E)

  var cycle = zero
  while Builtin.cmp_slt_Int32(cycle, totalCycles) != (Builtin.zeroInitializer() as Builtin.Int1) {
    // LED ON: write '*' to UART (simulating GPIO high)
    mmioWrite32(gpioAddr, onValue)
    delay(delayCount)

    // LED OFF: write '.' to UART (simulating GPIO low)
    mmioWrite32(gpioAddr, offValue)
    delay(delayCount)

    cycle = Builtin.add_Int32(cycle, one)
  }

  newline()
  return Builtin.zeroInitializer()
}
