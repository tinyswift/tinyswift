//===----------------------------------------------------------------------===//
//
// TinySwift Example: Hello UART
//
// Prints "Hello, TinySwift!" to the PL011 UART on QEMU virt machine.
// Uses @_silgen_name to link against putchar provided by support.c.
//
// Target: aarch64-none-elf (QEMU virt)
// UART:   PL011 at 0x09000000
//
//===----------------------------------------------------------------------===//

// Import C functions from support.c via @_silgen_name.
// putchar writes a single byte to the PL011 UART data register.
@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

// halt triggers QEMU semihosting exit.
@_silgen_name("halt")
func c_halt() -> Builtin.RawPointer

// Helper: emit a single ASCII character
func emit(_ byte: Builtin.Int8) {
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(byte))
}

// Helper: print a hardcoded message character by character.
// Since we have no standard library String type, each character
// is emitted individually using its ASCII hex value.
func printHello() {
  emit(0x48 as Builtin.Int8) // H
  emit(0x65 as Builtin.Int8) // e
  emit(0x6C as Builtin.Int8) // l
  emit(0x6C as Builtin.Int8) // l
  emit(0x6F as Builtin.Int8) // o
  emit(0x2C as Builtin.Int8) // ,
  emit(0x20 as Builtin.Int8) // (space)
  emit(0x54 as Builtin.Int8) // T
  emit(0x69 as Builtin.Int8) // i
  emit(0x6E as Builtin.Int8) // n
  emit(0x79 as Builtin.Int8) // y
  emit(0x53 as Builtin.Int8) // S
  emit(0x77 as Builtin.Int8) // w
  emit(0x69 as Builtin.Int8) // i
  emit(0x66 as Builtin.Int8) // f
  emit(0x74 as Builtin.Int8) // t
  emit(0x21 as Builtin.Int8) // !
  emit(0x0A as Builtin.Int8) // \n
}

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  printHello()
  return Builtin.zeroInitializer()
}
