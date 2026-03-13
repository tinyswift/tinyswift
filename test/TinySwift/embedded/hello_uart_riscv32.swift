// RUN: %target-swift-frontend -emit-ir -target riscv32-none-none-eabi \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature TinySwift \
// RUN:   -parse-stdlib -parse-as-library -Osize %s | FileCheck %s
// REQUIRES: swift_feature_TinySwift
// REQUIRES: swift_feature_Embedded

// RISC-V 32-bit UART demo: writes "Hello, TinySwift!" to UART at 0x10000000.
// Uses @_silgen_name to call putchar from support.c.

@_silgen_name("putchar")
func c_putchar(_ c: Builtin.Int32) -> Builtin.Int32

@_silgen_name("halt")
func c_halt() -> Builtin.RawPointer

@_silgen_name("main")
func main(_ argc: Builtin.Int32, _ argv: Builtin.RawPointer) -> Builtin.Int32 {
  // "Hello, TinySwift!\n" written character by character
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x48 as Builtin.Int8)) // H
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x65 as Builtin.Int8)) // e
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6C as Builtin.Int8)) // l
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6C as Builtin.Int8)) // l
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6F as Builtin.Int8)) // o
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x2C as Builtin.Int8)) // ,
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x20 as Builtin.Int8)) // (space)
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x54 as Builtin.Int8)) // T
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x69 as Builtin.Int8)) // i
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x6E as Builtin.Int8)) // n
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x79 as Builtin.Int8)) // y
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x53 as Builtin.Int8)) // S
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x77 as Builtin.Int8)) // w
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x69 as Builtin.Int8)) // i
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x66 as Builtin.Int8)) // f
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x74 as Builtin.Int8)) // t
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x21 as Builtin.Int8)) // !
  _ = c_putchar(Builtin.zextOrBitCast_Int8_Int32(0x0A as Builtin.Int8)) // \n
  return Builtin.zeroInitializer()
}

// CHECK: define {{.*}}@main
// CHECK-NOT: swift_retain
// CHECK-NOT: swift_release
