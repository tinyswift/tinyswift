//===----------------------------------------------------------------------===//
//
// This source file is part of the TinySwift open source project
//
// Copyright (c) 2024-2026 TinySwift contributors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//
//
// AArch64 startup code for QEMU virt machine.
//
// QEMU virt machine memory map:
//   - RAM starts at 0x40000000
//   - PL011 UART0 at 0x09000000
//
//===----------------------------------------------------------------------===//

#include <stddef.h>
#include <stdint.h>

int main(int argc, char *argv[]);
int puts(const char *p);

// 8KB stack, aligned to 16 bytes (AArch64 requires 16-byte SP alignment)
__attribute__((aligned(16))) char stack[8192];

__attribute__((naked))
__attribute__((section(".start")))
void start(void) {
  // Set stack pointer to top of stack (grows downward)
  __asm__ volatile(
    "adrp x0, stack\n"
    "add  x0, x0, :lo12:stack\n"
    "add  sp, x0, #8192\n"
    "bl   main\n"
    "bl   halt\n"
  );
}

void halt(void) {
  puts("HALT\n");
  // Use QEMU semihosting exit or WFI loop
  // angel_SWIreason_ReportException = 0x18, ADP_Stopped_ApplicationExit = 0x20026
  __asm__ volatile(
    "mov x0, #0x18\n"
    "movk x0, #0x2, lsl #16\n"  // x0 = 0x20018
    "mov x1, #0x20026\n"
    "hlt #0xf000\n"             // AArch64 semihosting trap
  );
  // Fallback: WFI loop
  while (1) {
    __asm__ volatile("wfi");
  }
}

int putchar(int c) {
  // QEMU virt machine PL011 UART0 data register at 0x09000000
  *(volatile uint8_t *)(0x09000000) = (uint8_t)c;
  return c;
}
