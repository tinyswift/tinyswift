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
// WASI startup support for TinySwift WebAssembly targets.
//
//===----------------------------------------------------------------------===//

#include <stddef.h>
#include <stdint.h>

// WASI fd_write syscall import
__attribute__((import_module("wasi_snapshot_preview1")))
__attribute__((import_name("fd_write")))
int __wasi_fd_write(int fd, const void *iovs, int iovs_len, int *nwritten);

// WASI proc_exit syscall import
__attribute__((import_module("wasi_snapshot_preview1")))
__attribute__((import_name("proc_exit")))
void __wasi_proc_exit(int code);

int putchar(int c) {
  char ch = (char)c;
  // iovec: pointer to buffer, buffer length
  struct {
    const char *buf;
    int buf_len;
  } iov = { &ch, 1 };
  int nwritten;
  __wasi_fd_write(1 /* stdout */, &iov, 1, &nwritten);
  return c;
}

int puts(const char *s) {
  while (*s) {
    putchar(*s);
    s++;
  }
  return 0;
}

void halt(void) {
  puts("HALT\n");
  __wasi_proc_exit(0);
}

// _start calls main, then halt
int main(int argc, char *argv[]);

void _start(void) {
  main(0, (char **)0);
  halt();
}
