# TinySwift Example: Fibonacci via WebAssembly/WASI
#
# Computes Fibonacci numbers and prints them using WASI I/O.
#
# Usage:
#   make          — build fibonacci.wasm
#   make run      — build and run with wasmtime
#   make clean    — remove build artifacts

SWIFT_FRONTEND ?= ../../build/tinyswift-debug/bin/swift-frontend
CLANG          ?= clang
WASM_LD        ?= wasm-ld
WASMTIME       ?= wasmtime

TARGET         := wasm32-unknown-wasi
MODULE_NAME    := fibonacci
SUPPORT_DIR    := ../../utils/embedded-test-support/wasm32-wasi

SWIFT_FLAGS := \
	-emit-object -target $(TARGET) \
	-enable-experimental-feature Embedded \
	-enable-experimental-feature TinySwift \
	-disable-objc-interop -parse-stdlib -parse-as-library -Osize \
	-module-name $(MODULE_NAME)

.PHONY: all run clean

all: $(MODULE_NAME).wasm

# Compile Swift source to object file
$(MODULE_NAME).o: fibonacci.swift
	$(SWIFT_FRONTEND) $(SWIFT_FLAGS) -o $@ $<

# Compile C support (WASI putchar, puts, halt, _start)
support.o: $(SUPPORT_DIR)/support.c
	$(CLANG) --target=$(TARGET) -ffreestanding -nostdlib -c -o $@ $<

# Link into final WebAssembly module
# --no-entry: support.c provides _start which calls main
# --export=_start: export the entry point for WASI runtimes
# --allow-undefined: WASI imports (fd_write, proc_exit) are resolved at runtime
$(MODULE_NAME).wasm: $(MODULE_NAME).o support.o
	$(WASM_LD) --no-entry --export=_start --allow-undefined \
		-o $@ $(MODULE_NAME).o support.o

# Run with wasmtime (WASI-compatible runtime)
run: $(MODULE_NAME).wasm
	$(WASMTIME) $(MODULE_NAME).wasm

clean:
	rm -f *.o *.wasm
