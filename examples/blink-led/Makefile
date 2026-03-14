# TinySwift Example: Blink LED (AArch64 QEMU virt)
#
# Simulates GPIO toggle via memory-mapped writes on QEMU virt machine.
#
# Usage:
#   make          — build blink.elf
#   make run      — build and run in QEMU
#   make clean    — remove build artifacts

SWIFT_FRONTEND ?= ../../build/tinyswift-debug/bin/swift-frontend
CLANG          ?= clang
LD             ?= ld.lld
QEMU           ?= qemu-system-aarch64

TARGET         := aarch64-none-elf
MODULE_NAME    := blink
SUPPORT_DIR    := ../../utils/embedded-test-support/aarch64-qemu-virt

SWIFT_FLAGS := \
	-emit-object -target $(TARGET) \
	-enable-experimental-feature Embedded \
	-enable-experimental-feature TinySwift \
	-disable-objc-interop -parse-stdlib -parse-as-library -Osize \
	-module-name $(MODULE_NAME)

.PHONY: all run clean

all: $(MODULE_NAME).elf

# Compile Swift source to object file
$(MODULE_NAME).o: blink.swift
	$(SWIFT_FRONTEND) $(SWIFT_FLAGS) -o $@ $<

# Compile C support (startup, putchar, halt)
support.o: $(SUPPORT_DIR)/support.c
	$(CLANG) --target=$(TARGET) -ffreestanding -nostdlib -c -o $@ $<

# Link into final ELF binary
$(MODULE_NAME).elf: $(MODULE_NAME).o support.o $(SUPPORT_DIR)/linkerscript.ld
	$(LD) -T $(SUPPORT_DIR)/linkerscript.ld -o $@ $(MODULE_NAME).o support.o

# Run in QEMU
run: $(MODULE_NAME).elf
	$(QEMU) -machine virt -cpu cortex-a57 -nographic -semihosting \
		-kernel $(MODULE_NAME).elf

clean:
	rm -f *.o *.elf
