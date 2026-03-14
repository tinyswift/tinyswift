# Blink LED — GPIO Toggle on AArch64 QEMU

Simulates an LED blink pattern by writing alternating values to a memory-mapped
register. Since QEMU's `virt` machine has no real GPIO, this example writes to
the PL011 UART data register to produce a visible blink pattern on the serial
console: `*` for ON, `.` for OFF.

## How it works

### Memory-mapped I/O

The core technique is converting an integer address to a pointer using
`Builtin.inttoptr_Word`, then storing a value at that address:

```swift
func mmioWrite32(_ address: Builtin.Word, _ value: Builtin.Int32) {
    let ptr = Builtin.inttoptr_Word(address) as Builtin.RawPointer
    Builtin.storeRaw(value, ptr)
}

let gpioAddr: Builtin.Word = Builtin.integerLiteral_Word(0x09000000)
mmioWrite32(gpioAddr, onValue)
```

This is the fundamental pattern for all bare-metal peripheral access: convert a
known hardware address to a pointer and read/write through it.

### Busy-wait delay

Without a timer peripheral or OS scheduler, we use a simple countdown loop as a
delay mechanism:

```swift
func delay(_ iterations: Builtin.Int32) {
    var count = iterations
    let one: Builtin.Int32 = Builtin.integerLiteral_Int32(1)
    let zero: Builtin.Int32 = Builtin.zeroInitializer()
    while Builtin.cmp_sgt_Int32(count, zero) != (Builtin.zeroInitializer() as Builtin.Int1) {
        count = Builtin.sub_Int32(count, one)
    }
}
```

Note: With `-Osize`, the compiler may optimize away the loop if it detects
the result is unused. In a real embedded application, you would read from a
hardware timer register inside the loop to prevent elimination.

### Blink loop

The main loop alternates between writing an ON value (`*`) and an OFF value
(`.`) to the GPIO address with delays between each transition:

```
Blink!
*.*.*.*.*.*.
```

## Build

```bash
make
```

## Run

```bash
make run
```

Expected output:
```
Blink!
*.*.*.*.*.*.
HALT
```

## Key concepts

- **Builtin.inttoptr_Word**: Converts a raw integer address to a `Builtin.RawPointer`
- **Builtin.storeRaw**: Stores a value at a raw pointer address
- **Builtin.integerLiteral_Word**: Creates a word-sized integer literal (for addresses)
- **Builtin.cmp_sgt_Int32**: Signed greater-than comparison
- **Busy-wait loops**: The only timing mechanism available without an OS
- **MMIO pattern**: Address -> pointer -> store is the universal embedded I/O pattern
