/*===----------------------------------------------------------------------===*\
|*                                                                            *|
|*  TinySwift Minimal Runtime                                                 *|
|*                                                                            *|
|*  This file provides the absolute minimum runtime support for TinySwift     *|
|*  programs targeting freestanding/embedded environments. It contains:        *|
|*                                                                            *|
|*    - Trap handler (fatal error)                                            *|
|*    - Memory operations (memcpy, memset, bzero)                             *|
|*    - Allocator stubs (trap by default; override for heap targets)          *|
|*    - Stack canary support stubs                                            *|
|*                                                                            *|
|*  Compile with:                                                             *|
|*    clang -c -ffreestanding -nostdlib -target arm64-none-eabi Runtime.c     *|
|*    clang -c -ffreestanding -nostdlib -target riscv32-none-elf Runtime.c    *|
|*    clang -c -ffreestanding -nostdlib -target x86_64-none-elf Runtime.c     *|
|*    clang -c -ffreestanding -nostdlib -target wasm32-unknown-unknown        *|
|*           Runtime.c                                                        *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifndef TINYSWIFT_RUNTIME_H
#define TINYSWIFT_RUNTIME_H

/* -------------------------------------------------------------------------- */
/* Platform detection                                                         */
/* -------------------------------------------------------------------------- */

#if defined(__wasm__) || defined(__wasm32__) || defined(__wasm64__)
  #define TINYSWIFT_WASM 1
#else
  #define TINYSWIFT_WASM 0
#endif

#if defined(__aarch64__) || defined(__arm64__)
  #define TINYSWIFT_ARM64 1
#else
  #define TINYSWIFT_ARM64 0
#endif

#if defined(__riscv)
  #define TINYSWIFT_RISCV 1
#else
  #define TINYSWIFT_RISCV 0
#endif

/* -------------------------------------------------------------------------- */
/* Compiler attribute macros                                                  */
/* -------------------------------------------------------------------------- */

#define TINYSWIFT_EXPORT __attribute__((visibility("default")))
#define TINYSWIFT_NORETURN __attribute__((noreturn))
#define TINYSWIFT_WEAK __attribute__((weak))
#define TINYSWIFT_NOINLINE __attribute__((noinline))
#define TINYSWIFT_USED __attribute__((used))

/* Use __SIZE_TYPE__ instead of including <stddef.h> for maximum portability
   in freestanding mode. __SIZE_TYPE__ is a compiler builtin. */
typedef __SIZE_TYPE__ tinyswift_size_t;
typedef __UINTPTR_TYPE__ tinyswift_uintptr_t;

/* -------------------------------------------------------------------------- */
/* Extern "C" guard for C++ compatibility                                     */
/* -------------------------------------------------------------------------- */

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================== */
/*  1. Trap handler                                                           */
/* ========================================================================== */

/**
 * _tinyswift_trap -- Called on fatal errors (precondition failure, nil
 * force-unwrap, integer overflow, fatalError(), etc.)
 *
 * Default: enters an infinite loop so a debugger can catch it.
 * Override this weak symbol with a platform-specific implementation:
 *   - ARM: trigger BKPT or HardFault
 *   - RISC-V: trigger EBREAK
 *   - Wasm: unreachable instruction
 *   - x86: int3 or hlt
 */
TINYSWIFT_EXPORT
TINYSWIFT_NORETURN
TINYSWIFT_WEAK
TINYSWIFT_NOINLINE
void _tinyswift_trap(void) {
#if TINYSWIFT_WASM
    __builtin_trap();
#elif TINYSWIFT_ARM64
    /* On ARM64, __builtin_trap() emits BRK #1 which is caught by debuggers. */
    __builtin_trap();
#elif TINYSWIFT_RISCV
    /* RISC-V EBREAK instruction. */
    __builtin_trap();
#else
    /* Generic fallback: spin forever. Debugger can inspect the call stack. */
    while (1) {
        /* Prevent the loop from being optimized away. */
        __asm__ volatile("" ::: "memory");
    }
#endif
    /* Hint to the compiler that this point is unreachable.
       Only reached if __builtin_trap() somehow returns (it should not). */
    __builtin_unreachable();
}

/* ========================================================================== */
/*  2. Memory operations                                                      */
/* ========================================================================== */

/**
 * _tinyswift_memcpy -- Copy `n` bytes from `src` to `dest`.
 *
 * The regions must not overlap. For overlapping regions, use memmove
 * (not provided -- TinySwift does not need it).
 *
 * This is a simple byte-by-byte implementation. For performance-critical
 * targets, override this weak symbol with a platform-optimized version.
 */
TINYSWIFT_EXPORT
TINYSWIFT_WEAK
void *_tinyswift_memcpy(void *dest, const void *src, tinyswift_size_t n) {
    unsigned char *d = (unsigned char *)dest;
    const unsigned char *s = (const unsigned char *)src;
    for (tinyswift_size_t i = 0; i < n; i++) {
        d[i] = s[i];
    }
    return dest;
}

/**
 * _tinyswift_memset -- Fill `n` bytes of `dest` with byte value `value`.
 */
TINYSWIFT_EXPORT
TINYSWIFT_WEAK
void *_tinyswift_memset(void *dest, int value, tinyswift_size_t n) {
    unsigned char *d = (unsigned char *)dest;
    unsigned char v = (unsigned char)value;
    for (tinyswift_size_t i = 0; i < n; i++) {
        d[i] = v;
    }
    return dest;
}

/**
 * _tinyswift_bzero -- Zero `n` bytes at `dest`.
 * Convenience wrapper around _tinyswift_memset.
 */
TINYSWIFT_EXPORT
TINYSWIFT_WEAK
void _tinyswift_bzero(void *dest, tinyswift_size_t n) {
    _tinyswift_memset(dest, 0, n);
}

/* ========================================================================== */
/*  3. Standard C library shims                                               */
/* ========================================================================== */

/*
 * Some compiler-generated code (e.g., structure copies with optimization
 * disabled) emits calls to standard memcpy/memset/bzero. Provide these as
 * aliases to our implementations so the linker resolves them.
 */

TINYSWIFT_WEAK
void *memcpy(void *dest, const void *src, tinyswift_size_t n) {
    return _tinyswift_memcpy(dest, src, n);
}

TINYSWIFT_WEAK
void *memset(void *dest, int value, tinyswift_size_t n) {
    return _tinyswift_memset(dest, value, n);
}

/* ========================================================================== */
/*  4. Allocator stubs                                                        */
/* ========================================================================== */

/**
 * _tinyswift_alloc -- Allocate `size` bytes with the given `alignment`.
 *
 * DEFAULT: Traps. Heap allocation is not available unless the target
 * provides an allocator override.
 *
 * To enable heap allocation, override these weak symbols in your target's
 * startup code with real malloc/free implementations.
 *
 * The Swift compiler emits calls to these from Builtin.allocRaw and
 * Builtin.deallocRaw (via UnsafeMutablePointer.allocate / .deallocate).
 */
/*
 * Bump allocator (optional).
 *
 * When TINYSWIFT_ENABLE_BUMP_ALLOC is defined, a simple bump-forward
 * allocator is provided using a static buffer. Deallocation is a no-op.
 * This is suitable for short-lived embedded programs where memory is
 * never reclaimed.
 *
 * Configure the buffer size with TINYSWIFT_HEAP_SIZE (default: 4096).
 */
#ifdef TINYSWIFT_ENABLE_BUMP_ALLOC

#ifndef TINYSWIFT_HEAP_SIZE
#define TINYSWIFT_HEAP_SIZE 4096
#endif

static unsigned char _tinyswift_heap[TINYSWIFT_HEAP_SIZE]
    __attribute__((aligned(16)));
static tinyswift_size_t _tinyswift_heap_offset = 0;

TINYSWIFT_EXPORT
void *_tinyswift_alloc(tinyswift_size_t size, tinyswift_size_t alignment) {
    /* Align the current offset. */
    tinyswift_size_t mask = alignment - 1;
    tinyswift_size_t aligned = (_tinyswift_heap_offset + mask) & ~mask;

    if (aligned + size > TINYSWIFT_HEAP_SIZE) {
        /* Out of memory. Trap. */
        _tinyswift_trap();
        return (void *)0;
    }

    void *ptr = &_tinyswift_heap[aligned];
    _tinyswift_heap_offset = aligned + size;

#ifndef NDEBUG
    /* Debug: fill allocated memory with 0xCD pattern. */
    _tinyswift_memset(ptr, 0xCD, size);
#endif

    return ptr;
}

TINYSWIFT_EXPORT
void _tinyswift_dealloc(void *ptr, tinyswift_size_t size,
                         tinyswift_size_t alignment) {
    /* Bump allocator: deallocation is a no-op. */
    (void)alignment;
#ifndef NDEBUG
    /* Debug: poison freed memory with 0xDE pattern. */
    if (ptr && size > 0) {
        _tinyswift_memset(ptr, 0xDE, size);
    }
#else
    (void)ptr;
    (void)size;
#endif
}

#else /* !TINYSWIFT_ENABLE_BUMP_ALLOC */

TINYSWIFT_EXPORT
TINYSWIFT_WEAK
void *_tinyswift_alloc(tinyswift_size_t size, tinyswift_size_t alignment) {
    (void)size;
    (void)alignment;
    /* No heap available. Trap. */
    _tinyswift_trap();
    /* Unreachable, but silences compiler warning. */
    return (void *)0;
}

/**
 * _tinyswift_dealloc -- Free memory previously allocated by _tinyswift_alloc.
 *
 * DEFAULT: Traps. See _tinyswift_alloc for details.
 */
TINYSWIFT_EXPORT
TINYSWIFT_WEAK
void _tinyswift_dealloc(void *ptr, tinyswift_size_t size,
                         tinyswift_size_t alignment) {
    (void)ptr;
    (void)size;
    (void)alignment;
    _tinyswift_trap();
}

#endif /* TINYSWIFT_ENABLE_BUMP_ALLOC */

/* ========================================================================== */
/*  5. Stack canary support                                                   */
/* ========================================================================== */

/**
 * Stack buffer overflow detection support.
 *
 * When the compiler is configured with -fstack-protector, it emits code that
 * checks a "canary" value on the stack before returning from functions with
 * local buffers. If the canary has been overwritten, __stack_chk_fail is
 * called.
 *
 * __stack_chk_guard: The expected canary value. In a real system, this should
 * be initialized to a random value at startup. The fixed value here is a
 * placeholder.
 *
 * __stack_chk_fail: Called when stack corruption is detected.
 */
TINYSWIFT_EXPORT
TINYSWIFT_USED
TINYSWIFT_WEAK
unsigned long __stack_chk_guard = 0xDEAD0BAD;

TINYSWIFT_EXPORT
TINYSWIFT_NORETURN
TINYSWIFT_WEAK
void __stack_chk_fail(void) {
    _tinyswift_trap();
    __builtin_unreachable();
}

/* ========================================================================== */
/*  6. Swift runtime entry points (stubs)                                     */
/* ========================================================================== */

/*
 * The Swift compiler may emit calls to these symbols even in embedded mode.
 * We provide minimal stubs that trap, ensuring link errors are avoided while
 * making it immediately obvious if any code path reaches them.
 *
 * In a correctly compiled TinySwift program, none of these should be called.
 * They exist purely as linker safety nets.
 */

TINYSWIFT_EXPORT
TINYSWIFT_NORETURN
TINYSWIFT_WEAK
void swift_deletedMethodError(void) {
    _tinyswift_trap();
    __builtin_unreachable();
}

/* ========================================================================== */
/*  7. Arithmetic overflow trap                                               */
/* ========================================================================== */

/**
 * _tinyswift_overflow_trap -- Called when checked arithmetic overflows.
 *
 * The Swift compiler's Builtin.condfail lowers to a conditional branch
 * that calls this on the overflow path.
 */
TINYSWIFT_EXPORT
TINYSWIFT_NORETURN
TINYSWIFT_WEAK
void _tinyswift_overflow_trap(void) {
    _tinyswift_trap();
    __builtin_unreachable();
}

/* -------------------------------------------------------------------------- */
/* End extern "C"                                                             */
/* -------------------------------------------------------------------------- */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* TINYSWIFT_RUNTIME_H */
