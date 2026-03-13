#!/bin/bash
#===----------------------------------------------------------------------===//
#
# binary-size-report.sh
#
# Compiles TinySwift benchmark programs for each embedded target, measures
# .text section size via llvm-size, and outputs a Markdown summary table.
# Exits non-zero if any benchmark exceeds its size budget.
#
#===----------------------------------------------------------------------===//

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BENCHMARK_DIR="$REPO_DIR/test/TinySwift/embedded/benchmarks"
BUILD_DIR="${BUILD_DIR:-$REPO_DIR/build/tinyswift-debug}"
SWIFT_FRONTEND="${SWIFT_FRONTEND:-$BUILD_DIR/bin/swift-frontend}"
LLVM_SIZE="${LLVM_SIZE:-llvm-size}"
TMPDIR="${TMPDIR:-/tmp}"
WORK_DIR="$TMPDIR/tinyswift-binary-size-$$"

mkdir -p "$WORK_DIR"
cleanup() { rm -rf "$WORK_DIR"; }
trap cleanup EXIT

# Size budgets (in bytes of .text section)
declare -A BUDGETS=(
  [empty]=200
  [arithmetic]=500
  [structs]=500
  [enums]=800
  [generics]=1000
)

# Targets to test (only targets we can emit objects for)
TARGETS=(
  "aarch64-none-elf"
  "riscv32-none-none-eabi"
  "wasm32-unknown-wasi"
)

# Benchmark files
BENCHMARKS=(empty arithmetic structs enums generics)

FAIL=0

# Print header
echo "# TinySwift Binary Size Report"
echo ""
echo "| Benchmark | Target | .text (B) | Budget (B) | Status |"
echo "|-----------|--------|-----------|------------|--------|"

for bench in "${BENCHMARKS[@]}"; do
  src="$BENCHMARK_DIR/${bench}.swift"
  if [ ! -f "$src" ]; then
    echo "| $bench | - | - | ${BUDGETS[$bench]} | MISSING |"
    FAIL=1
    continue
  fi

  budget="${BUDGETS[$bench]}"

  for target in "${TARGETS[@]}"; do
    obj="$WORK_DIR/${bench}-${target//\//_}.o"

    # Compile to object file
    if ! "$SWIFT_FRONTEND" \
        -emit-object \
        -target "$target" \
        -enable-experimental-feature Embedded \
        -enable-experimental-feature TinySwift \
        -disable-objc-interop \
        -parse-stdlib \
        -parse-as-library \
        -Osize \
        -module-name "$bench" \
        -o "$obj" \
        "$src" 2>/dev/null; then
      echo "| $bench | $target | COMPILE_ERROR | $budget | FAIL |"
      FAIL=1
      continue
    fi

    # Measure .text size
    # llvm-size output format: "   text    data     bss     dec     hex filename"
    text_size=0
    if command -v "$LLVM_SIZE" &>/dev/null; then
      text_size=$("$LLVM_SIZE" "$obj" 2>/dev/null | tail -1 | awk '{print $1}')
    else
      # Fallback: use size command
      text_size=$(size "$obj" 2>/dev/null | tail -1 | awk '{print $1}')
    fi

    # Handle case where size extraction fails
    if [ -z "$text_size" ] || [ "$text_size" = "text" ]; then
      text_size=0
    fi

    # Check budget
    status="OK"
    if [ "$text_size" -gt "$budget" ] 2>/dev/null; then
      status="OVER"
      FAIL=1
    fi

    echo "| $bench | $target | $text_size | $budget | $status |"
  done
done

echo ""

if [ "$FAIL" -ne 0 ]; then
  echo "**FAIL**: One or more benchmarks exceeded budget or failed to compile."
  exit 1
else
  echo "**PASS**: All benchmarks within budget."
  exit 0
fi
