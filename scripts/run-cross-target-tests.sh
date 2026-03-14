#!/usr/bin/env bash
#
# run-cross-target-tests.sh
#
# Compiles every valid and ported TinySwift test file with -emit-ir
# against multiple embedded target triples to verify cross-target
# code generation. Exits non-zero if any compilation fails.
#

set -euo pipefail

SWIFT_FRONTEND="${SWIFT_FRONTEND:-build/tinyswift-debug/bin/swift-frontend}"

TARGETS=(
  aarch64-none-elf
  riscv32-none-none-eabi
  riscv64-none-none-eabi
  wasm32-unknown-wasi
)

PASS=0
FAIL=0
FAILURES=()

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SRC_FILES=()
for f in "$REPO_ROOT"/test/TinySwift/valid/*.swift "$REPO_ROOT"/test/TinySwift/ported/*.swift; do
  [ -f "$f" ] && SRC_FILES+=("$f")
done

if [ ${#SRC_FILES[@]} -eq 0 ]; then
  echo "No test files found."
  exit 1
fi

echo "=== Cross-Target IR Tests ==="
echo "Compiler : $SWIFT_FRONTEND"
echo "Targets  : ${TARGETS[*]}"
echo "Files    : ${#SRC_FILES[@]}"
echo ""

for SRC_FILE in "${SRC_FILES[@]}"; do
  for TARGET in "${TARGETS[@]}"; do
    REL_PATH="${SRC_FILE#"$REPO_ROOT"/}"
    echo -n "  $REL_PATH ($TARGET) ... "

    if "$SWIFT_FRONTEND" -emit-ir -target "$TARGET" \
         -enable-experimental-feature Embedded \
         -enable-experimental-feature TinySwift \
         -disable-objc-interop -parse-stdlib -parse-as-library -Osize \
         -module-name test_module \
         "$SRC_FILE" -o /dev/null 2>/dev/null; then
      echo "PASS"
      PASS=$((PASS + 1))
    else
      echo "FAIL"
      FAIL=$((FAIL + 1))
      FAILURES+=("$REL_PATH ($TARGET)")
    fi
  done
done

echo ""
echo "=== Summary ==="
echo "  Total : $((PASS + FAIL))"
echo "  Pass  : $PASS"
echo "  Fail  : $FAIL"

if [ $FAIL -ne 0 ]; then
  echo ""
  echo "Failed tests:"
  for entry in "${FAILURES[@]}"; do
    echo "  - $entry"
  done
  exit 1
fi

echo ""
echo "All cross-target tests passed."
exit 0
