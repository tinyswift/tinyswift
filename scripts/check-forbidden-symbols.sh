#!/bin/bash
# Check that a TinySwift object file contains no forbidden runtime symbols.
# Usage: check-forbidden-symbols.sh <object-file>
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <object-file>" >&2
  exit 1
fi

OBJ="$1"

FORBIDDEN="swift_retain swift_release swift_allocObject swift_deallocObject
  swift_initClassMetadata swift_getGenericMetadata swift_dynamicCast
  swift_bridgeObjectRetain swift_bridgeObjectRelease swift_allocBox
  swift_deallocBox objc_retain objc_release objc_msgSend"

FAIL=0
for sym in $FORBIDDEN; do
  if nm "$OBJ" 2>/dev/null | grep -q "$sym"; then
    echo "FAIL: Found forbidden symbol: $sym"
    FAIL=1
  fi
done

if [ "$FAIL" -eq 0 ]; then
  echo "PASS: No forbidden runtime symbols found"
else
  exit 1
fi
