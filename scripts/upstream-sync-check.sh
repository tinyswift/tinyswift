#!/usr/bin/env bash
#
# upstream-sync-check.sh
#
# Compares the TinySwift fork against swiftlang/swift:swift-6.1-RELEASE
# and reports divergent files. Useful for tracking upstream drift and
# planning merge/rebase operations.
#
# Usage:
#   ./scripts/upstream-sync-check.sh [upstream-ref]
#
# Default upstream ref: upstream/swift-6.1-RELEASE
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

UPSTREAM_REF="${1:-upstream/swift-6.1-RELEASE}"

# Check if upstream remote exists
if ! git remote | grep -q upstream; then
  echo "Adding upstream remote..."
  git remote add upstream https://github.com/swiftlang/swift.git 2>/dev/null || true
fi

# Fetch upstream (only the specific branch)
echo "Fetching upstream reference: $UPSTREAM_REF..."
git fetch upstream swift-6.1-RELEASE --no-tags 2>/dev/null || {
  echo "Warning: Could not fetch upstream. Using local refs only."
}

# Check if the upstream ref exists locally
if ! git rev-parse --verify "$UPSTREAM_REF" >/dev/null 2>&1; then
  echo "Error: Reference '$UPSTREAM_REF' not found."
  echo "Available upstream refs:"
  git branch -r | grep upstream || echo "  (none)"
  exit 1
fi

echo ""
echo "=== TinySwift Fork Divergence Report ==="
echo "  Fork HEAD : $(git rev-parse --short HEAD)"
echo "  Upstream   : $(git rev-parse --short "$UPSTREAM_REF")"
echo ""

# Get list of changed files
CHANGED_FILES=$(git diff --name-only "$UPSTREAM_REF"...HEAD 2>/dev/null || \
                git diff --name-only "$UPSTREAM_REF" HEAD)

if [ -z "$CHANGED_FILES" ]; then
  echo "No divergent files found. Fork is in sync with upstream."
  exit 0
fi

# Categorize changes
ADDED=()
MODIFIED=()
DELETED=()

while IFS= read -r file; do
  if ! git show "$UPSTREAM_REF:$file" >/dev/null 2>&1; then
    ADDED+=("$file")
  elif ! git show "HEAD:$file" >/dev/null 2>&1; then
    DELETED+=("$file")
  else
    MODIFIED+=("$file")
  fi
done <<< "$CHANGED_FILES"

TOTAL=$(echo "$CHANGED_FILES" | wc -l | tr -d ' ')

echo "--- Files Added (${#ADDED[@]}) ---"
for f in "${ADDED[@]:-}"; do
  [ -n "$f" ] && echo "  + $f"
done

echo ""
echo "--- Files Modified (${#MODIFIED[@]}) ---"
for f in "${MODIFIED[@]:-}"; do
  [ -n "$f" ] && echo "  M $f"
done

echo ""
echo "--- Files Deleted (${#DELETED[@]}) ---"
for f in "${DELETED[@]:-}"; do
  [ -n "$f" ] && echo "  - $f"
done

echo ""
echo "=== Summary ==="
echo "  Total divergent files : $TOTAL"
echo "  Added                 : ${#ADDED[@]}"
echo "  Modified              : ${#MODIFIED[@]}"
echo "  Deleted               : ${#DELETED[@]}"

# Highlight TinySwift-specific files
echo ""
echo "--- TinySwift-Specific Files ---"
echo "$CHANGED_FILES" | grep -i "tinyswift\|TinySwift" || echo "  (none found by name)"

exit 0
