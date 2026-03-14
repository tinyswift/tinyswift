#!/usr/bin/env bash
#
# package-toolchain.sh — Package TinySwift build artifacts into a distributable tarball.
#
# Usage:
#   ./scripts/package-toolchain.sh [VERSION] [PLATFORM]
#
# Arguments:
#   VERSION   — Release version string (default: derived from git tag, e.g. v0.1.0-alpha.1 -> 0.1.0-alpha.1)
#   PLATFORM  — Platform identifier (default: derived from uname, e.g. macos-arm64 or linux-x86_64)
#
# Output:
#   tinyswift-<VERSION>-<PLATFORM>.tar.xz in the current directory
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# ---------------------------------------------------------------------------
# Resolve VERSION
# ---------------------------------------------------------------------------
if [[ -n "${1:-}" ]]; then
    VERSION="$1"
else
    GIT_TAG="$(git -C "$REPO_ROOT" describe --tags --exact-match 2>/dev/null || true)"
    if [[ -z "$GIT_TAG" ]]; then
        echo "Error: no VERSION argument and no git tag on HEAD." >&2
        exit 1
    fi
    VERSION="${GIT_TAG#v}"
fi

# ---------------------------------------------------------------------------
# Resolve PLATFORM
# ---------------------------------------------------------------------------
if [[ -n "${2:-}" ]]; then
    PLATFORM="$2"
else
    OS="$(uname -s)"
    ARCH="$(uname -m)"
    case "$OS" in
        Darwin) OS_LABEL="macos" ;;
        Linux)  OS_LABEL="linux" ;;
        *)      OS_LABEL="$(echo "$OS" | tr '[:upper:]' '[:lower:]')" ;;
    esac
    case "$ARCH" in
        x86_64)  ARCH_LABEL="x86_64" ;;
        aarch64) ARCH_LABEL="arm64"  ;;
        arm64)   ARCH_LABEL="arm64"  ;;
        *)       ARCH_LABEL="$ARCH"  ;;
    esac
    PLATFORM="${OS_LABEL}-${ARCH_LABEL}"
fi

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
PACKAGE_NAME="tinyswift-${VERSION}-${PLATFORM}"
STAGING_DIR="$(mktemp -d)"
DEST="${STAGING_DIR}/${PACKAGE_NAME}"
BUILD_DIR="${REPO_ROOT}/build/tinyswift-release"

echo "=== Packaging ${PACKAGE_NAME} ==="
echo "  Build dir:   ${BUILD_DIR}"
echo "  Staging dir: ${DEST}"

# ---------------------------------------------------------------------------
# Verify build directory exists
# ---------------------------------------------------------------------------
if [[ ! -d "$BUILD_DIR/bin" ]]; then
    echo "Error: build directory not found at ${BUILD_DIR}/bin" >&2
    echo "Run 'cmake --build --preset tinyswift-release' first." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Create directory layout
# ---------------------------------------------------------------------------
mkdir -p "${DEST}/bin"
mkdir -p "${DEST}/lib/swift/tinyswift"
mkdir -p "${DEST}/share/tinyswift/linker-scripts/aarch64-qemu-virt"
mkdir -p "${DEST}/share/tinyswift/linker-scripts/riscv32-qemu-virt"
mkdir -p "${DEST}/share/tinyswift/examples"

# ---------------------------------------------------------------------------
# bin/
# ---------------------------------------------------------------------------
# Copy the compiler driver and frontend
for BIN in tinyswiftc swift-frontend; do
    if [[ -f "${BUILD_DIR}/bin/${BIN}" ]]; then
        cp "${BUILD_DIR}/bin/${BIN}" "${DEST}/bin/"
        echo "  + bin/${BIN}"
    else
        echo "  Warning: ${BIN} not found in build, skipping." >&2
    fi
done

# ---------------------------------------------------------------------------
# lib/swift/tinyswift/  (Builtins.swiftmodule when available)
# ---------------------------------------------------------------------------
if [[ -d "${BUILD_DIR}/lib/swift/tinyswift" ]]; then
    cp -R "${BUILD_DIR}/lib/swift/tinyswift/." "${DEST}/lib/swift/tinyswift/"
    echo "  + lib/swift/tinyswift/ (modules)"
else
    echo "  (lib/swift/tinyswift/ left empty — no built modules yet)"
fi

# ---------------------------------------------------------------------------
# share/tinyswift/linker-scripts/
# ---------------------------------------------------------------------------
SUPPORT_DIR="${REPO_ROOT}/utils/embedded-test-support"

if [[ -f "${SUPPORT_DIR}/aarch64-qemu-virt/linkerscript.ld" ]]; then
    cp "${SUPPORT_DIR}/aarch64-qemu-virt/linkerscript.ld" \
       "${DEST}/share/tinyswift/linker-scripts/aarch64-qemu-virt/"
    echo "  + share/tinyswift/linker-scripts/aarch64-qemu-virt/linkerscript.ld"
fi

if [[ -f "${SUPPORT_DIR}/riscv32-qemu-virt/linkerscript.ld" ]]; then
    cp "${SUPPORT_DIR}/riscv32-qemu-virt/linkerscript.ld" \
       "${DEST}/share/tinyswift/linker-scripts/riscv32-qemu-virt/"
    echo "  + share/tinyswift/linker-scripts/riscv32-qemu-virt/linkerscript.ld"
fi

# ---------------------------------------------------------------------------
# share/tinyswift/examples/
# ---------------------------------------------------------------------------
EXAMPLES_DIR="${REPO_ROOT}/examples"
if [[ -d "$EXAMPLES_DIR" ]]; then
    cp -R "${EXAMPLES_DIR}/." "${DEST}/share/tinyswift/examples/"
    echo "  + share/tinyswift/examples/ (copied from examples/)"
fi

# ---------------------------------------------------------------------------
# Strip binaries (best-effort)
# ---------------------------------------------------------------------------
if command -v strip &>/dev/null; then
    for BIN_FILE in "${DEST}/bin/"*; do
        if [[ -f "$BIN_FILE" && -x "$BIN_FILE" ]]; then
            strip "$BIN_FILE" 2>/dev/null || true
        fi
    done
    echo "  (binaries stripped)"
fi

# ---------------------------------------------------------------------------
# Create tar.xz archive
# ---------------------------------------------------------------------------
OUTPUT_FILE="${REPO_ROOT}/${PACKAGE_NAME}.tar.xz"

tar -C "$STAGING_DIR" -cJf "$OUTPUT_FILE" "$PACKAGE_NAME"

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
rm -rf "$STAGING_DIR"

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
SIZE="$(du -h "$OUTPUT_FILE" | cut -f1)"
echo ""
echo "=== Done ==="
echo "  Archive: ${OUTPUT_FILE}"
echo "  Size:    ${SIZE}"

echo "$OUTPUT_FILE"
