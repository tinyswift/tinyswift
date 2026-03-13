// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib -parse-as-library %s | %FileCheck %s
// REQUIRES: swift_feature_TinySwift

// Test struct consumption patterns in TinySwift mode.
// In -parse-stdlib mode, ~Copyable is unavailable, so we verify
// ownership via SIL: no retain/release, clean OSSA form.

struct FileHandle {
  var fd: Builtin.Int64
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}11closeHandle
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
func closeHandle(_ h: FileHandle) -> Builtin.Int64 {
  return h.fd
}

func openHandle() -> FileHandle {
  return FileHandle(fd: Builtin.zeroInitializer())
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12transferFile
func transferFile(_ src: FileHandle) -> FileHandle {
  return FileHandle(fd: src.fd)
}
