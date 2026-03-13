# stdlib/public/TinySwift/CMakeLists.txt
#
# Build the TinySwift standard library (Builtins.swift + Runtime.c).
#
# Builtins.swift is compiled with stock swiftc using -parse-stdlib to produce
# a .swiftmodule. Runtime.c is compiled with clang in freestanding mode.
# Together they form the entire TinySwift "standard library".

# --------------------------------------------------------------------------
# Builtins.swift → TinySwift.swiftmodule + TinySwift.o
# --------------------------------------------------------------------------

# Find the host swiftc (used to bootstrap the builtins module).
find_program(SWIFTC_EXECUTABLE swiftc
  HINTS "${CMAKE_Swift_COMPILER}"
        "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}"
  DOC "Path to swiftc for bootstrapping TinySwift builtins"
)

if(NOT SWIFTC_EXECUTABLE)
  message(WARNING "swiftc not found; TinySwift builtins module will not be built")
  return()
endif()

set(TINYSWIFT_BUILTINS_SRC "${CMAKE_CURRENT_SOURCE_DIR}/Builtins.swift")
set(TINYSWIFT_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}")
set(TINYSWIFT_MODULE_PATH "${TINYSWIFT_MODULE_DIR}/TinySwift.swiftmodule")
set(TINYSWIFT_OBJECT_PATH "${TINYSWIFT_MODULE_DIR}/TinySwift.o")

add_custom_command(
  OUTPUT "${TINYSWIFT_MODULE_PATH}" "${TINYSWIFT_OBJECT_PATH}"
  COMMAND "${SWIFTC_EXECUTABLE}"
    -parse-stdlib -parse-as-library
    -enable-experimental-feature Embedded
    -module-name TinySwift
    -emit-module -emit-module-path "${TINYSWIFT_MODULE_PATH}"
    -emit-object -o "${TINYSWIFT_OBJECT_PATH}"
    -Xfrontend -disable-objc-interop
    "${TINYSWIFT_BUILTINS_SRC}"
  DEPENDS "${TINYSWIFT_BUILTINS_SRC}"
  COMMENT "Bootstrapping TinySwift builtins module"
  VERBATIM
)

add_custom_target(tinyswift-builtins-module ALL
  DEPENDS "${TINYSWIFT_MODULE_PATH}" "${TINYSWIFT_OBJECT_PATH}"
)

# --------------------------------------------------------------------------
# Runtime.c → TinySwiftRuntime.o
# --------------------------------------------------------------------------

set(TINYSWIFT_RUNTIME_SRC "${CMAKE_CURRENT_SOURCE_DIR}/Runtime.c")

add_library(TinySwiftRuntime OBJECT "${TINYSWIFT_RUNTIME_SRC}")
target_compile_options(TinySwiftRuntime PRIVATE
  -ffreestanding
  -nostdlib
)
target_compile_definitions(TinySwiftRuntime PRIVATE
  TINYSWIFT_RUNTIME=1
)

# --------------------------------------------------------------------------
# Install both the module and runtime object into the resource directory
# so that tinyswiftc can find them.
# --------------------------------------------------------------------------

install(FILES "${TINYSWIFT_MODULE_PATH}"
  DESTINATION "lib/swift/tinyswift"
  COMPONENT tinyswift-stdlib
)

install(FILES "${TINYSWIFT_OBJECT_PATH}"
  DESTINATION "lib/swift/tinyswift"
  COMPONENT tinyswift-stdlib
)

install(TARGETS TinySwiftRuntime
  DESTINATION "lib/swift/tinyswift"
  COMPONENT tinyswift-stdlib
)
