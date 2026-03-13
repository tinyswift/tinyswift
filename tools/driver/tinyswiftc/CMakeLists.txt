# TinySwift embedded compiler driver
#
# A standalone executable that wraps swift-frontend with
# embedded-first defaults. Only depends on LLVMSupport.

add_executable(tinyswiftc tinyswiftc_main.cpp)

# LLVM include directories are set by find_package(LLVM) in the parent project
target_include_directories(tinyswiftc PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(tinyswiftc PRIVATE LLVMSupport)

# Place tinyswiftc next to swift-frontend in the bin directory
set_target_properties(tinyswiftc PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

install(TARGETS tinyswiftc DESTINATION bin)
