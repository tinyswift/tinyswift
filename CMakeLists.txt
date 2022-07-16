cmake_minimum_required(VERSION 3.22)
project(tinyswift)

set(CMAKE_CXX_STANDARD 20)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
#llvm_map_components_to_libnames(llvm_libs support core irreader ScalarOpts
#        ExecutionEngine OrcJIT native Analysis RuntimeDyld Object InstCombine mcjit)
llvm_map_components_to_libnames(llvm_libs support)

add_subdirectory(lib)

add_executable(tinyswift main.cpp)
target_link_libraries(tinyswift tinyswiftParser)
