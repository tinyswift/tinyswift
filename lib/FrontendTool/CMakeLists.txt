
add_swift_host_library(swiftFrontendTool STATIC
  FrontendTool.cpp
  ImportedModules.cpp
  LoadedModuleTrace.cpp
  TBD.cpp)
add_dependencies(swiftFrontendTool
  SwiftOptions)
target_link_libraries(swiftFrontendTool INTERFACE
  clangAPINotes
  clangBasic)
target_link_libraries(swiftFrontendTool PRIVATE
    swiftClangImporter
    swiftDemangling
    swiftDependencyScan
    swiftFrontend
    swiftIRGen
    swiftOption
    swiftSerialization
    swiftSIL
    swiftSILGen
    swiftSILOptimizer
    swiftThreading)
if(NOT TINYSWIFT_BUILD)
  target_link_libraries(swiftFrontendTool PRIVATE
    swiftConstExtract
    swiftImmediate
    swiftIndex
    swiftMigrator
    swiftPrintAsClang)
endif()

set_swift_llvm_is_available(swiftFrontendTool)
