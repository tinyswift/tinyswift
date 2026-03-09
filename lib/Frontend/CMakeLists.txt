
add_swift_host_library(swiftFrontend STATIC
  ArgsToFrontendInputsConverter.cpp
  ArgsToFrontendOptionsConverter.cpp
  ArgsToFrontendOutputsConverter.cpp
  CachedDiagnostics.cpp
  CachingUtils.cpp
  CASOutputBackends.cpp
  CompileJobCacheKey.cpp
  CompileJobCacheResult.cpp
  CompilerInvocation.cpp
  DependencyVerifier.cpp
  DiagnosticHelper.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  FrontendInputsAndOutputs.cpp
  FrontendOptions.cpp
  MakeStyleDependencies.cpp
  ModuleInterfaceBuilder.cpp
  ModuleInterfaceLoader.cpp
  ModuleInterfaceSupport.cpp
  PrintingDiagnosticConsumer.cpp
  Serialization.cpp
  SerializedDiagnosticConsumer.cpp)
add_dependencies(swiftFrontend
  SwiftOptions)
target_link_libraries(swiftFrontend PRIVATE
  swiftAST
  swiftSIL
  swiftOption
  swiftSILGen
  swiftSILOptimizer
  swiftIRGen
  swiftSema
  swiftSerialization)
if(NOT TINYSWIFT_BUILD)
  target_link_libraries(swiftFrontend PRIVATE
    swiftAPIDigester
    swiftConstExtract
    swiftIDE
    swiftLocalization
    swiftSymbolGraphGen)
endif()

if (SWIFT_BUILD_SWIFT_SYNTAX)
  target_link_libraries(swiftFrontend
    PRIVATE
    swiftASTGen
  )

  add_dependencies(swiftFrontend
    swiftASTGen
  )
endif()

set_swift_llvm_is_available(swiftFrontend)
