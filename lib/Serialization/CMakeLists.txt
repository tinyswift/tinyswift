
add_swift_host_library(swiftSerialization STATIC
  Deserialization.cpp
  DeserializeSIL.cpp
  ModuleFile.cpp
  ModuleFileSharedCore.cpp
  ScanningLoaders.cpp
  Serialization.cpp
  SerializedModuleLoader.cpp
  SerializedSILLoader.cpp
  SerializeDoc.cpp
  SerializeSIL.cpp

  LLVM_LINK_COMPONENTS
    BitstreamReader
  )
target_link_libraries(swiftSerialization PRIVATE
  swiftOption
  swiftAST
  swiftSIL
  clangIndex)
if(NOT TINYSWIFT_BUILD)
  target_link_libraries(swiftSerialization PRIVATE
    swiftClangImporter)
endif()

set_swift_llvm_is_available(swiftSerialization)
