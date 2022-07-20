add_library(tinyswiftSource STATIC SourceLoc.cpp)
target_include_directories(tinyswiftSource PUBLIC ../../include)
target_link_libraries(tinyswiftSource ${llvm_libs})