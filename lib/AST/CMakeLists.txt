add_library(tinyswiftAST STATIC Identifier.cpp)
target_include_directories(tinyswiftAST PUBLIC ../../include)
target_link_libraries(tinyswiftAST ${llvm_libs} tinyswiftSource)