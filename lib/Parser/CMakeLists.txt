add_library(tinyswiftParser STATIC Token.cpp)
#target_include_directories(tinyswiftParser PUBLIC ../../include)
target_link_libraries(tinyswiftParser ${llvm_libs})