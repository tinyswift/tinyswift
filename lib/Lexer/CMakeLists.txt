add_library(tinyswiftLexer STATIC CharInfo.cpp Lexer.cpp)
target_include_directories(tinyswiftLexer PUBLIC ../../include)
target_link_libraries(tinyswiftLexer ${llvm_libs} tinyswiftDiagnostic tinyswiftSource)