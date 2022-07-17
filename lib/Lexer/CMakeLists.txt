add_library(tinyswiftLexer STATIC Token.cpp Lexer.cpp)
target_include_directories(tinyswiftLexer PUBLIC ../../include)
target_link_libraries(tinyswiftLexer ${llvm_libs})