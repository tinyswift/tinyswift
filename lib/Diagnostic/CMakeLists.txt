add_library(tinyswiftDiagnostic STATIC DiagnosticEngine.cpp DiagnosticConsumer.cpp DiagnosticList.cpp)
target_include_directories(tinyswiftDiagnostic PUBLIC ../../include)
target_link_libraries(tinyswiftDiagnostic ${llvm_libs})