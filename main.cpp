#include "tinyswift/Lexer/Lexer.h"
#include "llvm/Support/raw_ostream.h"

void runTest() {
  const char *Source = "aaa \t\0 bbb ccc";

  tinyswift::LangOptions LangOpts;
  tinyswift::SourceManager SourceMgr;

  unsigned BufferID = SourceMgr.addMemBufferCopy(llvm::StringRef(Source, 14));

  tinyswift::Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr,
                     /*InSILMode=*/false);

  tinyswift::Token Tok;

  L.lex(Tok);
  assert(Tok.getKind() == tinyswift::tok::identifier);
  assert(Tok.getText() == "aaa");
  assert(Tok.isAtStartOfLine());

  L.lex(Tok);
  assert(Tok.getKind() == tinyswift::tok::identifier);
  assert(Tok.getText() == "bbb");
  assert(!Tok.isAtStartOfLine());

  tinyswift::Lexer::State S = L.getStateForBeginningOfToken(Tok);

  L.lex(Tok);
  assert(Tok.getKind() == tinyswift::tok::identifier);
  assert(Tok.getText() == "ccc");
  assert(!Tok.isAtStartOfLine());

  L.lex(Tok);
  assert(Tok.getKind() == tinyswift::tok::eof);

  L.restoreState(S);

  L.lex(Tok);
  assert(Tok.getKind() == tinyswift::tok::identifier);
  assert(Tok.getText() == "bbb");
  assert(!Tok.isAtStartOfLine());

  L.lex(Tok);
  assert(Tok.getKind() == tinyswift::tok::identifier);
  assert(Tok.getText() == "ccc");
  assert(!Tok.isAtStartOfLine());

  L.lex(Tok);
  assert(Tok.getKind() == tinyswift::tok::eof);
}

void lex(llvm::StringRef Buffer) {
  tinyswift::LangOptions LangOpts;
  tinyswift::SourceManager SourceMgr;

  unsigned BufferID = SourceMgr.addMemBufferCopy(Buffer);

  tinyswift::Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr,
                     /*InSILMode=*/false);
  tinyswift::Token T;

  L.lex(T);
  llvm::raw_ostream &output = llvm::outs();
  while (T.isNot(tinyswift::tok::eof)) {
    L.lex(T);
    if (T.getKind() == tinyswift::tok::eof) {
      break;
    }
    output << T.getText() << "\n";
  }
}

int main() {
  tinyswift::Token T;
  T.setKind(tinyswift::tok::identifier);
  assert(T.is(tinyswift::tok::identifier));

  runTest();

  // Test Lexer
  llvm::StringRef Buffer = "var one: Int = 1;";
  lex(Buffer);

  return 0;
}