
#include "tinyswift/Lexer/Token.h"

int main(){

    tinyswift::Token T;
    T.setKind(tinyswift::tok::identifier);
    assert(T.is(tinyswift::tok::identifier));
    return 0;
}