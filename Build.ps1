bison -d Grammar.y
flex Scanner.l
gcc Grammar.tab.c Grammar.tab.h lex.yy.c