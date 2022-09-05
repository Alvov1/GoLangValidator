flex Scanner.l
bison -d Grammar.y
gcc Grammar.tab.c Grammar.tab.h lex.yy.c