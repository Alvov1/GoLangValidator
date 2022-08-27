bison -d GrammarTest.y
flex Scanner.l
gcc GrammarTest.tab.c GrammarTest.tab.h lex.yy.c