mkdir Build
cd Build
lex ../Scanner.l
bison -d ../Grammar.y -v
gcc lex.yy.c Grammar.tab.c Grammar.tab.h
cd ..