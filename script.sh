lex Scanner.l
bison -d Grammar.y
gcc lex.yy.c Grammar.tab.c Grammar.tab.h
./a.out < string.go