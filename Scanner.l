%{
    #include "Grammar.tab.h"
    #define bufferSize 100

    int check(int token);
    void writeString(const char* value);    
    void writeName(unsigned len);
    void writeDigit();
    
    FILE* lexOutput = NULL;  
%}
%array
%%

\"[^"]*\"                       { fprintf(lexOutput, "\"string\""); /*printf("#### string.\n"); */ writeName(5); return check(STRING); }

"package"                       { fprintf(lexOutput, "package"); /* printf("#### package.\n");*/         return check(PACKAGE); }
"interface"                     { fprintf(lexOutput, "interface"); /* printf("#### interface.\n");*/       return check(INTERFACE); }
"map"                           { fprintf(lexOutput, "map"); /* printf("#### map.\n");*/             return check(MAP); }
"goto"                          { fprintf(lexOutput, "goto"); /* printf("#### goto.\n");*/            return check(GOTO); }
"defer"                         { fprintf(lexOutput, "defer"); /* printf("#### defer.\n");*/           return check(DEFER); }
"chan"                          { fprintf(lexOutput, "chan"); /* printf("#### chan.\n");*/            return check(CHAN); }
"import"                        { fprintf(lexOutput, "import"); /* printf("#### import.\n");*/          return check(IMPORT); }
"func"                          { fprintf(lexOutput, "func"); /* printf("#### func.\n");*/            return check(FUNC); }
"case"                          { fprintf(lexOutput, "case"); /* printf("#### case.\n");*/            return check(CASE); }
"const"                         { fprintf(lexOutput, "const"); /* printf("#### const.\n");*/           return check(CONST); }
"default"                       { fprintf(lexOutput, "default"); /* printf("#### default.\n");*/         return check(DEFAULT); }
"else"                          { fprintf(lexOutput, "else"); /* printf("#### else.\n");*/            return check(ELSE); }
"for"                           { fprintf(lexOutput, "for"); /* printf("#### for.\n");*/             return check(FOR); }
"go"                            { fprintf(lexOutput, "go"); /* printf("#### go.\n");*/              return check(GO); }
"if"                            { fprintf(lexOutput, "if"); /* printf("#### if.\n");*/              return check(IF); }
"range"                         { fprintf(lexOutput, "range"); /* printf("#### range.\n");*/           return check(RANGE); }
"struct"                        { fprintf(lexOutput, "struct"); /* printf("#### struct.\n");*/          return check(STRUCT); }
"switch"                        { fprintf(lexOutput, "switch"); /* printf("#### switch.\n");*/          return check(SWITCH); }
"type"                          { fprintf(lexOutput, "type"); /* printf("#### type.\n");*/            return check(TYPE); }
"var"                           { fprintf(lexOutput, "var"); /* printf("#### var.\n");*/             return check(VAR); }
"<-"                            { fprintf(lexOutput, "<-"); /* printf("#### arrow left.\n");*/      return check(ARROW_LEFT); }
":="                            { fprintf(lexOutput, ":="); /* printf("#### assign.\n");*/          return check(ASSIGN); }
"||"                            { fprintf(lexOutput, "||"); /* printf("#### or.\n");*/              return check(OR); }
"&&"                            { fprintf(lexOutput, "&&"); /* printf("#### and.\n");*/             return check(AND); }
"=="                            { fprintf(lexOutput, "=="); /* printf("#### equall.\n");*/          return check(EQUALL); }
"!="                            { fprintf(lexOutput, "!="); /* printf("#### not equall.\n");*/      return check(NOT_EQUALL); }
">="                            { fprintf(lexOutput, ">="); /* printf("#### greater equall.\n");*/  return check(GREATER_EQUALL); }
"<="                            { fprintf(lexOutput, "<="); /* printf("#### less equall.\n");*/     return check(LESS_EQUALL); }
"<<"                            { fprintf(lexOutput, "<<"); /* printf("#### shift left.\n");*/      return check(SHIFT_LEFT); }
">>"                            { fprintf(lexOutput, ">>"); /* printf("#### shift right.\n");*/     return check(SHIFT_RIGHT); }
"&^"                            { fprintf(lexOutput, "&^"); /* printf("#### andxor.\n");*/         return check(AND_XOR); }
"..."                           { fprintf(lexOutput, "..."); /* printf("#### multidot.\n");*/        return check(MULTIDOT); } 
                          
"nil"                           { fprintf(lexOutput, "NIL"); /*printf("#### nil.\n"); */ writeName(5); return check(IDENTIFIER); }                                 
"true"                          { fprintf(lexOutput, "true"); /*printf("#### bool value.\n") */; writeName(5); return check(IDENTIFIER); }
"false"                         { fprintf(lexOutput, "true"); /*printf("#### bool value.\n") */; writeName(5); return check(IDENTIFIER); }
"append"                        { fprintf(lexOutput, "append"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"cap"                           { fprintf(lexOutput, "cap"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"close"                         { fprintf(lexOutput, "close"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"complex"                       { fprintf(lexOutput, "complex"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"copy"                          { fprintf(lexOutput, "copy"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"delete"                        { fprintf(lexOutput, "delete"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"imag"                          { fprintf(lexOutput, "imag"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"len"                           { fprintf(lexOutput, "len"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"make"                          { fprintf(lexOutput, "make"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"new"                           { fprintf(lexOutput, "new"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"panic"                         { fprintf(lexOutput, "panic"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"print"                         { fprintf(lexOutput, "print"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"println"                       { fprintf(lexOutput, "println"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"real"                          { fprintf(lexOutput, "real"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
"recover"                       { fprintf(lexOutput, "recover"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
bool                            { fprintf(lexOutput, "bool"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
byte                            { fprintf(lexOutput, "byte"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
error                           { fprintf(lexOutput, "error"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
float32                         { fprintf(lexOutput, "float32"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
float64                         { fprintf(lexOutput, "float64"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
int                             { fprintf(lexOutput, "int"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
int8                            { fprintf(lexOutput, "int8"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
int16                           { fprintf(lexOutput, "int16"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
int32                           { fprintf(lexOutput, "int32"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
int64                           { fprintf(lexOutput, "int64"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
rune                            { fprintf(lexOutput, "rune"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
string                          { fprintf(lexOutput, "string"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
uint                            { fprintf(lexOutput, "uint"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
uint8                           { fprintf(lexOutput, "uint8"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
uint16                          { fprintf(lexOutput, "uint16"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
uint32                          { fprintf(lexOutput, "uint32"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
uint64                          { fprintf(lexOutput, "uint64"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
uintptr                         { fprintf(lexOutput, "uintptr"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
complex64                       { fprintf(lexOutput, "complex64"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
complex12                       { fprintf(lexOutput, "complex12"); /*printf("#### .\n"); */ writeName(5); return check(IDENTIFIER); }
        
"break"                         { fprintf(lexOutput, "break"); /*printf("#### break.\n"); */ writeString("break"); return check(BREAK); }                     
"continue"                      { fprintf(lexOutput, "continue"); /*printf("#### cont.\n"); */ writeString("continue"); return check(CONTINUE); }
"fallthrough"                   { fprintf(lexOutput, "fallthrough"); /*printf("#### fall.\n"); */ writeString("fallthrough"); return check(FALLTHROUGH); }
"return"                        { fprintf(lexOutput, "return"); /*printf("#### ret.\n"); */ writeString("return"); return check(RETURN); }
"++"                            { fprintf(lexOutput, "++"); /*printf("#### ++.\n"); */ writeString("++"); return check(INCREMENT); }
"--"                            { fprintf(lexOutput, "--"); /*printf("#### --.\n"); */ writeString("--"); return check(DECREMENT); }

[0-9]*([0-9]\.?|\.[0-9])[0-9]*([Ee][-+]?[0-9]+)                                 { fprintf(lexOutput, "FLOAT"); /*printf("#### float.\n"); */ writeName(5); return check(FLOAT); }
0*([12]?[0-9]{1,4}|3(2(7(6[0-7]|[0-5][0-9])|[0-6][0-9]{2})|[0-1][0-9]{3}))      { fprintf(lexOutput, "INTEGER"); /*printf("#### int.\n"); */ writeString("int"); return check(INTEGER); }
[a-zA-Z_][a-zA-Z0-9_\.]*        { fprintf(lexOutput, "identifier"); /*printf("#### int.\n"); */ writeString("int"); return check(IDENTIFIER); }

"["                             { fprintf(lexOutput, "["); /*printf("Founded %c.\n", *yytext);*/ return check('['); }
"("                             { fprintf(lexOutput, "("); /*printf("Founded %c.\n", *yytext);*/ return check('('); }
"{"                             { fprintf(lexOutput, "{"); /*printf("Founded %c.\n", *yytext);*/ return check('{'); }
        
"]"                             { fprintf(lexOutput, "]"); /*printf("Founded %c.\n", *yytext);*/ return check(']'); }
")"                             { fprintf(lexOutput, ")"); /*printf("Founded %c.\n", *yytext);*/ return check(')'); }
"}"                             { fprintf(lexOutput, "}"); /*printf("Founded %c.\n", *yytext);*/ return check('}'); }
        
";"                             { fprintf(lexOutput, ";"); return check(';'); }   

"\r"                            {}
"\n"                            { int token = check('\n'); if(token != '\n') return token; }
                                         
"\t"                            { fprintf(lexOutput, "\t"); /* printf("%c", *yytext); */ }
" "                             { fprintf(lexOutput, " "); /* printf("%c", *yytext); */ }
[^a-zA-Z0-9_]                   { fprintf(lexOutput, "%c", *yytext); /* printf("Unexp c %c.\n", *yytext); */ return check(*yytext); };
%%

int check(int token) {
    static unsigned insert = 0;
    
    /* Если нашли перевод строки и флаг выставлен, то возвращаем ; и опускаем флаг. */
    if(token == '\n') {
        if(insert == 1) {
            fprintf(lexOutput, ";\n");
            insert = 0;
            return ';';
        } else {
            /* Иначе возвращаем перевод строки. */
            fprintf(lexOutput, "\n");
            return '\n';
        }
    }
    
    /* Если находим один из токенов, для которых нужно 
        добавлять ; в конце линии, поднимаем флаг. */
    if(token == IDENTIFIER || token == INTEGER || token == FLOAT || 
        token == IMAGINARY || token == RUNE || token == STRING || 
        token == BREAK || token == CONTINUE || token == FALLTHROUGH || 
        token == RETURN || token == INCREMENT || token == DECREMENT || 
        token == ')' || token == ']' || token == '}') {
        insert = 1;
    } else {
    /* В случае обычного токена, опускаем флаг. */
        insert = 0;
    }
    return token;
}

void writeString(const char* value) {
    yylval.digit = 0;
    const unsigned len = strlen(value);
    const unsigned writeLen = bufferSize > len ? len : bufferSize;
    strncpy(yylval.buffer, value, writeLen);
    yylval.buffer[writeLen - 1] = 0;
}

void writeName(unsigned len) {
    yylval.digit = 0;
    const int writeLen = bufferSize > len ? len : bufferSize;
    strncpy(yylval.buffer, yytext, writeLen);
    yylval.buffer[writeLen - 1] = 0;
}

void writeDigit() {
    memset(yylval.buffer, 0, bufferSize);
    yylval.digit = *yytext - '0'; 
}