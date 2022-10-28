%{
    #include "Grammar.tab.h"

    int check(int token); 
    void writeName();
    
    FILE* parsed = NULL;
    unsigned fileHeight = 0;
%}
%array
%%

"//".*                                  {}
[/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]     {}
[/][*]                                  { printf("Unterminated comment"); exit(0); }

\"[^"]*\"                               { fprintf(parsed, "\"string\""); writeName(); return check(STRING); }
\'[^']*\'                               { fprintf(parsed, "'rune'"); writeName(); return check(RUNE); }
`[^`]*`                                 { fprintf(parsed, "`rune`"); writeName(); return check(RUNE); }

([0-9]+)[\.]                            { fprintf(parsed, "%s", yytext); writeName(); return check(FLOAT); }
[\.]([0-9]+)                            { fprintf(parsed, "%s", yytext); writeName(); return check(FLOAT); }
[\.]([eE][+-]?[0-9]+)                   { fprintf(parsed, "%s", yytext); writeName(); return check(FLOAT); }
([0-9]+)[\.]([0-9]+)                    { fprintf(parsed, "%s", yytext); writeName(); return check(FLOAT); }
[\.]([0-9]+)([eE][+-]?[0-9]+)           { fprintf(parsed, "%s", yytext); writeName(); return check(FLOAT); }
([0-9]+)[\.]([eE][+-]?[0-9]+)           { fprintf(parsed, "%s", yytext); writeName(); return check(FLOAT); }
([0-9]+)[\.]([0-9]+)([eE][+-]?[0-9]+)   { fprintf(parsed, "%s", yytext); writeName(); return check(FLOAT); }

[0-9]+([eE][+-]?[0-9]+)?        { fprintf(parsed, "%s", yytext); writeName(); return check(INTEGER); }
0x[0-9a-fA-F]+                  { fprintf(parsed, "%s", yytext); writeName(); return check(INTEGER); }
0o[0-7]+                        { fprintf(parsed, "%s", yytext); writeName(); return check(INTEGER); }
0b[0-1]+                        { fprintf(parsed, "%s", yytext); writeName(); return check(INTEGER); }

"package"                       { fprintf(parsed, "package"); return check(PACKAGE); }
"interface"                     { fprintf(parsed, "interface"); return check(INTERFACE); }
"map"                           { fprintf(parsed, "map"); return check(MAP); }
"goto"                          { fprintf(parsed, "goto"); return check(GOTO); }
"defer"                         { fprintf(parsed, "defer"); return check(DEFER); }
"chan"                          { fprintf(parsed, "chan"); return check(CHAN); }
"import"                        { fprintf(parsed, "import"); return check(IMPORT); }
"func"                          { fprintf(parsed, "func"); return check(FUNC); }
"case"                          { fprintf(parsed, "case"); return check(CASE); }
"const"                         { fprintf(parsed, "const"); return check(CONST); }
"default"                       { fprintf(parsed, "default"); return check(DEFAULT); }
"else"                          { fprintf(parsed, "else"); return check(ELSE); }
"for"                           { fprintf(parsed, "for"); return check(FOR); }
"go"                            { fprintf(parsed, "go"); return check(GO); }
"if"                            { fprintf(parsed, "if"); return check(IF); }
"range"                         { fprintf(parsed, "range"); return check(RANGE); }
"struct"                        { fprintf(parsed, "struct"); return check(STRUCT); }
"switch"                        { fprintf(parsed, "switch"); return check(SWITCH); }
"type"                          { fprintf(parsed, "type"); return check(TYPE); }
"var"                           { fprintf(parsed, "var"); return check(VAR); }
"<-"                            { fprintf(parsed, "<-"); return check(ARROW_LEFT); }
":="                            { fprintf(parsed, ":="); return check(ASSIGN); }
"||"                            { fprintf(parsed, "||"); return check(OR); }
"&&"                            { fprintf(parsed, "&&"); return check(AND); }
"=="                            { fprintf(parsed, "=="); return check(EQUALL); }
"!="                            { fprintf(parsed, "!="); return check(NOT_EQUALL); }
">="                            { fprintf(parsed, ">="); return check(GREATER_EQUALL); }
"<="                            { fprintf(parsed, "<="); return check(LESS_EQUALL); }
"<<"                            { fprintf(parsed, "<<"); return check(SHIFT_LEFT); }
">>"                            { fprintf(parsed, ">>"); return check(SHIFT_RIGHT); }
"&^"                            { fprintf(parsed, "&^"); return check(AND_XOR); }
"..."                           { fprintf(parsed, "..."); return check(MULTIDOT); } 

"+="                            { fprintf(parsed, "+="); return check(ASSIGN_OP); }
"-="                            { fprintf(parsed, "-="); return check(ASSIGN_OP); }
"|="                            { fprintf(parsed, "|="); return check(ASSIGN_OP); }
"^="                            { fprintf(parsed, "^="); return check(ASSIGN_OP); }
"*="                            { fprintf(parsed, "*="); return check(ASSIGN_OP); }
"/="                            { fprintf(parsed, "/="); return check(ASSIGN_OP); }
"%="                            { fprintf(parsed, "%%="); return check(ASSIGN_OP); }
"<<="                           { fprintf(parsed, "<<"); return check(ASSIGN_OP); }
">>="                           { fprintf(parsed, ">>"); return check(ASSIGN_OP); }
"&="                            { fprintf(parsed, "&="); return check(ASSIGN_OP); }
"&^="                           { fprintf(parsed, "&^="); return check(ASSIGN_OP); }
                     
"nil"                           { fprintf(parsed, "nil"); writeName(); return check(IDENTIFIER); }                                 
"true"                          { fprintf(parsed, "true"); writeName(); return check(IDENTIFIER); }
"false"                         { fprintf(parsed, "true"); writeName(); return check(IDENTIFIER); }
"append"                        { fprintf(parsed, "append"); writeName(); return check(IDENTIFIER); }
"cap"                           { fprintf(parsed, "cap"); writeName(); return check(IDENTIFIER); }
"close"                         { fprintf(parsed, "close"); writeName(); return check(IDENTIFIER); }
"complex"                       { fprintf(parsed, "complex"); writeName(); return check(IDENTIFIER); }
"copy"                          { fprintf(parsed, "copy"); writeName(); return check(IDENTIFIER); }
"delete"                        { fprintf(parsed, "delete"); writeName(); return check(IDENTIFIER); }
"imag"                          { fprintf(parsed, "imag"); writeName(); return check(IDENTIFIER); }
"len"                           { fprintf(parsed, "len"); writeName(); return check(IDENTIFIER); }
"make"                          { fprintf(parsed, "make"); writeName(); return check(IDENTIFIER); }
"new"                           { fprintf(parsed, "new"); writeName(); return check(IDENTIFIER); }
"panic"                         { fprintf(parsed, "panic"); writeName(); return check(IDENTIFIER); }
"print"                         { fprintf(parsed, "print"); writeName(); return check(IDENTIFIER); }
"println"                       { fprintf(parsed, "println"); writeName(); return check(IDENTIFIER); }
"real"                          { fprintf(parsed, "real"); writeName(); return check(IDENTIFIER); }
"recover"                       { fprintf(parsed, "recover"); writeName(); return check(IDENTIFIER); }
"bool"                          { fprintf(parsed, "bool"); writeName(); return check(IDENTIFIER); }
"byte"                          { fprintf(parsed, "byte"); writeName(); return check(IDENTIFIER); }
"error"                         { fprintf(parsed, "error"); writeName(); return check(IDENTIFIER); }
"float32"                       { fprintf(parsed, "float32"); writeName(); return check(IDENTIFIER); }
"float64"                       { fprintf(parsed, "float64"); writeName(); return check(IDENTIFIER); }
"int"                           { fprintf(parsed, "int"); writeName(); return check(IDENTIFIER); }
"int8"                          { fprintf(parsed, "int8"); writeName(); return check(IDENTIFIER); }
"int16"                         { fprintf(parsed, "int16"); writeName(); return check(IDENTIFIER); }
"int32"                         { fprintf(parsed, "int32"); writeName(); return check(IDENTIFIER); }
"int64"                         { fprintf(parsed, "int64"); writeName(); return check(IDENTIFIER); }
"rune"                          { fprintf(parsed, "rune"); writeName(); return check(IDENTIFIER); }
"string"                        { fprintf(parsed, "string"); writeName(); return check(IDENTIFIER); }
"uint"                          { fprintf(parsed, "uint"); writeName(); return check(IDENTIFIER); }
"uint8"                         { fprintf(parsed, "uint8"); writeName(); return check(IDENTIFIER); }
"uint16"                        { fprintf(parsed, "uint16"); writeName(); return check(IDENTIFIER); }
"uint32"                        { fprintf(parsed, "uint32"); writeName(); return check(IDENTIFIER); }
"uint64"                        { fprintf(parsed, "uint64"); writeName(); return check(IDENTIFIER); }
"uintptr"                       { fprintf(parsed, "uintptr"); writeName(); return check(IDENTIFIER); }
"complex64"                     { fprintf(parsed, "complex64"); writeName(); return check(IDENTIFIER); }
"complex12"                     { fprintf(parsed, "complex12"); writeName(); return check(IDENTIFIER); }
        
"break"                         { fprintf(parsed, "break"); writeName(); return check(BREAK); }                     
"continue"                      { fprintf(parsed, "continue"); writeName(); return check(CONTINUE); }
"fallthrough"                   { fprintf(parsed, "fallthrough"); writeName(); return check(FALLTHROUGH); }
"return"                        { fprintf(parsed, "return"); writeName(); return check(RETURN); }
"++"                            { fprintf(parsed, "++"); writeName(); return check(INCREMENT); }
"--"                            { fprintf(parsed, "--"); writeName(); return check(DECREMENT); }

[a-zA-Z_][a-zA-Z0-9_]*          { fprintf(parsed, "%s", yytext); writeName(); return check(IDENTIFIER); }

"["                             { fprintf(parsed, "["); return check('['); }
"("                             { fprintf(parsed, "("); return check('('); }
"{"                             { fprintf(parsed, "{"); return check('{'); }
        
"]"                             { fprintf(parsed, "]"); return check(']'); }
")"                             { fprintf(parsed, ")"); return check(')'); }
"}"                             { fprintf(parsed, "}"); return check('}'); }
        
";"                             { fprintf(parsed, ";"); return check(';'); }   

"\r"                            {}
"\n"                            { fileHeight++; int token = check('\n'); if(token != '\n') return token; }
                                         
"\t"                            { fprintf(parsed, "\t"); }
" "                             { fprintf(parsed, " "); }
[^a-zA-Z0-9_]                   { fprintf(parsed, "%c", *yytext); return check(*yytext); };
%%

int check(int token) {
    static unsigned insert = 0;
    
    /* Если нашли перевод строки и флаг выставлен, то возвращаем ; и опускаем флаг. */
    if(token == '\n') {
        if(insert == 1) {
            fprintf(parsed, ";\n");
            insert = 0;
            return ';';
        } else {
            /* Иначе возвращаем перевод строки. */
            fprintf(parsed, "\n");
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

void writeName(unsigned len) {
    sprintf(yylval.buffer, "%s", yytext);
}