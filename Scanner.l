%{
#include "Grammar.tab.h"
%}

DIGIT           [0-9]
OCT             [0-7]
HEX             [0-9|a-f|A-F]

UNICODE_CHAR            [^\n]
OCT_BYTE                ["\\"]OCT{3}
HEX_BYTE                ["\\"]["x"]HEX{2}
LITTLE_U                ["\\"]["u"]HEX{4}
BIG_U                   ["\\"]["U"]HEX{8}
ESCAPED_CHAR            ["\\"][ "a" | "b" | "f" | "n" | "r" | "t" | "v" | "\\" | "'" | "\""]
BYTE                    OCT_BYTE | HEX_BYTE
UNICODE_VALUE           UNICODE_CHAR | LITTLE_U | BIG_U | ESCAPED_CHAR

VARIABLE_TYPE   "bool"|"byte"|"error"|"float32"|"float64"|"int"|"int8"|"int16"|"int32"|"int64"|"rune"|"string"|"uint"|"uint8"|"uint16"|"uint32"|"uint64"|"uintptr"|"complex64"|"complex128"
BOOL_VALUE      "true"|"false"
NIL             "nil"
CHANGE_ASSIGN   "+="|"-="|"*="|"/="|"%="|"<<="|">>="|"&="|"&^="|"="
PREDEFINED_FUNCTION = "append"|"cap"|"close"|"complex"|"copy"|"delete"|"imag"|"len"|"make"|"new"|"panic"|"print"|"println"|"real"|"recover"

%%
\"(\\.|[^"\\])*\"              { /*printf("STRING");*/          return STRING; }
"(\/\/([^\n][\s\w])*)|(\/\*[\w\n\s]*\*\/)"    { /* printf("Commentary."); */ }                       

"package"                      { /*printf("package");*/         return PACKAGE; }
"interface"                    { /*printf("interface");*/       return INTERFACE; }
"map"                          { /*printf("map");*/             return MAP; }
"goto"                         { /*printf("goto");*/            return GOTO; }
"fallthrough"                  { /*printf("fallthrough");*/     return FALLTHROUGH; }
"defer"                        { /*printf("defer");*/           return DEFER; }
"chan"                         { /*printf("chan");*/            return CHAN; }
"import"                       { /*printf("import");*/          return IMPORT; }
"func"                         { /*printf("func");*/            return FUNC; }
"break"                        { /*printf("break");*/           return BREAK; }
"case"                         { /*printf("case");*/            return CASE; }
"const"                        { /*printf("const");*/           return CONST; }
"continue"                     { /*printf("continue");*/        return CONTINUE; }
"default"                      { /*printf("default");*/         return DEFAULT; }
"else"                         { /*printf("else");*/            return ELSE; }
"for"                          { /*printf("for");*/             return FOR; }
"go"                           { /*printf("go");*/              return GO; }
"if"                           { /*printf("if");*/              return IF; }
"range"                        { /*printf("range");*/           return RANGE; }
"return"                       { /*printf("return");*/          return RETURN; }
"struct"                       { /*printf("struct");*/          return STRUCT; }
"switch"                       { /*printf("switch");*/          return SWITCH; }
"type"                         { /*printf("type");*/            return TYPE; }
"var"                          { /*printf("var");*/             return VAR; }
"<-"                           { /*printf("arrow left");*/      return ARROW_LEFT; }
":="                           { /*printf("assign");*/          return ASSIGN; }
"||"                           { /*printf("or");*/              return OR; }
"&&"                           { /*printf("and");*/             return AND; }
"=="                           { /*printf("equall");*/          return EQUALL; }
"!="                           { /*printf("not equall");*/      return NOT_EQUALL; }
">="                           { /*printf("greater equall");*/  return GREATER_EQUALL; }
"<="                           { /*printf("less equall");*/     return LESS_EQUALL; }
"<<"                           { /*printf("shift left");*/      return SHIFT_LEFT; }
">>"                           { /*printf("shift right");*/     return SHIFT_RIGHT; }
"&^"                           { /*printf("and xor");*/         return AND_XOR; }
"..."                          { /*printf("multidot");*/        return MULTIDOT; }
"++"                           { /*printf("increment");*/       return INCREMENT; }
"--"                           { /*printf("decrement");*/       return DECREMENT; }  

{CHANGE_ASSIGN}                { /*printf("change+assign");*/   return CHANGE_ASSIGN; }
{VARIABLE_TYPE}                {
                                    yylval.digit = 0;
                                    yylval.buffer[0] = *yytext;
                                    yylval.buffer[1] = yytext[1];
                                    yylval.buffer[2] = yytext[2];
                                    yylval.buffer[3] = yytext[3];
                                    yylval.buffer[4] = yytext[4]; 
                                    return IDENTIFIER; }

[a-zA-Z_][a-zA-Z0-9_]*          {
                                    yylval.digit = 0;
                                    yylval.buffer[0] = *yytext;
                                    yylval.buffer[1] = yytext[1];
                                    yylval.buffer[2] = yytext[2];
                                    yylval.buffer[3] = yytext[3];
                                    yylval.buffer[4] = yytext[4]; 
                                    return IDENTIFIER; }
                                    
{NIL}                           {   
                                    yylval.digit = 0;
                                    yylval.buffer[0] = *yytext;
                                    yylval.buffer[1] = yytext[1];
                                    yylval.buffer[2] = yytext[2];
                                    yylval.buffer[3] = 0;
                                    return IDENTIFIER; }
                                    
{BOOL_VALUE}                    {
                                    yylval.digit = 0;
                                    yylval.buffer[0] = 'b';
                                    yylval.buffer[1] = 'o';
                                    yylval.buffer[2] = 'o';
                                    yylval.buffer[3] = 'l';
                                    yylval.buffer[4] = 0;
                                    return IDENTIFIER; }
                                    
PREDEFINED_FUNCTION             {
                                    yylval.digit = 0;
                                    yylval.buffer[0] = 'f';
                                    yylval.buffer[1] = 'u';
                                    yylval.buffer[2] = 'n';
                                    yylval.buffer[3] = 'c';
                                    yylval.buffer[4] = 0;
                                    return IDENTIFIER;
                                }
                                    
{DIGIT}		                    { 
                                    yylval.buffer[0] = 0;
                                    yylval.buffer[1] = 0;
                                    yylval.buffer[2] = 0;
                                    yylval.buffer[3] = 0;
                                    yylval.buffer[4] = 0;
                                    yylval.digit = *yytext - '0'; 
                                     /*printf("Digit - %d.\n", yylval.digit);*/
                                     return DIGIT; };
                                     
[\t ]                           { /*printf("%c", *yytext);*/ }
[\n]                            { /*printf("\n");*/ }

[\[\]\(\)\{\}]                  { /*printf("Founded %c.\n", *yytext);*/ return *yytext; }
[^a-zA-Z0-9_]                   { /*printf("\nUnexpected character '%c'.\n\n", *yytext);*/ return *yytext; };

%%