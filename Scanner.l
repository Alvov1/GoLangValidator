%{
#include "Grammar.tab.h"
%}

LETTER          [a-zA-Z]
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
RUNE                    \'( UNICODE_VALUE|BYTE)\'

VAR_TYPE        "bool"|"byte"|"error"|"float32"|"float64"|"int"|"int8"|"int16"|"int32"|"int64"|"rune"|"string"|"uint"|"uint8"|"uint16"|"uint32"|"uint64"|"uintptr"|"complex64"|"complex128"
BOOL_CONST      "true"|"false"
NIL             "nil"

%%
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

{VAR_TYPE}                      { /*printf("type");*/           return VAR_TYPE; }
{BOOL_CONST}                    { /*printf("true");*/           return BOOL_CONST; } 
{NIL}                           { /*printf("nil");*/            return NIL; }

\"(\\.|[^"\\])*\"               { /*printf("STRING");*/         return STRING; }
[a-zA-Z][a-zA-Z0-9_]*           { /*printf("IDENTIFIER");*/     return IDENTIFIER; }
{DIGIT}		                    { /*printf("%c", *yytext);*/    return DIGIT; };
[\t ]                           { /*printf("%c", *yytext);*/ }
[\n]                            { /*printf("\n");*/ }

[\[\]\(\)\{\}]                  { /*printf("Founded %c.\n", *yytext);*/ return *yytext; }
[^a-zA-Z0-9_]                   { /*printf("Unexpected character '%c'", *yytext);*/ return *yytext; };

%%