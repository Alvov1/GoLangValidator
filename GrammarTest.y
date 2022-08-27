%{
	#include <stdio.h>
	#include <stdlib.h>
	
	int yylex();
	int yyerror();
	int yywrap();
	
%}

%union {
	int digit;
	char letter;
}

%token IDENTIFIER DIGIT LETTER VAR_TYPE BOOL_CONST NIL PACKAGE INTERFACE MAP 
%token GOTO FALLTHROUGH DEFER CHAN IMPORT FUNC BREAK CASE CONST
%token CONTINUE DEFAULT ELSE FOR GO IF RANGE RETURN STRUCT SWITCH TYPE VAR

%%

Program :
	VarDecl			{ exit(0); }
	| VarDecl '\n' 		{ exit(0); }

VarDecl :
	VAR VarSpec						{ printf("VarDecl - VAR ( VarSpec ).\n"); }
	| VAR "(" '{' VarSpec ";" '}' ")"			{ printf("VarDecl - VAR ( ( VarSpec ; ) ).\n"); }
	;
	
VarSpec :
	//IdentifierList '(' Type '[' "=" ExpressionList ']' ')' 	{ pritf("VarSpec - IdentifierList ( Type [ = ExpressionList ] ).\n"); }
	//| IdentifierList '(' "=" ExpressionList ')'		{ pritf("VarSpec - IdentifierList ( = ExpressionList ).\n"); }
	IDENTIFIER 					{ printf("VarSpec - IdentifierList.\n"); }
	;
//identifier :
	//LETTER '{' LETTER '}'		{ printf("Identifier - LETTER { LETTER }.\n"); exit(0); }
	//| LETTER '{' DIGIT '}'		{ printf("Identifier - LETTER { DIGIT }.\n"); }
	//;

%%

int main() {	
	yyparse();
	return 0;
}
int yyerror(const char* s) {
	printf("Error: %s\n", s);
	return 1;
}
int yywrap() {
	return 1;
}