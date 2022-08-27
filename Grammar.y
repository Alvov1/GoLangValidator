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

%token DIGIT LETTER VAR_TYPE BOOL_CONST NIL PACKAGE INTERFACE MAP 
%token GOTO FALLTHROUGH DEFER CHAN IMPORT FUNC BREAK CASE CONST 
%token CONTINUE DEFAULT ELSE FOR GO IF RANGE RETURN STRUCT SWITCH TYPE VAR

%%

VarDecl :
	VAR '(' VarSpec ')'				{ printf("VarDecl - VAR ( VarSpec ).\n"); }
	| VAR '(' "(" '{' VarSpec ";" '}' ")" ')'	{ printf("VarDecl - VAR ( ( VarSpec ; ) ).\n"); }
	;
	
VarSpec :
	//IdentifierList '(' Type '[' "=" ExpressionList ']' ')' 	{ pritf("VarSpec - IdentifierList ( Type [ = ExpressionList ] ).\n"); }
	//| IdentifierList '(' "=" ExpressionList ')'		{ pritf("VarSpec - IdentifierList ( = ExpressionList ).\n"); }
	IdentifierList 					{ printf("VarSpec - IdentifierList.\n"); }
	;
identifier :
	LETTER '{' LETTER '}'		{ printf("Identifier - LETTER { LETTER }.\n"); exit(0); }
	| LETTER '{' DIGIT '}'		{ printf("Identifier - LETTER { DIGIT }.\n"); }
	;

Type :
	TypeName '[' TypeArgs ']' 	{ printf("Type - Typename [ TypeArgs ].\n"); }
	| TypeLit			{ printf("Type - TypeLit.\n"); }
	| "(" Type ")"			{ printf("Type - ( Type ).\n"); }
	;

TypeName :
	identifier 			{ printf("TypeName - identifier.\n"); }
	| QualifiedIdent 		{ printf("TypeName - QualifiedIdent.\n"); }
	;

TypeArgs :
	"[" TypeList '[' "," ']' "]" 	{ printf("TypeArgs - [ TypeList [ , ] ].\n"); }
	;
	
TypeList :
	Type '{' "," Type '}' 		{ printf("TypeList - Type { , Type }.\n"); }
	;
	
TypeLit :
	//ArrayType			{ printf("TypeLit - ArrayType.\n");}
	//| StructType			{ printf("TypeLit - StructType.\n"); }
	//| PointerType			{ printf("TypeLit - PointerType.\n"); } 
	FunctionType			{ printf("TypeLit - FunctionType.\n"); }
	//| InterfaceType			{ printf("TypeLit - InterfaceType.\n"); } 
	//| SliceType			{ printf("TypeLit - SliceType.\n"); }
	//| MapType			{ printf("TypeLit - MapType.\n"); }
	//| ChannelType			{ printf("TypeLit - ChannelType.\n"); }
	;

//ArrayType :
	//"[" ArrayLength "]" ElementType { printf("ArrayType.\n"); }
	//;

FunctionType :
	FUNC Signature 	{ printf("FunctionType.\n"); }
	;

Signature :
	Parameters '[' Result ']'	{ printf("Signature - Parameters Result.\n"); }
	;
	
Result :
	Parameters 		{ printf("Result - Parameters.\n"); }
	| Type			{ printf("Result - Type.\n"); }
	;
	
Parameters :
	"(" '[' ParameterList '[' "," ']' ']' ")" 	{ printf("Parameters.\n"); }
	;
	
ParameterList :
	ParameterDecl '{' "," ParameterDecl '}'		{ printf("ParameterList.\n"); }
	;

ParameterDecl :
	'[' IdentifierList ']' '[' "..." ']' Type	{ printf("ParameterDecl.\n"); }
	;

Block :
	"{" StatementList "}" 				{ printf("Block.\n"); }
	;
	
StatementList :
	'{' Statement ";" '}'				{ printf("StatementList.\n"); }
	;

Statement :
	";"
	;
	
IdentifierList :
	identifier					{ printf("IdentifierList - identifier.\n"); exit(0); }
	| identifier '{' "," identifier '}'		{ printf("Identifier - identifier { , identifier }.\n"); }
	;
	
FunctionDecl :
	FUNC FunctionName '[' TypeParameters ']' Signature '[' FunctionBody ']'	{ printf("FunctionDecl.\n"); }
	;
	
TypeParameters :
	"[" TypeParamList '[' "," ']' "]" 		{ printf("TypeParameters.\n"); }
	;
	
TypeParamList :
	TypeParamDecl '{' "," TypeParamDecl '}'		{ printf("TypeParamList.\n"); }
	;
	
TypeConstraint :
	TypeElem					{ printf("TypeConstraint.\n"); }
	;
	
TypeElem :
	TypeTerm '{' "|" TypeTerm '}'			{ printf("TypeElem.\n"); }
	;

TypeTerm :
	Type						{ printf("TypeTerm - Type.\n"); }
	| UnderlyingType				{ printf("TypeTerm - UnderlyingType.\n"); }
	;

UnderlyingType :
	"~" Type					{ printf("UnderlyingType.\n"); }
	;

TypeParamDecl :
	IdentifierList TypeConstraint 			{ printf("TypeParamDecl.\n"); }
	;

FunctionName :
	identifier	{ printf("FunctionName.\n"); }
	;

FunctionBody :
	Block		{ printf("FunctionBody.\n"); }
	;

QualifiedIdent :
	PackageName "." identifier	{ printf("QualifiedIdent - PackageName . identifier.\n"); }
	;

PackageName :
	identifier			{ printf("PackageName - identifier.\n"); }
	;

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