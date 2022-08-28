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

VarDecl :
	VAR VarSpec				{ printf("VarDecl - VAR VarSpec.\n"); exit(0); }
	| VAR "(" '{' VarSpec ";" '}' ")"	{ printf("VarDecl - VAR ( VarSpec ; ).\n"); exit(0); }
	;
	
VarSpec :
	IdentifierList Type '[' "=" ExpressionList ']' 		{ pritf("VarSpec - IdentifierList ( Type [ = ExpressionList ] ).\n"); }
	| IdentifierList "=" ExpressionList				{ pritf("VarSpec - IdentifierList ( = ExpressionList ).\n"); }
	//IdentifierList 							{ printf("VarSpec - IdentifierList.\n"); }
	;
	
ExpressionList :
	Expression '{' "," Expression '}'			{ printf("ExpressionList.\n"); }
	;

Expression :
	UnaryExpr						{ printf("Expression - UnaryExpr.\n"); }
	| Expression binary_op Expression			{ printf("Expression - Expression op Expression.\n"); }
	;
	
UnaryExpr :
	PrimaryExpr						{ printf("UnaryExpr - PrimaryExpr.\n"); }
	| unary_op UnaryExpr					{ printf("UnaryExpr - op UnaryExpr.\n"); }
	;
	
binary_op :
	"||"							{ printf("Binary_operator: ||.\n"); }
	| "&&"							{ printf("Binary_operator: &&.\n"); }
	| rel_op							{ printf("Binary_operator: Rel_op.\n"); }
	| add_op							{ printf("Binary_operator: Add_op.\n"); }
	| mul_op							{ printf("Binary_operator: Mul_op.\n"); }
	;
	
rel_op :
	"=="							{ printf("Relativeness_operator: ==.\n"); }
	| "!="							{ printf("Relativeness_operator: !=.\n"); }
	| "<"							{ printf("Relativeness_operator: <.\n"); }
	| "<="							{ printf("Relativeness_operator: <=.\n"); }
	| ">"							{ printf("Relativeness_operator: >.\n"); }
	| ">="							{ printf("Relativeness_operator: >=.\n"); }
	;
	
add_op :
	"+"							{ printf("Addition_operator: +.\n"); }
	| "-"							{ printf("Addition_operator: -.\n"); }
	| "|"							{ printf("Addition_operator: |.\n"); }
	| "^"							{ printf("Addition_operator: ^.\n"); }
	;
	
mul_op :
	"*"							{ printf("Multiply_operator: *.\n"); }
	| "/"							{ printf("Multiply_operator: /.\n"); }
	| "%"							{ printf("Multiply_operator: %.\n"); }
	| "<<"							{ printf("Multiply_operator: <<.\n"); }
	| ">>"							{ printf("Multiply_operator: >>.\n"); }
	| "&"							{ printf("Multiply_operator: &.\n"); }
	| "&^"							{ printf("Multiply_operator: &^.\n"); }
	;
	
unary_op :
	"+"							{ printf("Unary_operator: +.\n"); }
	| "-"							{ printf("Unary_operator: -.\n"); }
	| "!"							{ printf("Unary_operator: !.\n"); }
	| "^"							{ printf("Unary_operator: ^.\n"); }
	| "*"							{ printf("Unary_operator: *.\n"); }
	| "&"							{ printf("Unary_operator: &.\n"); }
	| "<-"							{ printf("Unary_operator: <-.\n"); }
	;

PrimaryExpr :
	Operand							{ printf("PrimaryExpr - Operand.\n"); }
	| Conversion						{ printf("PrimaryExpr - Conversion.\n"); }
	| MethodExpr						{ printf("PrimaryExpr - MethodExpr.\n"); }
	| PrimaryExpr Selector					{ printf("PrimaryExpr - PrimaryExpr Selector.\n"); }
	| PrimaryExpr Index					{ printf("PrimaryExpr - PrimaryExpr Index.\n"); }
	| PrimaryExpr Slice					{ printf("PrimaryExpr - PrimaryExpr Slice.\n"); }
	| PrimaryExpr TypeAssertion				{ printf("PrimaryExpr - PrimaryExpr TypeAssertion.\n"); }
	| PrimaryExpr Arguments					{ printf("PrimaryExpr - PrimaryExpr Arguments.\n"); }
	;
	
Selector :
	"." IDENTIFIER						{ printf("Selector.\n");}
	;

Index :
	"[" Expression "]"					{ printf("Index.\n"); }
	;
	
Slice :
	"[" '[' Expression ']' ":" '[' Expression ']' "]"		{ printf("Slice - [ [ Expression ] : [ Expression ] ].\n");}
	| "[" '[' Expression ']' ":" Expression ":" Expression "]"	{ printf("Slice - [ [ Expression ] : [ Expression : Expression ].\n");}
	;
	
TypeAssertion :
	"." "(" Type ")"				{ printf("TypeAssertion.\n"); }
	;
	
Arguments :
	"(" '[' '(' ExpressionList | Type '[' "," ExpressionList ']' ')' '[' "..." ']' '[' "," ']' ']' ")" { printf("Arguments.\n"); }
	;
	
Operand :
	Literal						{ printf("Operand - Literal.\n"); }
	| OperandName '[' TypeArgs ']'						{ printf("Operand - OperandName [ TypeArgs ].\n"); } 
	| "(" Expression ")"						{ printf("Operand - ( Expression ).\n"); }
	;
	
Literal :
	BasicLit				{ printf("Literal - BasicLit.\n"); } 
	| CompositeLit				{ printf("Literal - CompositeLit.\n"); } 
	| FunctionLit				{ printf("Literal - FunctionLit.\n"); } 
	;
	
BasicLit :
	int_lit					{ printf("BasicLit - Int_lit.\n"); }
	| float_lit				{ printf("BasicLit - Float_lit.\n"); }
	| imaginary_lit				{ printf("BasicLit - Imaginary_lit.\n"); }
	| rune_lit				{ printf("BasicLit - Rune_lit.\n"); }
	| string_lit				{ printf("BasicLit - String_lit.\n"); }
	;
	
OperandName :
	IDENTIFIER				{ printf("OparandName - Identifier.\n"); }
	| QualifiedIdent			{ printf("OperandName - QualifiedIdent.\n"); }
	
CompositeLit :
	LiteralType LiteralValue
	;
	
LiteralType :
	StructType					{ printf("LiteralType - StructType.\n"); }
	| ArrayType					{ printf("LiteralType - ArrayType.\n"); }
	| "[" "..." "]" ElementType			{ printf("LiteralType - [ ... ] ElementType.\n"); }
	| SliceType					{ printf("LiteralType - SliceType.\n"); }
	| MapType					{ printf("LiteralType - MapType.\n"); }
	| TypeName '[' TypeArgs ']'			{ printf("LiteralType - Typename [ TypeArgs ].\n"); }
	;
	
LiteralValue :
	"{" '[' ElementList '[' "," ']' ']' "}"		{ printf("LiteralValue.\n"); }
	;
	
ElementList :
	KeyedElement '{' "," KeyedElement '}'		{ printf("ElementList.\n"); }
	;
	
KeyedElement :
 	'[' Key ":" ']' Element				{ printf("KeyedElement.\n"); }
 	;
 	
Key :
 	FieldName					{ printf("Key - FieldName.\n"); } 
 	| Expression					{ printf("Key - Expression.\n"); } 
 	| LiteralValue					{ printf("Key - LiteralValue.\n"); }
 	;
 	
FieldName : 
	IDENTIFIER					{ printf("FieldName.\n"); }
	;
	
Element : 
	Expression					{ printf("Element - Expression.\n"); }
	| LiteralValue					{ printf("Element - LiteralValue.\n"); }
	;
	
Type :
	TypeName '[' TypeArgs ']' 	{ printf("Type - Typename [ TypeArgs ].\n"); }
	| TypeLit			{ printf("Type - TypeLit.\n"); }
	| "(" Type ")"			{ printf("Type - ( Type ).\n"); }
	;

TypeName :
	IDENTIFIER 			{ printf("TypeName - identifier.\n"); }
	| QualifiedIdent 		{ printf("TypeName - QualifiedIdent.\n"); }
	;

TypeArgs :
	"[" TypeList '[' "," ']' "]" 	{ printf("TypeArgs - [ TypeList [ , ] ].\n"); }
	;
	
TypeList :
	Type '{' "," Type '}' 		{ printf("TypeList - Type { , Type }.\n"); }
	;
	
TypeLit :
	ArrayType			{ printf("TypeLit - ArrayType.\n");}
	| FunctionType			{ printf("TypeLit - FunctionType.\n"); }
	| StructType			{ printf("TypeLit - StructType.\n"); }
	| PointerType			{ printf("TypeLit - PointerType.\n"); } 
	| InterfaceType		{ printf("TypeLit - InterfaceType.\n"); } 
	| SliceType			{ printf("TypeLit - SliceType.\n"); }
	| MapType			{ printf("TypeLit - MapType.\n"); }
	| ChannelType			{ printf("TypeLit - ChannelType.\n"); }
	;

ArrayType :
	"[" ArrayLength "]" ElementType { printf("ArrayType.\n"); }
	;

ArrayLength :
	Expression			{ printf("ArrayLength.\n"); }
	;
	
ElementType :
	Type				{ printf("ElementType.\n"); }
	;

FunctionType :
	FUNC Signature 			
			{ printf("FunctionType.\n"); }
	;

/* Struct Types */	
StructType :
	STRUCT "{" '{' FieldDecl ";" '}' "}"		{ printf("StructType.\n"); }
	;
	
FieldDecl :
	IdentifierList Type '[' Tag ']'			{ printf("FieldDecl - IdentifierList Type [ Tag ].\n"); }
	| EmbeddedField '[' Tag ']'			{ printf("FieldDecl - EmbeddedField [ Tag ].\n"); }
	;
	
EmbeddedField :
 	'[' "*" ']' TypeName '[' TypeArgs ']'		{ printf("EmbeddedField.\n"); }
 	;
Tag : 
	string_lit					{ printf("Tag.\n"); }
	;
	
/**/
Signature :
	Parameters '[' Result ']'			{ printf("Signature - Parameters Result.\n"); }
	;
	
Result :
	Parameters 					{ printf("Result - Parameters.\n"); }
	| Type						{ printf("Result - Type.\n"); }
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
	";"						{ printf("Statement.\n"); }
	;
	
IdentifierList :
	IDENTIFIER					{ printf("IdentifierList - identifier.\n"); }
	| IDENTIFIER '{' "," IDENTIFIER '}'		{ printf("Identifier - identifier { , identifier }.\n"); }
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
	IDENTIFIER	{ printf("FunctionName.\n"); }
	;

FunctionBody :
	Block		{ printf("FunctionBody.\n"); }
	;

QualifiedIdent :
	PackageName "." IDENTIFIER	{ printf("QualifiedIdent - PackageName . identifier.\n"); }
	;

PackageName :
	IDENTIFIER			{ printf("PackageName - identifier.\n"); }
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