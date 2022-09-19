%{
	#define FIELD 5
	#include <stdio.h>
	#include <stdlib.h>
	
	int yylex();
	int yyerror();
	int yywrap();
	
	#define ALL_OUTPUT 1
	#define IMPORTANT_OUTPUT 3
	
	unsigned priority = ALL_OUTPUT;
	extern FILE* lexOutput;
	extern unsigned fileHeight;
%}

%union {
	#define bufferSize 100
	int digit;
	char buffer[bufferSize];
}

%token IDENTIFIER DIGIT PACKAGE INTEGER FLOAT IMAGINARY RUNE ASSIGN_OP
%token GOTO FALLTHROUGH DEFER CHAN IMPORT FUNC BREAK CASE CONST
%token CONTINUE DEFAULT ELSE FOR GO IF RANGE RETURN STRUCT 
%token HEX_BYTE LITTLE_U BIG_U ESCAPED CHAR SELECT OCT HEX OCT_BYTE
%token ASSIGN OR AND EQUALL NOT_EQUALL LESS_EQUALL GREATER_EQUALL 
%token SHIFT_LEFT SHIFT_RIGHT AND_XOR MULTIDOT INCREMENT DECREMENT
%token SWITCH TYPE VAR STRING ARROW_LEFT INTERFACE MAP

%right IDENTIFIER
%left '+' '-' '/' '%' '&' '|' '^'
%right '*'
%right '=' '!'
%left AND OR EQUALL NOT_EQUALL LESS_EQUALL GREATER_EQUALL ';' ':' INCREMENT DECREMENT
%left '>' '<' '(' ')' '{' '}' '[' ']' ','
%right '.'

%start SourceFile

%%

Type :
    	TypeOperandName                          		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Type - TypeOperandName.\n", fileHeight); }
	| TypeOperandName TypeArgs        	        	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Type - Typename TypeArgs.\n", fileHeight); }
	| TypeLit			                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Type - TypeLit.\n", fileHeight); }
//	| '(' Type ')'			            		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Type - ( Type ).\n", fileHeight); }
	;

TypeOperandName :
	IDENTIFIER 			                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] TypeOperandName - identifier.\n", fileHeight, yylval.buffer); }
	| IDENTIFIER '.' IDENTIFIER	            		{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]    TypeOperandName - identifier . identifier.\n", fileHeight, yylval.buffer); }
	;

TypeArgs :
    	'[' TypeList ']'                    			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeArgs - [ TypeList      ].\n", fileHeight); }
	'[' TypeList ',' ']' 	    				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeArgs - [ TypeList ,      ].\n", fileHeight); }
	;
	
TypeList :
	Type                        				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeList - Type.\n", fileHeight); }
	| TypeList ',' Type                     			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeList - TypeList , Type.\n", fileHeight); }
	;
	
TypeLit :
	ArrayType			                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - ArrayType.\n", fileHeight);}
	| StructType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - StructType.\n", fileHeight); }
	| PointerType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - PointerType.\n", fileHeight); } 
	| FunctionType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - FunctionType.\n", fileHeight); }
	| InterfaceType		                		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - InterfaceType.\n", fileHeight); } 
	| SliceType			                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - SliceType.\n", fileHeight); }
	| MapType			                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - MapType.\n", fileHeight); }
	| ChannelType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeLit - ChannelType.\n", fileHeight); }
	;

ArrayType :
	'[' ArrayLength ']' ElementType     			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ArrayType - [ ArrayLength      ] ElemenType.\n", fileHeight); }
	;

ArrayLength :
	Expression			                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ArrayLength - Expression.\n", fileHeight); }
	;
	
ElementType :
	Type				                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ElementType - Type.\n", fileHeight); }
	;
	
SliceType :
	'[' ']' ElementType                 			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    SliceType - [      ] ElemType.\n", fileHeight); }
	;
    
StructType :
	STRUCT '{' FieldDecls '}'				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    StructType - struct { FieldDecls }.\n", fileHeight); }
	;
	
FieldDecls :
    	| FieldDecls  FieldDecl ';'             		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FieldDecls - FieldDecls FieldDecl.\n", fileHeight); }
    	;
    
FieldDecl :
	IdentifierListOrNothing Type				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FieldDecl - IdentifierListOrNothing Type.\n", fileHeight); }
	| '*' IdentifierListOrNothing TypeOperandName		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FieldDecl - * IdentifierListOrNothing TypeOperandName.\n", fileHeight); }
	| IdentifierListOrNothing Type Tag			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FieldDecl - IdentifierListOrNothing Type Tag.\n", fileHeight); }
	| '*' IdentifierListOrNothing TypeOperandName Tag	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FieldDecl - * IdentifierListOrNothing TypeOperandName Tag.\n", fileHeight); }
	;
	
IdentifierListOrNothing :
								{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    IdentifierListOrNothing - nothing.\n", fileHeight); }
	| IdentifierList					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    IdentifierListOrNothing - IdentifierList.\n", fileHeight); }
	; 	
Tag : 
	STRING					            	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Tag - string.\n", fileHeight); }
	;

PointerType :
	'*' BaseType                        			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PointerType - * BaseType.\n", fileHeight); }
	;
	
BaseType :
	Type                                			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    BaseType - Type.\n", fileHeight); }
	;
    
FunctionType :
	FUNC Signature 		                		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FunctionType - func Signature.\n", fileHeight); }
	;

Signature :
	Parameters						{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Signature - Parameters.\n", fileHeight); }
	| Parameters Result			        	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Signature - Parameters Result.\n", fileHeight); }
	;
	
Result :
	Parameters 					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Result - Parameters.\n", fileHeight); }
	| Type						        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Result - Type.\n", fileHeight); }
	;
	
Parameters :
	'('')'                             			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Parameters - ( ).\n", fileHeight); }
	| '(' ParameterList ')'             			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Parameters - ( ParameterList ).\n", fileHeight); }
	| '(' ParameterList ',' ')' 	    			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Parameters - ( ParameterList , ).\n", fileHeight); }
	;
	
ParameterList :	
	ParameterDecl						{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ParameterList - ParameterDecl.\n", fileHeight); }
	| ParameterList ',' ParameterDecl			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ParameterList - ParameterList , ParameterDecl.\n", fileHeight); }
	;
	
ParameterDecl :	
	IdentifierListOrNothing Type				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ParameterDecl - IdentifierListOrNothing Type.\n", fileHeight); }
	| IdentifierListOrNothing MULTIDOT Type			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ParameterDecl - IdentifierListOrNothing ... Type.\n", fileHeight); }
	;
		
InterfaceType :
	INTERFACE '{' InterfaceElems '}'         		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    InterfaceType - interface { InterfaceElems }.\n", fileHeight); }
	;

InterfaceElems :
								{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    InterfaceElems - nothing.\n", fileHeight); }
	| InterfaceElems InterfaceElem ';'                 	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    InterfaceElems - InterfaceElems InterfaceElem.\n", fileHeight); }
	;
    
InterfaceElem :
	MethodElem                          			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    InterfaceElem - MethodElem.\n", fileHeight); }
	| TypeElem                          			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    InterfaceElem - TypeElem.\n", fileHeight); }
	;

MethodElem :
	IDENTIFIER Signature                			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    MethodElem - MethodName Signature.\n", fileHeight); }
	;
    
TypeElem :
	TypeTerm                            			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeElem - TypeTerm.\n", fileHeight); }
	| TypeElem '|' TypeTerm             			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeElem - TypeElem | TypeTerm.\n", fileHeight); }
	;
    
TypeTerm :
	Type                                			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeTerm - Type\n", fileHeight); }
	| UnderlyingType                    			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeTerm - UnderlyingType\n", fileHeight); }
	;

UnderlyingType :
	'~' Type                            			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    UnderlyingType - ~ Type.\n", fileHeight); }
	;
    
MapType :
	MAP '[' KeyType ']' ElementType     			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    MapType - map [ KeyType      ] ElementType.\n", fileHeight); }
	;

KeyType :
	Type                                			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    KeyType - Type.\n", fileHeight); }
	;
    
ChannelType :    
	CHAN ArrowLeftNothing ElementType                    	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ChannelType - chan ElementType.\n", fileHeight); }
	| ARROW_LEFT CHAN ElementType       			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ChannelType - <- chan ElementType.\n", fileHeight); }
	;
	
ArrowLeftNothing :
	| ARROW_LEFT
	;
    
Block :
	'{' StatementList '}'				        { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Block - { StatementList }.\n", fileHeight); }
	;
    
StatementList :
								{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    StatementList - nothing ;.\n", fileHeight); }
	| StatementList Statement ';'          			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    StatementList - StatementList Statement ;.\n", fileHeight); }
	;
    
Declaration :
	ConstDecl                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Declaration - ConstDecl\n", fileHeight); }
	| TypeDecl                          			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Declaration - TypeDecl\n", fileHeight); }
	| VarDecl                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Declaration - VarDecl\n", fileHeight); }
	;
    
TopLevelDecl :
	Declaration                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    TopLevelDecl - Declaration.\n", fileHeight); }
	| FunctionDecl                      			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    TopLevelDecl - FunctionDecl.\n", fileHeight); }
	| MethodDecl                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    TopLevelDecl - MethodDecl.\n", fileHeight); }
	;
    
ConstDecl :
	CONST ConstSpec                                       	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ConstDecl - const ConstSpec.\n", fileHeight); }  
	| CONST '(' ConstSpecs ')'                 			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ConstDecl - const ( ConstSpecs ).\n", fileHeight); }
	;

ConstSpecs :
								{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ConstSpecs - nothing.\n", fileHeight); }
	| ConstSpecs ConstSpec ';'                             	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ConstSpecs - ConstSpecs ConstSpec.\n", fileHeight); }
	;

ConstSpec :
	IdentifierList                                      	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ConstSpec - IdentifierList.\n", fileHeight); }
	| IdentifierList '=' ExpressionList                 	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ConstSpec - IdentifierList = ExpressionList.\n", fileHeight); }
	| IdentifierList Type '=' ExpressionList            	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ConstSpec - IdentifierList Type = ExpressionList.\n", fileHeight); }
	;
    
IdentifierList :
	IDENTIFIER                                              	{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] IdentifierList - Identifier.\n", fileHeight, yylval.buffer); }
	| IdentifierList ',' IDENTIFIER                         	{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] IdentifierList - IdentifierList Identifier.\n", fileHeight, yylval.buffer); }
	;
	
ExpressionList :
	Expression                                      { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExpressionList - Expression.\n", fileHeight); }
	| ExpressionList ',' Expression                 { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExpressionList - ExpressionList , Expression.\n", fileHeight); }
	;
    
TypeDecl :
	TYPE TypeSpec                                               { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeDecl - type TypeSpec.\n", fileHeight); }
	| TYPE '(' TypeSpecs ')'                                    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeDecl - type ( TypeSpecs ).\n", fileHeight); }
	;
    
TypeSpecs :
								{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSpecs - nothing.\n", fileHeight); }
	| TypeSpecs TypeSpec ';'				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSpecs - TypeSpecs TypeSpec.\n", fileHeight); }
	;
    
TypeSpec :
	AliasDecl                                                   { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSpec - AliasDecl\n", fileHeight); }
	| TypeDef                                                   { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSpec - TypeDef\n", fileHeight); }
	;

AliasDecl :
	IDENTIFIER '=' Type                                         { if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] AliasDecl - identifier = Type.\n", fileHeight, yylval.buffer); }
	;    

TypeDef :
	IDENTIFIER Type                                             { if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] TypeDef - Identifier Type.\n", fileHeight, yylval.buffer); }
	| IDENTIFIER TypeParameters  Type                           { if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] TypeDef - Identifier TypeParameters Type.\n", fileHeight, yylval.buffer); }
	;
    
TypeParameters :
    	'[' TypeParamList ']'                                   	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeParameters - [ TypeParamList      ].\n", fileHeight); }
	| '[' TypeParamList ',' ']' 		                { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeParameters - [ TypeParamList ,      ].\n", fileHeight); }
	;
	
TypeParamList :
	TypeParamDecl                                           	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeParamList - TypeParamDecl.\n", fileHeight); }
	| TypeParamList ',' TypeParamDecl                       	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeParamList - TypeParamList , TypeParamDecl.\n", fileHeight); }
	;

TypeParamDecl :
	IdentifierList TypeConstraint 			        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeParamDecl - IdentifierList TypeConstraint.\n", fileHeight); }
	;
	
TypeConstraint :
	TypeElem					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeConstraint - TypeElem.\n", fileHeight); }
	;

VarDecl :
	VAR VarSpec				    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    VarDecl - VAR VarSpec.\n", fileHeight); }
	| VAR '(' VarSpecs ')'	                    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    VarDecl - VAR ( VarSpec ; ).\n", fileHeight); }
	;
	
VarSpecs :
							{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    VarSpecs - nothing.\n", fileHeight); }
	| VarSpecs VarSpec ';'                          { if (priority < IMPORTANT_OUTPUT) printf("[%d]    VarSpecs - VarSpecs VarSpec.\n", fileHeight); }
	;
    
VarSpec :
	IdentifierList Type                             { if (priority < IMPORTANT_OUTPUT) printf("[%d]    VarSpec: IdentifierList Type.\n", fileHeight); }
	| IdentifierList Type '=' ExpressionList        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    VarSpec: IdentifierList Type = ExpressionList.\n", fileHeight); }
	| IdentifierList '=' ExpressionList             { if (priority < IMPORTANT_OUTPUT) printf("[%d]    VarSpec: IdentifierList = ExpressionList.\n", fileHeight); }
	;
    
ShortVarDecl :
	IdentifierList ASSIGN ExpressionList                          { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ShortVarDecl - IdentifierList := ExpressionList.\n", fileHeight); }
	;
    
FunctionDecl :
	FUNC IDENTIFIER Signature                     			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FunctionDecl - func FunctionName Signature.\n", fileHeight); }
	| FUNC IDENTIFIER TypeParameters Signature                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FunctionDecl - func FunctionName TypeParameters Signature.\n", fileHeight); }
	| FUNC IDENTIFIER Signature FunctionBody			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FunctionDecl - func FunctionName Signature FunctionBody.\n", fileHeight); }
	| FUNC IDENTIFIER TypeParameters Signature FunctionBody   	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FunctionDecl - func FunctionName TypeParameters Signature FunctionBody.\n", fileHeight); }
	;

FunctionBody :
	Block		                                        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    FunctionBody - Block.\n", fileHeight); }
	;
	
MethodDecl :
	FUNC Receiver IDENTIFIER Signature                          { if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]    MethodDecl - func Receiver identifier Signature.\n", fileHeight, yylval.buffer); }
	| FUNC Receiver IDENTIFIER Signature FunctionBody           { if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]    MethodDecl - func Receiver identifier Signature FunctionBody.\n", fileHeight, yylval.buffer); }
	;

Receiver :
	Parameters                                                  { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Receiver - Parameters.\n", fileHeight); }
	;
    
Operand :
	Literal						        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Operand - Literal.\n", fileHeight); }
//	| TypeOperandName                       		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Operand - TypeOperandName.\n", fileHeight); }
//	| TypeOperandName TypeArgs		        	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Operand - TypeOperandName TypeArgs.\n", fileHeight); } 
	| '(' Expression ')'					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Operand - ( Expression ).\n", fileHeight); }
	;
	
Literal :
	BasicLit				            	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Literal - BasicLit.\n", fileHeight); } 
	| CompositeLit				        	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Literal - CompositeLit.\n", fileHeight); } 
	| FunctionLit				        	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Literal - FunctionLit.\n", fileHeight); } 
	;
	
BasicLit :
	INTEGER							{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]     BasicLit - Integer.\n", fileHeight, yylval.buffer); } 
	| FLOAT							{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]     BasicLit - Float.\n", fileHeight, yylval.buffer); } 
	| IMAGINARY						{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]     BasicLit - Imaginary.\n", fileHeight, yylval.buffer); } 
	| RUNE							{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]     BasicLit - Rune.\n", fileHeight, yylval.buffer); } 
	| STRING						{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s]     BasicLit - String.\n", fileHeight, yylval.buffer); } 
	;
	
CompositeLit :
	LiteralType LiteralValue				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    CompositeLit - LiteralType LiteralValue.\n", fileHeight); }
	;
	
LiteralType :
	StructType					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralType - StructType.\n", fileHeight); }
	| ArrayType					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralType - ArrayType.\n", fileHeight); }
	| '[' MULTIDOT ']' ElementType				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralType - [ ...      ] ElementType.\n", fileHeight); }
	| SliceType					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralType - SliceType.\n", fileHeight); }
	| MapType					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralType - MapType.\n", fileHeight); }
	| TypeOperandName                          		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralType - TypeOperandName.\n", fileHeight); }
//	| TypeOperandName TypeArgs     				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralType - TypeOperandName TypeArgs.\n", fileHeight); }
	;
	
LiteralValue :
	'{' '}'                         				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralValue - { }.\n", fileHeight); }
	| '{' ElementList '}'                           		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralValue - { ElementList }.\n", fileHeight); }
	| '{' ElementList ',' '}'                           	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    LiteralValue - { ElementList , }.\n", fileHeight); }
	;
	
ElementList :
	KeyedElement						{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ElementList - KeyedElement.\n", fileHeight); }
	| ElementList ',' KeyedElement				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ElementList - ElementList , KeyedElement.\n", fileHeight); }
	;
    
KeyedElement :
	Element                             			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    KeyedElement - Element.\n", fileHeight); }
	| Key ':' Element                               		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    KeyedElement - Key : Element.\n", fileHeight); }
	;
 	
Key :
	IDENTIFIER
 	| Expression					    	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Key - Expression.\n", fileHeight); } 
 	| LiteralValue					    	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Key - LiteralValue.\n", fileHeight); }
 	;

Element : 
	Expression					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Element - Expression.\n", fileHeight); }
	| LiteralValue					    	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Element - LiteralValue.\n", fileHeight); }
	;	
			
FunctionLit :
	FUNC Signature FunctionBody         			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    FunctionLit - func Signature FucntionBody.\n", fileHeight); }
	;
    
PrimaryExpr :
	Operand							{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PrimaryExpr - Operand.\n", fileHeight); }
//	| Conversion						{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PrimaryExpr - Conversion.\n", fileHeight); }
//	| Type '.' IDENTIFIER
	| PrimaryExpr Selector					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PrimaryExpr - PrimaryExpr Selector.\n", fileHeight); }
	| PrimaryExpr Index					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PrimaryExpr - PrimaryExpr Index.\n", fileHeight); }
	| PrimaryExpr Slice					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PrimaryExpr - PrimaryExpr Slice.\n", fileHeight); }
	| PrimaryExpr TypeAssertion				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PrimaryExpr - PrimaryExpr TypeAssertion.\n", fileHeight); }
	| PrimaryExpr Arguments					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PrimaryExpr - PrimaryExpr Arguments.\n", fileHeight); }
	;
	
Selector :
	'.' IDENTIFIER						{ if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] Selector - . identifier.\n", fileHeight, yylval.buffer);}
	;

Index :
	'[' Expression ']'						{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Index - Expression.\n", fileHeight); }
	;
	
Slice :
	'[' ':' ']'                                 		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Slice - [ ].\n", fileHeight); }
	| '[' Expression ':' ']'                                    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Slice - [ Expression ].\n", fileHeight); }
	| '[' ':' Expression ']'                                    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Slice - [ : Expression      ].\n", fileHeight); }
	| '[' Expression ':' Expression ']'                         { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Slice - [ Expression : Expression      ].\n", fileHeight); }
	| '[' ':' Expression ':' Expression ']'                     { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Slice - [ : Expression : Expression      ].\n", fileHeight); }
	| '[' Expression ':' Expression ':' Expression ']'          { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Slice - [ Expression : Expression : Expression      ].\n", fileHeight); }
	;
	
TypeAssertion :
	'.' '(' Type ')'				    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeAssertion - . ( Type ).\n", fileHeight); }
	;

Arguments :
	'(' ')'                                 			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( ).\n", fileHeight);  }
	| '(' ExpressionList ')'                                    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( ExpressionList ).\n", fileHeight);  }
	| '(' ExpressionList MULTIDOT ')'                              { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( ExpressionList ... ).\n", fileHeight);  }
	| '(' ExpressionList ',' ')'                                { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( ExpressionList , ).\n", fileHeight);  }
	| '(' ExpressionList MULTIDOT ',' ')'                          { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( ExpressionList ... , ).\n", fileHeight);  }    

	| '(' Type ')'                                  		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type ).\n", fileHeight);  }
	| '(' Type MULTIDOT ')'                                    	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type ... ).\n", fileHeight);  }
	| '(' Type ',' ')'                                  	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type , ).\n", fileHeight);  }
	| '(' Type MULTIDOT ',' ')'                                    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type ... , ).\n", fileHeight);  }   

	| '(' Type ',' ExpressionList ')'                           { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type , ExpressionList ).\n", fileHeight);  }
	| '(' Type ',' ExpressionList MULTIDOT ')'                     { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type , ExpressionList ... ).\n", fileHeight);  }
	| '(' Type ',' ExpressionList ',' ')'                       { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type ExpressionList , ).\n", fileHeight);  }
	| '(' Type ',' ExpressionList MULTIDOT ',' ')'                 { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Arguments: ( Type ExpressionList ... , ).\n", fileHeight);  }
	;
    
Expression :
	UnaryExpr					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Expression - UnaryExpr.\n", fileHeight); }
	| Expression binary_op Expression		{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Expression - Expression binary_op Expression.\n", fileHeight); }
	;
	
UnaryExpr :
	PrimaryExpr					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    UnaryExpr - PrimaryExpr.\n", fileHeight); }
	| unary_op UnaryExpr				{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    UnaryExpr - unary_op UnaryExpr.\n", fileHeight); }
	;
	
binary_op :
	OR						{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Binary_operator - ||.\n", fileHeight); }
	| AND						{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Binary_operator - &&.\n", fileHeight); }
	| rel_op					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Binary_operator - Rel_op.\n", fileHeight); }
	| add_op					        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Binary_operator - Add_op.\n", fileHeight); }
	| mul_op						    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Binary_operator - Mul_op.\n", fileHeight); }
	;
	
rel_op :
	EQUALL							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Relativeness_operator - ==.\n", fileHeight); }
	| NOT_EQUALL					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Relativeness_operator - !=.\n", fileHeight); }
	| '<'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Relativeness_operator - <.\n", fileHeight); }
	| LESS_EQUALL							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Relativeness_operator - <=.\n", fileHeight); }
	| '>'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Relativeness_operator - >.\n", fileHeight); }
	| GREATER_EQUALL							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Relativeness_operator - >=.\n", fileHeight); }
	;
	
add_op :
	'+'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Addition_operator - +.\n", fileHeight); }
	| '-'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Addition_operator - -.\n", fileHeight); }
	| '|'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Addition_operator - |.\n", fileHeight); }
	| '^'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Addition_operator - ^.\n", fileHeight); }
	;
	
mul_op :
	'*'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Multiply_operator - *.\n", fileHeight); }
	| '/'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Multiply_operator - /.\n", fileHeight); }
	| '%'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Multiply_operator - %%.\n", fileHeight); }
	| SHIFT_LEFT							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Multiply_operator - <<.\n", fileHeight); }
	| SHIFT_RIGHT							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Multiply_operator - >>.\n", fileHeight); }
	| '&'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Multiply_operator - &.\n", fileHeight); }
	| AND_XOR							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Multiply_operator - &^.\n", fileHeight); }
	;
	
unary_op :
	'+'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Unary_operator - +.\n", fileHeight); }
	| '-'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Unary_operator - -.\n", fileHeight); }
	| '!'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Unary_operator - !.\n", fileHeight); }
	| '^'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Unary_operator - ^.\n", fileHeight); }
	| '*'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Unary_operator - *.\n", fileHeight); }
	| '&'							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Unary_operator - &.\n", fileHeight); }
	| ARROW_LEFT							    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Unary_operator - <-.\n", fileHeight); }
	;

Conversion :
	Type '(' Expression ')'                                     { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Conversion - Type ( Expression ).\n", fileHeight); }
	| Type '(' Expression ',' ')'                               { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Conversion - Type ( Expression , ).\n", fileHeight); }
	;
    
Statement :
	Declaration                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - Declaration\n", fileHeight); }
	| LabeledStmt                       			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - LabeledStmt\n", fileHeight); }
	| SimpleStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - SimpleStmt\n", fileHeight); }
	| GoStmt                            			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - GoStmt\n", fileHeight); }
	| ReturnStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - ReturnStmt\n", fileHeight); }
	| BreakStmt                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - BreakStmt\n", fileHeight); }
	| ContinueStmt                      			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - ContinueStmt\n", fileHeight); }
	| GotoStmt                          			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - GotoStmt\n", fileHeight); }
	| FallthroughStmt                   			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - FallthroughStmt\n", fileHeight); }
	| Block                             			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - Block\n", fileHeight); }
	| IfStmt                            			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - IfStmt\n", fileHeight); }
	| SwitchStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - SwitchStmt\n", fileHeight); }
	| SelectStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - SelectStmt\n", fileHeight); }
	| ForStmt                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - ForStmt\n", fileHeight); }
	| DeferStmt                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Statement - DeferStmt\n", fileHeight); }
	;
    
SimpleStmt :
	EmptyStmt                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SimpleStmt - EmptyStmt\n", fileHeight); }
	| ExpressionStmt                    			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SimpleStmt - ExpressionStmt\n", fileHeight); }
	| SendStmt                          			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SimpleStmt - SendStmt\n", fileHeight); }
	| IncDecStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SimpleStmt - IncDecStmt\n", fileHeight); }
	| Assignment                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SimpleStmt - Assignment\n", fileHeight); }
	| ShortVarDecl                      			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SimpleStmt - ShortVarDecl\n", fileHeight); }
	;
    
EmptyStmt :
								{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    EmptyStmt - nothing.\n", fileHeight); }
	;
    
LabeledStmt :
	Label ':' Statement                                         { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    LabeledStmt - Label : Statement.\n", fileHeight); }
	;

Label :
	IDENTIFIER                                                  { if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] Label - Identifier.\n", fileHeight, yylval.buffer); }
	;

ExpressionStmt :
	Expression                                                  { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExpressionStmt - Expression.\n", fileHeight); }
	;

SendStmt :
	Channel ARROW_LEFT Expression                                     { if (priority < IMPORTANT_OUTPUT) printf("[%d]    SendStmt - Channel <- Expression.\n", fileHeight); }
	;

Channel :
	Expression                                                  { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Channel - Expression.\n", fileHeight); }
	;
    
IncDecStmt :
	Expression INCREMENT                                             { if (priority < IMPORTANT_OUTPUT) printf("[%d]    IncDecStmt - Expression ++.\n", fileHeight); }
	| Expression DECREMENT                                          { if (priority < IMPORTANT_OUTPUT) printf("[%d]    IncDecStmt - Expression --.\n", fileHeight); }
	;

Assignment :
	ExpressionList ASSIGN_OP ExpressionList                     { if (priority < IMPORTANT_OUTPUT) printf("[%d]    Assignment - ExpressionList assign_op ExpressionList.\n", fileHeight); }
	| ExpressionList '=' ExpressionList				{ if (priority < IMPORTANT_OUTPUT) printf("[%d] Assignment - ExpressionList = ExpressionList.\n", fileHeight); }
	//| IdentifierList '=' ExpressionList				{ if (priority < IMPORTANT_OUTPUT) printf("[%d] Assignment - IdentifierList = ExpressionList.\n", fileHeight); }
	;
    
IfStmt :
	IF Expression Block                                         { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    IfStmt - if Expression Block.\n", fileHeight); }
	
	| IF SimpleStmt ';' Expression Block                            { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    IfStmt - if SimpleStmt Expression Block.\n", fileHeight); }
	
	| IF Expression Block ELSE IfStmt                           { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    IfStmt - if Expression Block else IfStmt.\n", fileHeight); }
	| IF Expression Block ELSE Block                            { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    IfStmt - if Expression Block else Block.\n", fileHeight); }
	
	| IF SimpleStmt ';' Expression Block ELSE IfStmt                { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    IfStmt - if SimpleStmt Expression Block else IfStmt.\n", fileHeight); }
	| IF SimpleStmt ';' Expression Block ELSE Block                 { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    IfStmt - if SimpleStmt Expression Block else Block.\n", fileHeight); }
	;

SwitchStmt :
	ExprSwitchStmt                                              { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SwitchStmt - ExprSwitchStmt\n", fileHeight); }
	| TypeSwitchStmt                                            { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SwitchStmt - TypeSwitchStmt\n", fileHeight); }
	;

ExprSwitchStmt :
	SWITCH '{' ExprCaseClauses '}'                              { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprSwitchStmt - switch { ExprCaseClauses }.\n", fileHeight); }
	| SWITCH SimpleStmt ';' '{' ExprCaseClauses '}'                 { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprSwitchStmt - switch SimpleStmt { ExprCaseClauses }.\n", fileHeight); }
	| SWITCH Expression '{' ExprCaseClauses '}'                 { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprSwitchStmt - switch Expression { ExprCaseClauses }.\n", fileHeight); }
	| SWITCH SimpleStmt ';' Expression '{' ExprCaseClauses '}'      { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprSwitchStmt - switch SimpleStmt Expression { ExprCaseClauses }.\n", fileHeight); }
	;

ExprCaseClauses :
								{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprCaseClauses - nothing.\n", fileHeight); }
	| ExprCaseClauses ExprCaseClause                        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprCaseClauses - ExprCaseClauses ExprCaseClause.\n", fileHeight); }
	;
    
ExprCaseClause :
	ExprSwitchCase ':' StatementList                            { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprCaseClause - ExprSwitchCase : StatementList.\n", fileHeight); }
	;

ExprSwitchCase :
	CASE ExpressionList                                         { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprSwitchCase - case ExpressionList.\n", fileHeight); }
	| DEFAULT                                                   { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ExprSwitchCase - default.\n", fileHeight); }
	;

TypeSwitchStmt :
	SWITCH TypeSwitchGuard '{' TypeCaseClauses '}'                          { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSwitchStmt - switch TypeSwitchGuard { TypeCaseClause }.\n", fileHeight); }
	| SWITCH SimpleStmt ';' TypeSwitchGuard '{' TypeCaseClauses '}'     { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSwitchStmt - switch SimpleStmt ; TypeSwitchGuard { TypeCaseClauses }.\n", fileHeight); }
	;

TypeSwitchGuard :
	PrimaryExpr '.' '(' TYPE ')'                               	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSwitchGuard - PrimaryExpr . ( Type ).\n", fileHeight); }
	| IDENTIFIER ASSIGN PrimaryExpr '.' '(' TYPE ')'              { if (priority < IMPORTANT_OUTPUT) printf("[%d: %s] TypeSwitchGuard - Identifier := PrimaryExpr . ( Type ).\n", fileHeight, yylval.buffer); }
	;
   
TypeCaseClauses :						
									{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeCaseClauses - nothing.\n", fileHeight); }
	| TypeCaseClauses TypeCaseClause                            { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeCaseClauses - TypeCaseClauses TypeCaseClause.\n", fileHeight); }
	;
    
TypeCaseClause :
	TypeSwitchCase ':' StatementList                            { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeCaseClause - TypeSwitchCase : StatementList.\n", fileHeight); }
	;
   
TypeSwitchCase :
	CASE TypeList                                               { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSwitchCase - case TypeList.\n", fileHeight); }
	| DEFAULT                                                   { if (priority < IMPORTANT_OUTPUT) printf("[%d]    TypeSwitchCase - default.\n", fileHeight); }
	;
    
ForStmt :
	FOR Block                                                   { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ForStmt - for Block.\n", fileHeight); }
	| FOR Condition Block                                       { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ForStmt - for Condition Block.\n", fileHeight); }
	| FOR ForClause Block                                       { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ForStmt - for ForClause Block.\n", fileHeight); }
	| FOR RangeClause Block                                     { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ForStmt - for RangeClause Block.\n", fileHeight); }
	;
    
Condition :
	Expression              					{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    Condition - Expression.\n", fileHeight); }
	;
    
ForClause :
	InitStmt ';'           ';' PostStmt                   	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ForClause - InitStmt ; ; PostStmt.\n", fileHeight); }
	| InitStmt ';' Condition ';' PostStmt                   	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ForClause - InitStmt ; Condition ; PostStmt.\n", fileHeight); }
	;
    
InitStmt :
	SimpleStmt                                                  { if (priority < IMPORTANT_OUTPUT) printf("[%d]    InitStmt - SimpleStmt.\n", fileHeight); }
	;
    
PostStmt :
	SimpleStmt                          			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    PostStmt - SimpleStmt.\n", fileHeight); }
	;
    
RangeClause :
	RANGE Expression                                            { if (priority < IMPORTANT_OUTPUT) printf("[%d]    RangeClause - range Expression.\n", fileHeight); }
	| ExpressionList '=' RANGE Expression                       { if (priority < IMPORTANT_OUTPUT) printf("[%d]    RangeClause - ExpressionList = range Expression.\n", fileHeight); }
	| IdentifierList ASSIGN RANGE Expression                      { if (priority < IMPORTANT_OUTPUT) printf("[%d]    RangeClause - IdentifierList := range Expression.\n", fileHeight); }
	;
    
GoStmt :
	GO Expression                                               { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    GoStmt - go Expression.\n", fileHeight); }
	;    

SelectStmt : 
	SELECT '{' CommClauses '}'       				{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SelectStmt - select { CommClauses }.\n", fileHeight); }
	;
    
CommClauses :
									{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    CommClauses - nothing.\n", fileHeight); }
	| CommClauses CommClause                			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    CommClauses - CommClauses CommClause.\n", fileHeight); }
	;
    
CommClause :
	CommCase ':' StatementList              			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    CommClause - CommCase : StatementList.\n", fileHeight); }
	;
    
CommCase :
	CASE SendStmt                           			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    CommCase - SendStmt.\n", fileHeight); }
	| CASE RecvStmt                         			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    CommCase - RecvStmt.\n", fileHeight); }
	| DEFAULT                               			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    CommCase - default.\n", fileHeight); }
	;
    
RecvStmt :
	RecvExpr                                                	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    RecvStmt - RecvExpr.\n", fileHeight); }
	| ExpressionList '=' RecvExpr                           	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    RecvStmt - ExpressionList = RecvExpr.\n", fileHeight); }
	| IdentifierList ASSIGN RecvExpr                          	{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    RecvStmt - IdentifierList := RecvExpr.\n", fileHeight); }
	;
    
RecvExpr :
	Expression                              			{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    RecvExpr - Expression.\n", fileHeight); }
	;
    
ReturnStmt :
	RETURN                                                      { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ReturnStmt - return.\n", fileHeight); }
	| RETURN ExpressionList                                     { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ReturnStmt - return ExpressionList.\n", fileHeight); }
	;

BreakStmt :
	BREAK                                                       { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    BreadStmt - break.\n", fileHeight); }
	| BREAK Label                                               { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    BreakStmt - break Label.\n", fileHeight); }    
	;

ContinueStmt :
	CONTINUE                                                    { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ContinueStmt - continue.\n", fileHeight); }
	| CONTINUE Label                                            { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ContinueStmt - continue Label.\n", fileHeight); }
	;

GotoStmt :
	GOTO Label                                                  { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    GotoStmt - goto Label.\n", fileHeight); }
	;

FallthroughStmt :
	FALLTHROUGH                                                 { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    FallthroughStmt - fallthrough.\n", fileHeight); }
	;
    
DeferStmt :
	DEFER Expression                                            { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    DeferStmt - defer Expression.\n", fileHeight); }
	;

SourceFile :
	PackageClause ';' ImportDecls TopLevelDecls         { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    SourceFile - PackageClause ImportDecls TopLevelDecls.\n", fileHeight); exit(0); }
	;

PackageClause : 
	PACKAGE PackageName                             { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    PackageClause - package PackageName.\n", fileHeight); }
	;
    
PackageName :
	IDENTIFIER			            { if (priority <= IMPORTANT_OUTPUT) printf("[%d: %s] PackageName - identifier.\n", fileHeight, yylval.buffer); }
	;
	
ImportDecls :
								{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ImportDecls - nothing.\n", fileHeight); }
	| ImportDecls ImportDecl ';'                        { if (priority <= IMPORTANT_OUTPUT) printf("[%d]    ImportDecls - ImportDecls ImportDecl.\n", fileHeight); }
	;
    
TopLevelDecls :
								{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    TopLevelDecls - nothing.\n", fileHeight); }
	| TopLevelDecls TopLevelDecl ';'        			{ if (priority <= IMPORTANT_OUTPUT) printf("[%d]    TopLevelDecls - TopLevelDecls TopLevelDecl.\n", fileHeight); }
	;
    
ImportDecl :
	IMPORT ImportSpec                               { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportDecl - import ImportSpec.\n", fileHeight); }
	| IMPORT '(' ImportSpecs ')'                    { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportDecl - import ( ImportSpec ).\n", fileHeight); }
	;
    
ImportSpecs :
							{ if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportSpecs - nothingf.\n", fileHeight); }
	| ImportSpecs ImportSpec ';'                       { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportSpecs - ImportSpecsImportSpec.\n", fileHeight); }
	;

ImportSpec :
	ImportPath                                      { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportSpec - ImportPath.\n", fileHeight); }
	| '.' ImportPath                                { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportSpec - . ImportPath.\n", fileHeight); }
	| PackageName ImportPath                        { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportSpec - PackageName ImportPath.\n", fileHeight); }
	;
    
ImportPath :
	STRING                                          { if (priority < IMPORTANT_OUTPUT) printf("[%d]    ImportPath - string.\n", fileHeight); }
	;

%%

int main() {
	lexOutput = fopen("lexOutput.go", "w");
	if(lexOutput == NULL) exit(0);

	yylval.digit = 0;
    	yylval.buffer[FIELD] = 0;
	yyparse();
	
	fclose(lexOutput);
	return 0;
}
int yyerror(const char* s) {
	if (priority <= IMPORTANT_OUTPUT) printf("[%d]    Error: %s\n", fileHeight, s);
	return 1;
}
int yywrap() {
	return 1;
}