%{
	#include <stdio.h>
	#include <stdlib.h>
	
	int yylex();
	int yyerror();
	int yywrap();
	
	extern FILE* parsed;
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
    	TypeOperandName                          		{ printf("[%d]    Type - TypeOperandName.\n", fileHeight); }
	| TypeLit			                	{ printf("[%d]    Type - TypeLit.\n", fileHeight); }
	;

TypeOperandName :
	IDENTIFIER 			                	{ printf("[%d: %s] TypeOperandName - identifier.\n", fileHeight, yylval.buffer); }
	| IDENTIFIER '.' IDENTIFIER	            		{ printf("[%d: %s]    TypeOperandName - identifier . identifier.\n", fileHeight, yylval.buffer); }
	;
	
TypeList :
	Type                        				{ printf("[%d]    TypeList - Type.\n", fileHeight); }
	| TypeList ',' Type                     		{ printf("[%d]    TypeList - TypeList , Type.\n", fileHeight); }
	;
	
TypeLit :
	ArrayType			                	{ printf("[%d]    TypeLit - ArrayType.\n", fileHeight);}
	| StructType			            		{ printf("[%d]    TypeLit - StructType.\n", fileHeight); }
	| PointerType			            		{ printf("[%d]    TypeLit - PointerType.\n", fileHeight); } 
	| FunctionType			            		{ printf("[%d]    TypeLit - FunctionType.\n", fileHeight); }
	| InterfaceType		                		{ printf("[%d]    TypeLit - InterfaceType.\n", fileHeight); } 
	| SliceType			                	{ printf("[%d]    TypeLit - SliceType.\n", fileHeight); }
	| MapType			                	{ printf("[%d]    TypeLit - MapType.\n", fileHeight); }
	| ChannelType			            		{ printf("[%d]    TypeLit - ChannelType.\n", fileHeight); }
	;

ArrayType :
	'[' ArrayLength ']' ElementType     			{ printf("[%d]    ArrayType - [ ArrayLength      ] ElemenType.\n", fileHeight); }
	;

ArrayLength :
	Expression			                	{ printf("[%d]    ArrayLength - Expression.\n", fileHeight); }
	;
	
ElementType :
	Type				                	{ printf("[%d]    ElementType - Type.\n", fileHeight); }
	;
	
SliceType :
	'[' ']' ElementType                 			{ printf("[%d]    SliceType - [      ] ElemType.\n", fileHeight); }
	;
    
StructType :
	STRUCT '{' FieldDecls '}'				{ printf("[%d]    StructType - struct { FieldDecls }.\n", fileHeight); }
	;
	
FieldDecls :
    	| FieldDecls  FieldDecl ';'             		{ printf("[%d]    FieldDecls - FieldDecls FieldDecl.\n", fileHeight); }
    	;
    
FieldDecl :
	IdentifierListOrNothing Type				{ printf("[%d]    FieldDecl - IdentifierListOrNothing Type.\n", fileHeight); }
	| '*' IdentifierListOrNothing TypeOperandName		{ printf("[%d]    FieldDecl - * IdentifierListOrNothing TypeOperandName.\n", fileHeight); }
	| IdentifierListOrNothing Type Tag			{ printf("[%d]    FieldDecl - IdentifierListOrNothing Type Tag.\n", fileHeight); }
	| '*' IdentifierListOrNothing TypeOperandName Tag	{ printf("[%d]    FieldDecl - * IdentifierListOrNothing TypeOperandName Tag.\n", fileHeight); }
	;
	
IdentifierListOrNothing :
								{ printf("[%d]    IdentifierListOrNothing - nothing.\n", fileHeight); }
	| IdentifierList					{ printf("[%d]    IdentifierListOrNothing - IdentifierList.\n", fileHeight); }
	;
	
Tag : 
	STRING					            	{ printf("[%d]    Tag - string.\n", fileHeight); }
	;

PointerType :
	'*' BaseType                        			{ printf("[%d]    PointerType - * BaseType.\n", fileHeight); }
	;
	
BaseType :
	Type                                			{ printf("[%d]    BaseType - Type.\n", fileHeight); }
	;
    
FunctionType :
	FUNC Signature 		                		{ printf("[%d]    FunctionType - func Signature.\n", fileHeight); }
	;

Signature :
	Parameters						{ printf("[%d]    Signature - Parameters.\n", fileHeight); }
	| Parameters Result			        	{ printf("[%d]    Signature - Parameters Result.\n", fileHeight); }
	;
	
Result :
	Parameters 					        { printf("[%d]    Result - Parameters.\n", fileHeight); }
	| Type						        { printf("[%d]    Result - Type.\n", fileHeight); }
	;
	
Parameters :
	'('')'                             			{ printf("[%d]    Parameters - ( ).\n", fileHeight); }
	| '(' ParameterList ')'             			{ printf("[%d]    Parameters - ( ParameterList ).\n", fileHeight); }
	| '(' ParameterList ',' ')' 	    			{ printf("[%d]    Parameters - ( ParameterList , ).\n", fileHeight); }
	;
	
ParameterList :	
	ParameterDecl						{ printf("[%d]    ParameterList - ParameterDecl.\n", fileHeight); }
	| ParameterList ',' ParameterDecl			{ printf("[%d]    ParameterList - ParameterList , ParameterDecl.\n", fileHeight); }
	;
	
ParameterDecl :	
	IdentifierListOrNothing TypeOrNothing			{ printf("[%d]    ParameterDecl - IdentifierListOrNothing Type.\n", fileHeight); }
	| IdentifierListOrNothing MULTIDOT TypeOrNothing	{ printf("[%d]    ParameterDecl - IdentifierListOrNothing ... Type.\n", fileHeight); }
	;
	
TypeOrNothing :
								{ printf("[%d]    TypeOrNothing - nothing.\n", fileHeight); }
	| Type							{ printf("[%d]    TypeOrNothing - Type.\n", fileHeight); }
	;
		
InterfaceType :
	INTERFACE '{' InterfaceElems '}'         		{ printf("[%d]    InterfaceType - interface { InterfaceElems }.\n", fileHeight); }
	;

InterfaceElems :
								{ printf("[%d]    InterfaceElems - nothing.\n", fileHeight); }
	| InterfaceElems InterfaceElem ';'                 	{ printf("[%d]    InterfaceElems - InterfaceElems InterfaceElem.\n", fileHeight); }
	;
    
InterfaceElem :
	MethodElem                          			{ printf("[%d]    InterfaceElem - MethodElem.\n", fileHeight); }
	| TypeElem                          			{ printf("[%d]    InterfaceElem - TypeElem.\n", fileHeight); }
	;

MethodElem :
	IDENTIFIER Signature                			{ printf("[%d]    MethodElem - MethodName Signature.\n", fileHeight); }
	;
    
TypeElem :
	TypeTerm                            			{ printf("[%d]    TypeElem - TypeTerm.\n", fileHeight); }
	| TypeElem '|' TypeTerm             			{ printf("[%d]    TypeElem - TypeElem | TypeTerm.\n", fileHeight); }
	;
    
TypeTerm :
	Type                                			{ printf("[%d]    TypeTerm - Type\n", fileHeight); }
	| UnderlyingType                    			{ printf("[%d]    TypeTerm - UnderlyingType\n", fileHeight); }
	;

UnderlyingType :
	'~' Type                            			{ printf("[%d]    UnderlyingType - ~ Type.\n", fileHeight); }
	;
    
MapType :
	MAP '[' KeyType ']' ElementType     			{ printf("[%d]    MapType - map [ KeyType      ] ElementType.\n", fileHeight); }
	;

KeyType :
	Type                                			{ printf("[%d]    KeyType - Type.\n", fileHeight); }
	;
    
ChannelType :    
	CHAN ArrowLeftNothing ElementType                    	{ printf("[%d]    ChannelType - chan ElementType.\n", fileHeight); }
	| ARROW_LEFT CHAN ElementType       			{ printf("[%d]    ChannelType - <- chan ElementType.\n", fileHeight); }
	;
	
ArrowLeftNothing :
								{ printf("[%d]    ArrowLeftNothing - nothing.\n", fileHeight); }
	| ARROW_LEFT						{ printf("[%d]    ArrowLeftNothing - <-.\n", fileHeight); }
	;
    
Block :
	'{' StatementList '}'				        { printf("[%d]    Block - { StatementList }.\n", fileHeight); }
	;
    
StatementList :
								{ printf("[%d]    StatementList - nothing ;.\n", fileHeight); }
	| StatementList Statement ';'          			{ printf("[%d]    StatementList - StatementList Statement ;.\n", fileHeight); }
	;
    
Declaration :
	ConstDecl                           			{ printf("[%d]    Declaration - ConstDecl\n", fileHeight); }
	| TypeDecl                          			{ printf("[%d]    Declaration - TypeDecl\n", fileHeight); }
	| VarDecl                           			{ printf("[%d]    Declaration - VarDecl\n", fileHeight); }
	;
    
TopLevelDecl :
	Declaration                         			{ printf("[%d]    TopLevelDecl - Declaration.\n", fileHeight); }
	| FunctionDecl                      			{ printf("[%d]    TopLevelDecl - FunctionDecl.\n", fileHeight); }
	| MethodDecl                        			{ printf("[%d]    TopLevelDecl - MethodDecl.\n", fileHeight); }
	;
    
ConstDecl :
	CONST ConstSpec                                       	{ printf("[%d]    ConstDecl - const ConstSpec.\n", fileHeight); }  
	| CONST '(' ConstSpecs ')'                 		{ printf("[%d]    ConstDecl - const ( ConstSpecs ).\n", fileHeight); }
	;

ConstSpecs :
								{ printf("[%d]    ConstSpecs - nothing.\n", fileHeight); }
	| ConstSpecs ConstSpec ';'                             	{ printf("[%d]    ConstSpecs - ConstSpecs ConstSpec.\n", fileHeight); }
	;

ConstSpec :
	IdentifierList                                      	{ printf("[%d]    ConstSpec - IdentifierList.\n", fileHeight); }
	| IdentifierList '=' ExpressionList                 	{ printf("[%d]    ConstSpec - IdentifierList = ExpressionList.\n", fileHeight); }
	| IdentifierList Type '=' ExpressionList            	{ printf("[%d]    ConstSpec - IdentifierList Type = ExpressionList.\n", fileHeight); }
	;
    
IdentifierList :
	IDENTIFIER                                              { printf("[%d: %s] IdentifierList - Identifier.\n", fileHeight, yylval.buffer); }
	| IdentifierList ',' IDENTIFIER                         { printf("[%d: %s] IdentifierList - IdentifierList Identifier.\n", fileHeight, yylval.buffer); }
	;
	
ExpressionList :
	Expression                                      	{ printf("[%d]    ExpressionList - Expression.\n", fileHeight); }
	| ExpressionList ',' Expression                 	{ printf("[%d]    ExpressionList - ExpressionList , Expression.\n", fileHeight); }
	;
    
TypeDecl :
	TYPE TypeSpec                                           { printf("[%d]    TypeDecl - type TypeSpec.\n", fileHeight); }
	| TYPE '(' TypeSpecs ')'                                { printf("[%d]    TypeDecl - type ( TypeSpecs ).\n", fileHeight); }
	;
    
TypeSpecs :
								{ printf("[%d]    TypeSpecs - nothing.\n", fileHeight); }
	| TypeSpecs TypeSpec ';'				{ printf("[%d]    TypeSpecs - TypeSpecs TypeSpec.\n", fileHeight); }
	;
    
TypeSpec :
	AliasDecl                                               { printf("[%d]    TypeSpec - AliasDecl\n", fileHeight); }
	| TypeDef                                               { printf("[%d]    TypeSpec - TypeDef\n", fileHeight); }
	;

AliasDecl :
	IDENTIFIER '=' Type                                     { printf("[%d: %s] AliasDecl - identifier = Type.\n", fileHeight, yylval.buffer); }
	;    

TypeDef :
	IDENTIFIER Type                                         { printf("[%d: %s] TypeDef - Identifier Type.\n", fileHeight, yylval.buffer); }
	| IDENTIFIER TypeParameters  Type                       { printf("[%d: %s] TypeDef - Identifier TypeParameters Type.\n", fileHeight, yylval.buffer); }
	;
    
TypeParameters :
    	'[' TypeParamList ']'                                   { printf("[%d]    TypeParameters - [ TypeParamList      ].\n", fileHeight); }
	| '[' TypeParamList ',' ']' 		                { printf("[%d]    TypeParameters - [ TypeParamList ,      ].\n", fileHeight); }
	;
	
TypeParamList :
	TypeParamDecl                                          	{ printf("[%d]    TypeParamList - TypeParamDecl.\n", fileHeight); }
	| TypeParamList ',' TypeParamDecl                       { printf("[%d]    TypeParamList - TypeParamList , TypeParamDecl.\n", fileHeight); }
	;

TypeParamDecl :
	IdentifierList TypeConstraint 			        { printf("[%d]    TypeParamDecl - IdentifierList TypeConstraint.\n", fileHeight); }
	;
	
TypeConstraint :
	TypeElem					        { printf("[%d]    TypeConstraint - TypeElem.\n", fileHeight); }
	;

VarDecl :
	VAR VarSpec				    		{ printf("[%d]    VarDecl - VAR VarSpec.\n", fileHeight); }
	| VAR '(' VarSpecs ')'	                    		{ printf("[%d]    VarDecl - VAR ( VarSpec ; ).\n", fileHeight); }
	;
	
VarSpecs :
								{ printf("[%d]    VarSpecs - nothing.\n", fileHeight); }
	| VarSpecs VarSpec ';'                          	{ printf("[%d]    VarSpecs - VarSpecs VarSpec.\n", fileHeight); }
	;
    
VarSpec :
	IdentifierList Type                             	{ printf("[%d]    VarSpec: IdentifierList Type.\n", fileHeight); }
	| IdentifierList Type '=' ExpressionList        	{ printf("[%d]    VarSpec: IdentifierList Type = ExpressionList.\n", fileHeight); }
	| IdentifierList '=' ExpressionList             	{ printf("[%d]    VarSpec: IdentifierList = ExpressionList.\n", fileHeight); }
	;
    
ShortVarDecl :
	IdentifierList ASSIGN ExpressionList                    { printf("[%d]    ShortVarDecl - IdentifierList := ExpressionList.\n", fileHeight); }
	| ExpressionList ASSIGN ExpressionList			{ printf("[%d]    ShortVarDecl - ExpressionList := ExpressionList.\n", fileHeight); }
	;
    
FunctionDecl :
	FUNC IDENTIFIER Signature                     			{ printf("[%d]    FunctionDecl - func FunctionName Signature.\n", fileHeight); }
	| FUNC IDENTIFIER TypeParameters Signature                	{ printf("[%d]    FunctionDecl - func FunctionName TypeParameters Signature.\n", fileHeight); }
	| FUNC IDENTIFIER Signature FunctionBody			{ printf("[%d]    FunctionDecl - func FunctionName Signature FunctionBody.\n", fileHeight); }
	| FUNC IDENTIFIER TypeParameters Signature FunctionBody   	{ printf("[%d]    FunctionDecl - func FunctionName TypeParameters Signature FunctionBody.\n", fileHeight); }
	;

FunctionBody :
	Block		                                        { printf("[%d]    FunctionBody - Block.\n", fileHeight); }
	;
	
MethodDecl :
	FUNC Receiver IDENTIFIER Signature                      { printf("[%d: %s]    MethodDecl - func Receiver identifier Signature.\n", fileHeight, yylval.buffer); }
	| FUNC Receiver IDENTIFIER Signature FunctionBody       { printf("[%d: %s]    MethodDecl - func Receiver identifier Signature FunctionBody.\n", fileHeight, yylval.buffer); }
	;

Receiver :
	Parameters                                             	{ printf("[%d]    Receiver - Parameters.\n", fileHeight); }
	;
    
Operand :
	Literal						        { printf("[%d]    Operand - Literal.\n", fileHeight); }
	| TypeOperandName                       		{ printf("[%d]    Operand - TypeOperandName.\n", fileHeight); }
	| '(' Expression ')'					{ printf("[%d]    Operand - ( Expression ).\n", fileHeight); }
	;
	
PrimaryExpr :
	Operand							{ printf("[%d]    PrimaryExpr - Operand.\n", fileHeight); }
	| PrimaryExpr Selector					{ printf("[%d]    PrimaryExpr - PrimaryExpr Selector.\n", fileHeight); }
	| PrimaryExpr Index					{ printf("[%d]    PrimaryExpr - PrimaryExpr Index.\n", fileHeight); }
	| PrimaryExpr Slice					{ printf("[%d]    PrimaryExpr - PrimaryExpr Slice.\n", fileHeight); }
	| PrimaryExpr TypeAssertion				{ printf("[%d]    PrimaryExpr - PrimaryExpr TypeAssertion.\n", fileHeight); }
	| PrimaryExpr Arguments					{ printf("[%d]    PrimaryExpr - PrimaryExpr Arguments.\n", fileHeight); }
	;	
	
Literal :
	BasicLit				            	{ printf("[%d]    Literal - BasicLit.\n", fileHeight); } 
	| CompositeLit				        	{ printf("[%d]    Literal - CompositeLit.\n", fileHeight); } 
	| FunctionLit				        	{ printf("[%d]    Literal - FunctionLit.\n", fileHeight); } 
	;
	
BasicLit :
	INTEGER							{ printf("[%d: %s]     BasicLit - Integer.\n", fileHeight, yylval.buffer); } 
	| FLOAT							{ printf("[%d: %s]     BasicLit - Float.\n", fileHeight, yylval.buffer); } 
	| IMAGINARY						{ printf("[%d: %s]     BasicLit - Imaginary.\n", fileHeight, yylval.buffer); } 
	| RUNE							{ printf("[%d: %s]     BasicLit - Rune.\n", fileHeight, yylval.buffer); } 
	| STRING						{ printf("[%d: %s]     BasicLit - String.\n", fileHeight, yylval.buffer); } 
	;
	
CompositeLit :
	LiteralType LiteralValue				{ printf("[%d]    CompositeLit - LiteralType LiteralValue.\n", fileHeight); }
	;
	
LiteralType :
	StructType					        { printf("[%d]    LiteralType - StructType.\n", fileHeight); }
	| ArrayType					        { printf("[%d]    LiteralType - ArrayType.\n", fileHeight); }
	| '[' MULTIDOT ']' ElementType				{ printf("[%d]    LiteralType - [ ...      ] ElementType.\n", fileHeight); }
	| SliceType					        { printf("[%d]    LiteralType - SliceType.\n", fileHeight); }
	| MapType					        { printf("[%d]    LiteralType - MapType.\n", fileHeight); }
	| TypeOperandName                          		{ printf("[%d]    LiteralType - TypeOperandName.\n", fileHeight); }
	;
	
LiteralValue :
	'{' '}'                         			{ printf("[%d]    LiteralValue - { }.\n", fileHeight); }
	| '{' ElementList '}'                           	{ printf("[%d]    LiteralValue - { ElementList }.\n", fileHeight); }
	| '{' ElementList ',' '}'                           	{ printf("[%d]    LiteralValue - { ElementList , }.\n", fileHeight); }
	;
	
ElementList :
	KeyedElement						{ printf("[%d]    ElementList - KeyedElement.\n", fileHeight); }
	| ElementList ',' KeyedElement				{ printf("[%d]    ElementList - ElementList , KeyedElement.\n", fileHeight); }
	;
    
KeyedElement :
	Element                             			{ printf("[%d]    KeyedElement - Element.\n", fileHeight); }
	| Key ':' Element                               	{ printf("[%d]    KeyedElement - Key : Element.\n", fileHeight); }
	;
 	
Key :
	Expression					    	{ printf("[%d]    Key - Expression.\n", fileHeight); } 
 	| LiteralValue					    	{ printf("[%d]    Key - LiteralValue.\n", fileHeight); }
 	;

Element :
	Expression					        { printf("[%d]    Element - Expression.\n", fileHeight); }
	| LiteralValue					    	{ printf("[%d]    Element - LiteralValue.\n", fileHeight); }
	;	
			
FunctionLit :
	FUNC Signature FunctionBody         			{ printf("[%d]    FunctionLit - func Signature FucntionBody.\n", fileHeight); }
	;
	
Selector :
	'.' IDENTIFIER						{ printf("[%d: %s] Selector - . identifier.\n", fileHeight, yylval.buffer);}
	;

Index :
	'[' Expression ']'					{ printf("[%d]    Index - Expression.\n", fileHeight); }
	;
	
Slice :
	'[' ':' ']'                                 		{ printf("[%d]    Slice - [ ].\n", fileHeight); }
	| '[' Expression ':' ']'                                { printf("[%d]    Slice - [ Expression ].\n", fileHeight); }
	| '[' ':' Expression ']'                                { printf("[%d]    Slice - [ : Expression      ].\n", fileHeight); }
	| '[' Expression ':' Expression ']'                     { printf("[%d]    Slice - [ Expression : Expression      ].\n", fileHeight); }
	| '[' ':' Expression ':' Expression ']'                 { printf("[%d]    Slice - [ : Expression : Expression      ].\n", fileHeight); }
	| '[' Expression ':' Expression ':' Expression ']'      { printf("[%d]    Slice - [ Expression : Expression : Expression      ].\n", fileHeight); }
	;
	
TypeAssertion :
	'.' '(' Type ')'				    	{ printf("[%d]    TypeAssertion - . ( Type ).\n", fileHeight); }
	;

Arguments :
	'(' ')'                                 		{ printf("[%d]    Arguments: ( ).\n", fileHeight);  }
	| '(' ExpressionList ')'                                { printf("[%d]    Arguments: ( ExpressionList ).\n", fileHeight);  }
	| '(' ExpressionList MULTIDOT ')'                       { printf("[%d]    Arguments: ( ExpressionList ... ).\n", fileHeight);  }
	| '(' ExpressionList ',' ')'                            { printf("[%d]    Arguments: ( ExpressionList , ).\n", fileHeight);  }
	| '(' ExpressionList MULTIDOT ',' ')'                   { printf("[%d]    Arguments: ( ExpressionList ... , ).\n", fileHeight);  }    

	| '(' Type ')'                                  	{ printf("[%d]    Arguments: ( Type ).\n", fileHeight);  }
	| '(' Type MULTIDOT ')'                                 { printf("[%d]    Arguments: ( Type ... ).\n", fileHeight);  }
	| '(' Type ',' ')'                                  	{ printf("[%d]    Arguments: ( Type , ).\n", fileHeight);  }
	| '(' Type MULTIDOT ',' ')'                             { printf("[%d]    Arguments: ( Type ... , ).\n", fileHeight);  }   

	| '(' Type ',' ExpressionList ')'                       { printf("[%d]    Arguments: ( Type , ExpressionList ).\n", fileHeight);  }
	| '(' Type ',' ExpressionList MULTIDOT ')'              { printf("[%d]    Arguments: ( Type , ExpressionList ... ).\n", fileHeight);  }
	| '(' Type ',' ExpressionList ',' ')'                   { printf("[%d]    Arguments: ( Type ExpressionList , ).\n", fileHeight);  }
	| '(' Type ',' ExpressionList MULTIDOT ',' ')'          { printf("[%d]    Arguments: ( Type ExpressionList ... , ).\n", fileHeight);  }
	;
    
Expression :
	UnaryExpr						{ printf("[%d]    Expression - UnaryExpr.\n", fileHeight); }
	| Expression binary_op Expression			{ printf("[%d]    Expression - Expression binary_op Expression.\n", fileHeight); }
	;
	
UnaryExpr :
	PrimaryExpr						{ printf("[%d]    UnaryExpr - PrimaryExpr.\n", fileHeight); }
	| unary_op UnaryExpr					{ printf("[%d]    UnaryExpr - unary_op UnaryExpr.\n", fileHeight); }
	;
	
binary_op :
	OR							{ printf("[%d]    Binary_operator - ||.\n", fileHeight); }
	| AND							{ printf("[%d]    Binary_operator - &&.\n", fileHeight); }
	| rel_op					        { printf("[%d]    Binary_operator - Rel_op.\n", fileHeight); }
	| add_op					        { printf("[%d]    Binary_operator - Add_op.\n", fileHeight); }
	| mul_op						{ printf("[%d]    Binary_operator - Mul_op.\n", fileHeight); }
	;
	
rel_op :
	EQUALL							{ printf("[%d]    Relativeness_operator - ==.\n", fileHeight); }
	| NOT_EQUALL						{ printf("[%d]    Relativeness_operator - !=.\n", fileHeight); }
	| '<'							{ printf("[%d]    Relativeness_operator - <.\n", fileHeight); }
	| LESS_EQUALL						{ printf("[%d]    Relativeness_operator - <=.\n", fileHeight); }
	| '>'							{ printf("[%d]    Relativeness_operator - >.\n", fileHeight); }
	| GREATER_EQUALL					{ printf("[%d]    Relativeness_operator - >=.\n", fileHeight); }
	;
	
add_op :
	'+'							{ printf("[%d]    Addition_operator - +.\n", fileHeight); }
	| '-'							{ printf("[%d]    Addition_operator - -.\n", fileHeight); }
	| '|'							{ printf("[%d]    Addition_operator - |.\n", fileHeight); }
	| '^'							{ printf("[%d]    Addition_operator - ^.\n", fileHeight); }
	;
	
mul_op :
	'*'							{ printf("[%d]    Multiply_operator - *.\n", fileHeight); }
	| '/'							{ printf("[%d]    Multiply_operator - /.\n", fileHeight); }
	| '%'							{ printf("[%d]    Multiply_operator - %%.\n", fileHeight); }
	| SHIFT_LEFT						{ printf("[%d]    Multiply_operator - <<.\n", fileHeight); }
	| SHIFT_RIGHT						{ printf("[%d]    Multiply_operator - >>.\n", fileHeight); }
	| '&'							{ printf("[%d]    Multiply_operator - &.\n", fileHeight); }
	| AND_XOR						{ printf("[%d]    Multiply_operator - &^.\n", fileHeight); }
	;
	
unary_op :
	'+'							{ printf("[%d]    Unary_operator - +.\n", fileHeight); }
	| '-'							{ printf("[%d]    Unary_operator - -.\n", fileHeight); }
	| '!'							{ printf("[%d]    Unary_operator - !.\n", fileHeight); }
	| '^'							{ printf("[%d]    Unary_operator - ^.\n", fileHeight); }
	| '*'							{ printf("[%d]    Unary_operator - *.\n", fileHeight); }
	| '&'							{ printf("[%d]    Unary_operator - &.\n", fileHeight); }
	| ARROW_LEFT						{ printf("[%d]    Unary_operator - <-.\n", fileHeight); }
	;
    
Statement :
	Declaration                         			{ printf("[%d]    Statement - Declaration\n", fileHeight); }
	| LabeledStmt                       			{ printf("[%d]    Statement - LabeledStmt\n", fileHeight); }
	| SimpleStmt                        			{ printf("[%d]    Statement - SimpleStmt\n", fileHeight); }
	| GoStmt                            			{ printf("[%d]    Statement - GoStmt\n", fileHeight); }
	| ReturnStmt                        			{ printf("[%d]    Statement - ReturnStmt\n", fileHeight); }
	| BreakStmt                         			{ printf("[%d]    Statement - BreakStmt\n", fileHeight); }
	| ContinueStmt                      			{ printf("[%d]    Statement - ContinueStmt\n", fileHeight); }
	| GotoStmt                          			{ printf("[%d]    Statement - GotoStmt\n", fileHeight); }
	| FallthroughStmt                   			{ printf("[%d]    Statement - FallthroughStmt\n", fileHeight); }
	| Block                             			{ printf("[%d]    Statement - Block\n", fileHeight); }
	| IfStmt                            			{ printf("[%d]    Statement - IfStmt\n", fileHeight); }
	| SwitchStmt                        			{ printf("[%d]    Statement - SwitchStmt\n", fileHeight); }
	| SelectStmt                        			{ printf("[%d]    Statement - SelectStmt\n", fileHeight); }
	| ForStmt                           			{ printf("[%d]    Statement - ForStmt\n", fileHeight); }
	| DeferStmt                         			{ printf("[%d]    Statement - DeferStmt\n", fileHeight); }
	;
    
SimpleStmt :
	EmptyStmt                           			{ printf("[%d]    SimpleStmt - EmptyStmt\n", fileHeight); }
	| ExpressionStmt                    			{ printf("[%d]    SimpleStmt - ExpressionStmt\n", fileHeight); }
	| SendStmt                          			{ printf("[%d]    SimpleStmt - SendStmt\n", fileHeight); }
	| IncDecStmt                        			{ printf("[%d]    SimpleStmt - IncDecStmt\n", fileHeight); }
	| Assignment                        			{ printf("[%d]    SimpleStmt - Assignment\n", fileHeight); }
	| ShortVarDecl                      			{ printf("[%d]    SimpleStmt - ShortVarDecl\n", fileHeight); }
	;
    
EmptyStmt :
								{ printf("[%d]    EmptyStmt - nothing.\n", fileHeight); }
	;
    
LabeledStmt :
	Label ':' Statement                                     { printf("[%d]    LabeledStmt - Label : Statement.\n", fileHeight); }
	;

Label :
	IDENTIFIER                                              { printf("[%d: %s] Label - Identifier.\n", fileHeight, yylval.buffer); }
	;

ExpressionStmt :
	Expression                                              { printf("[%d]    ExpressionStmt - Expression.\n", fileHeight); }
	;

SendStmt :
	Channel ARROW_LEFT Expression                           { printf("[%d]    SendStmt - Channel <- Expression.\n", fileHeight); }
	;

Channel :
	Expression                                              { printf("[%d]    Channel - Expression.\n", fileHeight); }
	;
    
IncDecStmt :
	Expression INCREMENT                                    { printf("[%d]    IncDecStmt - Expression ++.\n", fileHeight); }
	| Expression DECREMENT                                  { printf("[%d]    IncDecStmt - Expression --.\n", fileHeight); }
	;

Assignment :
	ExpressionList ASSIGN_OP ExpressionList                 { printf("[%d]    Assignment - ExpressionList assign_op ExpressionList.\n", fileHeight); }
	| ExpressionList '=' ExpressionList			{ printf("[%d] Assignment - ExpressionList = ExpressionList.\n", fileHeight); }
	;
    
IfStmt :
	IF Expression Block                                     { printf("[%d]    IfStmt - if Expression Block.\n", fileHeight); }
	
	| IF SimpleStmt ';' Expression Block                    { printf("[%d]    IfStmt - if SimpleStmt Expression Block.\n", fileHeight); }
	
	| IF Expression Block ELSE IfStmt                       { printf("[%d]    IfStmt - if Expression Block else IfStmt.\n", fileHeight); }
	| IF Expression Block ELSE Block                        { printf("[%d]    IfStmt - if Expression Block else Block.\n", fileHeight); }
	
	| IF SimpleStmt ';' Expression Block ELSE IfStmt        { printf("[%d]    IfStmt - if SimpleStmt Expression Block else IfStmt.\n", fileHeight); }
	| IF SimpleStmt ';' Expression Block ELSE Block         { printf("[%d]    IfStmt - if SimpleStmt Expression Block else Block.\n", fileHeight); }
	;

SwitchStmt :
	ExprSwitchStmt                                          { printf("[%d]    SwitchStmt - ExprSwitchStmt\n", fileHeight); }
	| TypeSwitchStmt                                       	{ printf("[%d]    SwitchStmt - TypeSwitchStmt\n", fileHeight); }
	;

ExprSwitchStmt :
	SWITCH '{' ExprCaseClauses '}'                              	{ printf("[%d]    ExprSwitchStmt - switch { ExprCaseClauses }.\n", fileHeight); }
	| SWITCH SimpleStmt ';' '{' ExprCaseClauses '}'                 { printf("[%d]    ExprSwitchStmt - switch SimpleStmt { ExprCaseClauses }.\n", fileHeight); }
	| SWITCH Expression '{' ExprCaseClauses '}'                 	{ printf("[%d]    ExprSwitchStmt - switch Expression { ExprCaseClauses }.\n", fileHeight); }
	| SWITCH SimpleStmt ';' Expression '{' ExprCaseClauses '}'      { printf("[%d]    ExprSwitchStmt - switch SimpleStmt Expression { ExprCaseClauses }.\n", fileHeight); }
	;

ExprCaseClauses :
								{ printf("[%d]    ExprCaseClauses - nothing.\n", fileHeight); }
	| ExprCaseClauses ExprCaseClause                        { printf("[%d]    ExprCaseClauses - ExprCaseClauses ExprCaseClause.\n", fileHeight); }
	;
    
ExprCaseClause :
	ExprSwitchCase ':' StatementList                        { printf("[%d]    ExprCaseClause - ExprSwitchCase : StatementList.\n", fileHeight); }
	;

ExprSwitchCase :
	CASE ExpressionList                                     { printf("[%d]    ExprSwitchCase - case ExpressionList.\n", fileHeight); }
	| DEFAULT                                              	{ printf("[%d]    ExprSwitchCase - default.\n", fileHeight); }
	;

TypeSwitchStmt :
	SWITCH TypeSwitchGuard '{' TypeCaseClauses '}'          		{ printf("[%d]    TypeSwitchStmt - switch TypeSwitchGuard { TypeCaseClause }.\n", fileHeight); }
	| SWITCH SimpleStmt ';' TypeSwitchGuard '{' TypeCaseClauses '}'    	{ printf("[%d]    TypeSwitchStmt - switch SimpleStmt ; TypeSwitchGuard { TypeCaseClauses }.\n", fileHeight); }
	;

TypeSwitchGuard :
	PrimaryExpr '.' '(' TYPE ')'                            { printf("[%d]    TypeSwitchGuard - PrimaryExpr . ( Type ).\n", fileHeight); }
	| IDENTIFIER ASSIGN PrimaryExpr '.' '(' TYPE ')'        { printf("[%d: %s] TypeSwitchGuard - Identifier := PrimaryExpr . ( Type ).\n", fileHeight, yylval.buffer); }
	;
   
TypeCaseClauses :						
								{ printf("[%d]    TypeCaseClauses - nothing.\n", fileHeight); }
	| TypeCaseClauses TypeCaseClause                        { printf("[%d]    TypeCaseClauses - TypeCaseClauses TypeCaseClause.\n", fileHeight); }
	;
    
TypeCaseClause :
	TypeSwitchCase ':' StatementList                        { printf("[%d]    TypeCaseClause - TypeSwitchCase : StatementList.\n", fileHeight); }
	;
   
TypeSwitchCase :
	CASE TypeList                                           { printf("[%d]    TypeSwitchCase - case TypeList.\n", fileHeight); }
	| DEFAULT                                               { printf("[%d]    TypeSwitchCase - default.\n", fileHeight); }
	;
    
ForStmt :
	FOR Block                                               { printf("[%d]    ForStmt - for Block.\n", fileHeight); }
	| FOR Condition Block                                   { printf("[%d]    ForStmt - for Condition Block.\n", fileHeight); }
	| FOR ForClause Block                                   { printf("[%d]    ForStmt - for ForClause Block.\n", fileHeight); }
	| FOR RangeClause Block                               	{ printf("[%d]    ForStmt - for RangeClause Block.\n", fileHeight); }
	;
    
Condition :
	Expression              				{ printf("[%d]    Condition - Expression.\n", fileHeight); }
	;
    
ForClause :
	InitStmt ';'           ';' PostStmt                   	{ printf("[%d]    ForClause - InitStmt ; ; PostStmt.\n", fileHeight); }
	| InitStmt ';' Condition ';' PostStmt                	{ printf("[%d]    ForClause - InitStmt ; Condition ; PostStmt.\n", fileHeight); }
	;
    
InitStmt :
	SimpleStmt                                            	{ printf("[%d]    InitStmt - SimpleStmt.\n", fileHeight); }
	;
    
PostStmt :
	SimpleStmt                          			{ printf("[%d]    PostStmt - SimpleStmt.\n", fileHeight); }
	;
    
RangeClause :
	RANGE Expression                                      	{ printf("[%d]    RangeClause - range Expression.\n", fileHeight); }
	| ExpressionList '=' RANGE Expression                  	{ printf("[%d]    RangeClause - ExpressionList = range Expression.\n", fileHeight); }
	| IdentifierList ASSIGN RANGE Expression               	{ printf("[%d]    RangeClause - IdentifierList := range Expression.\n", fileHeight); }
	| ExpressionList ASSIGN RANGE Expression
	;
    
GoStmt :
	GO Expression                                          	{ printf("[%d]    GoStmt - go Expression.\n", fileHeight); }
	;    

SelectStmt : 
	SELECT '{' CommClauses '}'       			{ printf("[%d]    SelectStmt - select { CommClauses }.\n", fileHeight); }
	;
    
CommClauses :
								{ printf("[%d]    CommClauses - nothing.\n", fileHeight); }
	| CommClauses CommClause                		{ printf("[%d]    CommClauses - CommClauses CommClause.\n", fileHeight); }
	;
    
CommClause :
	CommCase ':' StatementList              		{ printf("[%d]    CommClause - CommCase : StatementList.\n", fileHeight); }
	;
    
CommCase :
	CASE SendStmt                           		{ printf("[%d]    CommCase - SendStmt.\n", fileHeight); }
	| CASE RecvStmt                         		{ printf("[%d]    CommCase - RecvStmt.\n", fileHeight); }
	| DEFAULT                               		{ printf("[%d]    CommCase - default.\n", fileHeight); }
	;
    
RecvStmt :
	RecvExpr                                              	{ printf("[%d]    RecvStmt - RecvExpr.\n", fileHeight); }
	| ExpressionList '=' RecvExpr                         	{ printf("[%d]    RecvStmt - ExpressionList = RecvExpr.\n", fileHeight); }
	| IdentifierList ASSIGN RecvExpr                        { printf("[%d]    RecvStmt - IdentifierList := RecvExpr.\n", fileHeight); }
	;
    
RecvExpr :
	Expression                              		{ printf("[%d]    RecvExpr - Expression.\n", fileHeight); }
	;
    
ReturnStmt :
	RETURN                                              	{ printf("[%d]    ReturnStmt - return.\n", fileHeight); }
	| RETURN ExpressionList                             	{ printf("[%d]    ReturnStmt - return ExpressionList.\n", fileHeight); }
	;

BreakStmt :
	BREAK                                               	{ printf("[%d]    BreadStmt - break.\n", fileHeight); }
	| BREAK Label                                       	{ printf("[%d]    BreakStmt - break Label.\n", fileHeight); }    
	;

ContinueStmt :
	CONTINUE                                            	{ printf("[%d]    ContinueStmt - continue.\n", fileHeight); }
	| CONTINUE Label                                    	{ printf("[%d]    ContinueStmt - continue Label.\n", fileHeight); }
	;

GotoStmt :
	GOTO Label                                          	{ printf("[%d]    GotoStmt - goto Label.\n", fileHeight); }
	;

FallthroughStmt :
	FALLTHROUGH                                         	{ printf("[%d]    FallthroughStmt - fallthrough.\n", fileHeight); }
	;
    
DeferStmt :
	DEFER Expression                                      	{ printf("[%d]    DeferStmt - defer Expression.\n", fileHeight); }
	;

SourceFile :
	PackageClause ';' ImportDecls TopLevelDecls         
	{ 
		printf("[%d]    SourceFile - PackageClause ImportDecls TopLevelDecls.\n", fileHeight); 
		printf(" ---------- Completed !!! ---------- \n" ); exit(0); }
	;

PackageClause : 
	PACKAGE PackageName                             	{ printf("[%d]    PackageClause - package PackageName.\n", fileHeight); }
	;
    
PackageName :
	IDENTIFIER			            		{ printf("[%d: %s] PackageName - identifier.\n", fileHeight, yylval.buffer); }
	;
	
ImportDecls :
								{ printf("[%d]    ImportDecls - nothing.\n", fileHeight); }
	| ImportDecls ImportDecl ';'                        	{ printf("[%d]    ImportDecls - ImportDecls ImportDecl.\n", fileHeight); }
	;
    
TopLevelDecls :
								{ printf("[%d]    TopLevelDecls - nothing.\n", fileHeight); }
	| TopLevelDecls TopLevelDecl ';'        		{ printf("[%d]    TopLevelDecls - TopLevelDecls TopLevelDecl ;.\n", fileHeight); }
	| TopLevelDecls TopLevelDecl				{ printf("[%d]    TopLevelDecls - TopLevelDecls TopLevelDecl.\n", fileHeight); }
	;
    
ImportDecl :
	IMPORT ImportSpec                              		{ printf("[%d]    ImportDecl - import ImportSpec.\n", fileHeight); }
	| IMPORT '(' ImportSpecs ')'                    	{ printf("[%d]    ImportDecl - import ( ImportSpec ).\n", fileHeight); }
	;
    
ImportSpecs :
								{ printf("[%d]    ImportSpecs - nothingf.\n", fileHeight); }
	| ImportSpecs ImportSpec ';'                       	{ printf("[%d]    ImportSpecs - ImportSpecsImportSpec.\n", fileHeight); }
	;

ImportSpec :
	ImportPath                                      	{ printf("[%d]    ImportSpec - ImportPath.\n", fileHeight); }
	| '.' ImportPath                                	{ printf("[%d]    ImportSpec - . ImportPath.\n", fileHeight); }
	| PackageName ImportPath                        	{ printf("[%d]    ImportSpec - PackageName ImportPath.\n", fileHeight); }
	;
    
ImportPath :
	STRING                                          	{ printf("[%d]    ImportPath - string.\n", fileHeight); }
	;

%%

int main() {
	parsed = fopen("parsed.go", "w");
	if(parsed == NULL) exit(0);

	yylval.digit = 0;
	yyparse();
	
	fclose(parsed);
	return 0;
}
int yyerror(const char* s) {
	printf("[%d]    Error: %s\n", fileHeight, s);
	return 1;
}
int yywrap() {
	return 1;
}