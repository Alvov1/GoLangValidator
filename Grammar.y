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
	
%}

%union {
	int digit;
	char buffer[5 + 1];
}

%token IDENTIFIER DIGIT BOOL_VALUE NIL PACKAGE 
%token GOTO FALLTHROUGH DEFER CHAN IMPORT FUNC BREAK CASE CONST
%token CONTINUE DEFAULT ELSE FOR GO IF RANGE RETURN STRUCT 
%token HEX_BYTE LITTLE_U BIG_U ESCAPED CHAR SELECT OCT HEX OCT_BYTE
%token ASSIGN OR AND EQUALL NOT_EQUALL LESS_EQUALL GREATER_EQUALL 
%token SHIFT_LEFT SHIFT_RIGHT AND_XOR MULTIDOT INCREMENT DECREMENT
%token SWITCH TYPE VAR STRING ARROW_LEFT INTERFACE MAP CHANGE_ASSIGN

%left '+' '-' '*' '/' '%' '&' '|' '^'
%right '=' '!'
%left AND OR EQUALL NOT_EQUALL LESS_EQUALL GREATER_EQUALL ';' ':' INCREMENT DECREMENT
%left '>' '<' '(' ')' '{' '}' '[' ']' ',' '.'


%%

SourceFile :
    PackageClause ';' ImportDecls TopLevelDecls         { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SourceFile - PackageClause ImportDecls TopLevelDecls.\n"); exit(0); }
    ;
    
PackageClause : 
    PACKAGE PackageName                             { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  PackageClause - package PackageName.\n"); }
    ;
    
PackageName :
	IDENTIFIER			            { if (priority <= IMPORTANT_OUTPUT) printf("[%s] PackageName - identifier.\n", yylval.buffer); }
	;
	
ImportDecls :
    | ImportDecls ImportDecl ';'                        { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ImportDecls - ImportDecls ImportDecl.\n"); }
    ;
    
ImportDecl :
    IMPORT ImportSpec                               { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ImportDecl - import ImportSpec.\n"); }
    | IMPORT '(' ImportSpecs ')'                    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ImportDecl - import ( ImportSpec ).\n"); }
    ;
    
ImportSpecs :
    | ImportSpecs ImportSpec ';'                       { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ImportSpecs - ImportSpecsImportSpec.\n"); }
    ;

ImportSpec :
    ImportPath                                      { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ImportSpec - ImportPath.\n"); }
    | '.' ImportPath                                { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ImportSpec - . ImportPath.\n"); }
    | PackageName ImportPath                        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ImportSpec - PackageName ImportPath.\n"); }
    ;
    
ImportPath :
    STRING                                          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ImportPath - string.\n"); }
    ;

VarDecl :
	VAR VarSpec				    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  VarDecl - VAR VarSpec.\n"); exit(0); }
	| VAR '(' VarSpecs ')'	                    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  VarDecl - VAR ( VarSpec ; ).\n"); exit(0); }
	;
	
VarSpecs :
    | VarSpecs VarSpec ';'                          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  VarSpecs - VarSpecs VarSpec.\n"); }
    ;
    
VarSpec :
    IdentifierList Type                             { if (priority < IMPORTANT_OUTPUT) printf("[     ]  VarSpec: IdentifierList Type.\n"); }
    | IdentifierList Type '=' ExpressionList        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  VarSpec: IdentifierList Type = ExpressionList.\n"); }
    | IdentifierList Type 			    { if (priority < IMPORTANT_OUTPUT) printf("[     ] 	VarSpec: IdentifierList Type.\n"); }
    | IdentifierList '=' ExpressionList             { if (priority < IMPORTANT_OUTPUT) printf("[     ]  VarSpec: IdentifierList = ExpressionList.\n"); }
    ;
	
	
IdentifierList :
    IDENTIFIER                                              	{ if (priority < IMPORTANT_OUTPUT) printf("[%s] IdentifierList - Identifier.\n", yylval.buffer); }
    | IdentifierList ',' IDENTIFIER                         	{ if (priority < IMPORTANT_OUTPUT) printf("[%s] IdentifierList - IdentifierList Identifier.\n", yylval.buffer); }
    ;
	
ExpressionList :
    Expression                                      { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExpressionList - Expression.\n"); }
    | ExpressionList ',' Expression                 { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExpressionList - ExpressionList , Expression.\n"); }
    ;

Expression :
	UnaryExpr					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Expression - UnaryExpr.\n"); }
	| Expression binary_op Expression		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Expression - Expression binary_op Expression.\n"); }
	;
	
UnaryExpr :
	PrimaryExpr					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  UnaryExpr - PrimaryExpr.\n"); }
	| unary_op UnaryExpr				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  UnaryExpr - unary_op UnaryExpr.\n"); }
	;
	
binary_op :
	OR						{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Binary_operator - ||.\n"); }
	| AND						{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Binary_operator - &&.\n"); }
	| rel_op					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Binary_operator - Rel_op.\n"); }
	| add_op					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Binary_operator - Add_op.\n"); }
	| mul_op						    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Binary_operator - Mul_op.\n"); }
	;
	
rel_op :
	EQUALL							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Relativeness_operator - ==.\n"); }
	| NOT_EQUALL					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Relativeness_operator - !=.\n"); }
	| '<'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Relativeness_operator - <.\n"); }
	| LESS_EQUALL							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Relativeness_operator - <=.\n"); }
	| '>'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Relativeness_operator - >.\n"); }
	| GREATER_EQUALL							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Relativeness_operator - >=.\n"); }
	;
	
add_op :
	'+'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Addition_operator - +.\n"); }
	| '-'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Addition_operator - -.\n"); }
	| '|'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Addition_operator - |.\n"); }
	| '^'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Addition_operator - ^.\n"); }
	;
	
mul_op :
	'*'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Multiply_operator - *.\n"); }
	| '/'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Multiply_operator - /.\n"); }
	| '%'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Multiply_operator - %%.\n"); }
	| SHIFT_LEFT							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Multiply_operator - <<.\n"); }
	| SHIFT_RIGHT							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Multiply_operator - >>.\n"); }
	| '&'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Multiply_operator - &.\n"); }
	| AND_XOR							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Multiply_operator - &^.\n"); }
	;
	
unary_op :
	'+'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Unary_operator - +.\n"); }
	| '-'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Unary_operator - -.\n"); }
	| '!'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Unary_operator - !.\n"); }
	| '^'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Unary_operator - ^.\n"); }
	| '*'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Unary_operator - *.\n"); }
	| '&'							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Unary_operator - &.\n"); }
	| ARROW_LEFT							    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Unary_operator - <-.\n"); }
	;

PrimaryExpr :
	Operand							{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - Operand.\n"); }
	| Conversion						{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - Conversion.\n"); }
	| MethodExpr						{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - MethodExpr.\n"); }
	| PrimaryExpr Selector					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - PrimaryExpr Selector.\n"); }
	| PrimaryExpr Index					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - PrimaryExpr Index.\n"); }
	| PrimaryExpr Slice					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - PrimaryExpr Slice.\n"); }
	| PrimaryExpr TypeAssertion				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - PrimaryExpr TypeAssertion.\n"); }
	| PrimaryExpr Arguments					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PrimaryExpr - PrimaryExpr Arguments.\n"); }
	;
	
Selector :
	'.' IDENTIFIER						{ if (priority < IMPORTANT_OUTPUT) printf("[%s] Selector - . identifier.\n", yylval.buffer);}
	;

Index :
    '[' Expression ']'						{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Index - Expression.\n"); }
    ;
	
Slice :
    '[' ':' ']'                                 		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Slice - [ : ].\n"); }
    | '[' Expression ':' ']'                                    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Slice - [ Expression : ].\n"); }
    | '[' ':' Expression ']'                                    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Slice - [ : Expression ].\n"); }
    | '[' Expression ':' Expression ']'                         { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Slice - [ Expression : Expression ].\n"); }
    | '[' ':' Expression ':' Expression ']'                     { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Slice - [ : Expression : Expression ].\n"); }
    | '[' Expression ':' Expression ':' Expression ']'          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Slice - [ Expression : Expression : Expression ].\n"); }
    ;
	
TypeAssertion :
	'.' '(' Type ')'				    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeAssertion - . ( Type ).\n"); }
	;
	
Arguments :
    '(' ')'                                 			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( ).\n");  }
    | '(' ExpressionList ')'                                    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( ExpressionList ).\n");  }
    | '(' ExpressionList MULTIDOT ')'                              { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( ExpressionList ... ).\n");  }
    | '(' ExpressionList ',' ')'                                { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( ExpressionList , ).\n");  }
    | '(' ExpressionList MULTIDOT ',' ')'                          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( ExpressionList ... , ).\n");  }    
    | '(' Type ')'                                  		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type ).\n");  }
    | '(' Type MULTIDOT ')'                                    	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type ... ).\n");  }
    | '(' Type ',' ')'                                  	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type , ).\n");  }
    | '(' Type MULTIDOT ',' ')'                                    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type ... , ).\n");  }   
    | '(' Type ',' ExpressionList ')'                           { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type , ExpressionList ).\n");  }
    | '(' Type ',' ExpressionList MULTIDOT ')'                     { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type , ExpressionList ... ).\n");  }
    | '(' Type ',' ExpressionList ',' ')'                       { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type ExpressionList , ).\n");  }
    | '(' Type ',' ExpressionList MULTIDOT ',' ')'                 { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Arguments: ( Type ExpressionList ... , ).\n");  }
    ;
	
Operand :
	Literal						        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Operand - Literal.\n"); }
	| OperandName                       			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Operand - OperandName.\n"); }
	| OperandName TypeArgs		        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Operand - OperandName TypeArgs.\n"); } 
	| '(' Expression ')'					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Operand - ( Expression ).\n"); }
	;
	
Literal :
	BasicLit				            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Literal - BasicLit.\n"); } 
	| CompositeLit				        	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Literal - CompositeLit.\n"); } 
	| FunctionLit				        	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Literal - FunctionLit.\n"); } 
	;
	
FunctionLit :
    FUNC Signature FunctionBody         			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  FunctionLit - func Signature FucntionBody.\n"); }
    ;
	
BasicLit :
	int_lit					            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  BasicLit - Int_lit.\n"); }
	| float_lit				            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  BasicLit - Float_lit.\n"); }
	| imaginary_lit				        	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  BasicLit - Imaginary_lit.\n"); }
	| rune_lit				            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  BasicLit - Rune_lit.\n"); }
	| STRING				            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  BasicLit - String_lit.\n"); }
	;
	
OperandName :
	IDENTIFIER				            	{ if (priority < IMPORTANT_OUTPUT) printf("[%s] OperandName - Identifier.\n", yylval.buffer); }
	| QualifiedIdent			        	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  OperandName - QualifiedIdent.\n"); }
	
CompositeLit :
	LiteralType LiteralValue				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  CompositeLit - LiteralType LiteralValue.\n"); }
	;
	
LiteralType :
	StructType					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralType - StructType.\n"); }
	| ArrayType					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralType - ArrayType.\n"); }
	| '[' MULTIDOT ']' ElementType				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralType - [ ... ] ElementType.\n"); }
	| SliceType					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralType - SliceType.\n"); }
	| MapType					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralType - MapType.\n"); }
	| TypeName                          			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralType - TypeName.\n"); }
	| TypeName TypeArgs     				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralType - TypeName TypeArgs.\n"); }
	;
	
LiteralValue :
    '{' '}'                         				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralValue - { }.\n"); }
    | '{' ElementList '}'                           		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralValue - { ElementList }.\n"); }
    | '{' ElementList ',' '}'                           	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  LiteralValue - { ElementList , }.\n"); }
    ;
	
ElementList :
	KeyedElement						{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ElementList - KeyedElement.\n"); }
	| ElementList ',' KeyedElement				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ElementList - ElementList , KeyedElement.\n"); }
	;
    
KeyedElement :
    Element                             			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  KeyedElement - Element.\n"); }
    | Key ':' Element                               		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  KeyedElement - Key : Element.\n"); }
    ;
 	
Key :
 	FieldName					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Key - FieldName.\n"); } 
 	| Expression					    	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Key - Expression.\n"); } 
 	| LiteralValue					    	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Key - LiteralValue.\n"); }
 	;
 	
FieldName : 
	IDENTIFIER					        { if (priority < IMPORTANT_OUTPUT) printf("[%s] FieldName - identifier.\n", yylval.buffer); }
	;
	
Element : 
	Expression					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Element - Expression.\n"); }
	| LiteralValue					    	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Element - LiteralValue.\n"); }
	;

Type :
    	TypeName                          			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Type - TypeName.\n"); }
	| TypeName TypeArgs        	        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Type - Typename TypeArgs.\n"); }
	| TypeLit			                	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Type - TypeLit.\n"); }
	| '(' Type ')'			            		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Type - ( Type ).\n"); }
	;

TypeName :
	IDENTIFIER 			                	{ if (priority < IMPORTANT_OUTPUT) printf("[%s] TypeName - identifier.\n", yylval.buffer); }
	| QualifiedIdent 		            		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeName - QualifiedIdent.\n"); }
	;

TypeArgs :
    	'[' TypeList ']'                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeArgs - [ TypeList ].\n"); }
	'[' TypeList ',' ']' 	    				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeArgs - [ TypeList , ].\n"); }
	;
	
TypeList :
    Type                        				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeList - Type.\n"); }
    | TypeList ',' Type                     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeList - TypeList , Type.\n"); }
    ;
	
TypeLit :
	ArrayType			                	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - ArrayType.\n");}
	| FunctionType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - FunctionType.\n"); }
	| StructType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - StructType.\n"); }
	| PointerType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - PointerType.\n"); } 
	| InterfaceType		                		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - InterfaceType.\n"); } 
	| SliceType			                	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - SliceType.\n"); }
	| MapType			                	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - MapType.\n"); }
	| ChannelType			            		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeLit - ChannelType.\n"); }
	;

ArrayType :
	'[' ArrayLength ']' ElementType     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ArrayType - [ ArrayLength ] ElemenType.\n"); }
	;

ArrayLength :
	Expression			                	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ArrayLength - Expression.\n"); }
	;
	
ElementType :
	Type				                	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ElementType - Type.\n"); }
	;

FunctionType :
	FUNC Signature 		                		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  FunctionType - func Signature.\n"); }
	;
	
StructType :
	STRUCT '{' FieldDecls '}'				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  StructType - struct { FieldDecls }.\n"); }
	;
	
FieldDecls :
    	| FieldDecls  FieldDecl ';'             			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  FieldDecls - FieldDecls FieldDecl.\n"); }
    	;
    
FieldDecl :
    	IdentifierList Type                 			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  FieldDecl - IdentifierList Type.\n"); }
	| IdentifierList Type Tag     	    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  FieldDecl - IdentifierList Type Tag.\n"); }
	| EmbeddedField                     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  FieldDecl - EmbeddedField.\n"); }
	| EmbeddedField Tag     				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  FieldDecl - EmbeddedField Tag.\n"); }
	;
	
EmbeddedField :
    TypeName                        				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  EmbeddedField - TypeName.\n"); }
    | '*' TypeName                      			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  EmbeddedField - * TypeName.\n"); }
    | TypeName TypeArgs                     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  EmbeddedField - TypeName TypeArgs.\n"); }
    | '*' TypeName TypeArgs                     		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  EmbeddedField - * TypeName TypeArgs.\n"); }
    ;
 	
Tag : 
	STRING					            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Tag - string.\n"); }
	;
	
PointerType :
    '*' BaseType                        			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PointerType - * BaseType.\n"); }
    ;

BaseType :
    Type                                			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  BaseType - Type.\n"); }
    ;
    
InterfaceType :
    INTERFACE '{' InterfaceElems '}'         			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  InterfaceType - interface { InterfaceElems }.\n"); }
    ;

InterfaceElems :
    | InterfaceElems InterfaceElem ';'                 		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  InterfaceElems - InterfaceElems InterfaceElem.\n"); }
    ;
    
InterfaceElem :
    MethodElem                          			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  InterfaceElem - MethodElem.\n"); }
    | TypeElem                          			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  InterfaceElem - TypeElem.\n"); }
    ;

MethodElem :
    MethodName Signature                			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  MethodElem - MethodName Signature.\n"); }
    ;
    
MethodName :
    IDENTIFIER                          			{ if (priority < IMPORTANT_OUTPUT) printf("[%s] MethodName - identifier.\n", yylval.buffer); }
    ;
    
TypeElem :
    TypeTerm                            			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeElem - TypeTerm.\n"); }
    | TypeElem '|' TypeTerm             			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeElem - TypeElem | TypeTerm.\n"); }
    ;
    
TypeTerm :
    Type                                			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeTerm - Type\n"); }
    | UnderlyingType                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeTerm - UnderlyingType\n"); }
    ;

UnderlyingType :
    '~' Type                            			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  UnderlyingType - ~ Type.\n"); }
    ;
    
SliceType :
    '[' ']' ElementType                 			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  SliceType - [ ] ElemType.\n"); }
    ;

MapType :
    MAP '[' KeyType ']' ElementType     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  MapType - map [ KeyType ] ElementType.\n"); }
    ;

KeyType :
    Type                                			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  KeyType - Type.\n"); }
    ;

ChannelType :    
    CHAN ElementType                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ChannelType - chan ElementType.\n"); }
    | CHAN ARROW_LEFT ElementType       			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ChannelType - chan <- ElementType.\n"); }
    | ARROW_LEFT CHAN ElementType       			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ChannelType - <- chan ElementType.\n"); }
    ;

int_lit :
    decimal_lit                         			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  int_lit - decimal_lit.\n"); }
    | binary_lit                        			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  int_lit - binary_lit.\n"); }           
    | octal_lit                         			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  int_lit - octal_lit.\n"); }
    | hex_lit                           			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  int_lit - hex_lit.\n"); }
    ;

decimal_lit :
    '0'                                 			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_lit - 0.\n"); }                      
    | DIGIT                             			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_lit - digit.\n"); }
    | DIGIT decimal_digits              			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_lit - digit decimal_digits.\n"); }                 
    | DIGIT '_' decimal_digits          			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_lit - digit _ decimal_digits.\n"); }                         
    ;

binary_lit :
    '0' 'b' binary_digits                   			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_lit - 0 b binary_digits.\n"); }
    | '0' 'B' binary_digits                 			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_lit - 0 B binary_digits.\n"); }
    | '0' 'b' '_' binary_digits                 		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_lit - 0 b _ binary_digits.\n"); }
    | '0' 'B' '_' binary_digits                 		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_lit - 0 B _ binary_digits.\n"); }
    ;

octal_lit :
    '0' 'o' octal_digits                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_lit - 0 o octal_digits.\n"); }
    | '0' 'O' octal_digits                  			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_lit - 0 O octal_digits.\n"); }
    | '0' 'o' '_' octal_digits                  		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_lit - 0 o _ octal_digits.\n"); }
    | '0' 'O' '_' octal_digits                  		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_lit - 0 O _ octal_digits.\n"); }
    ;

hex_lit :
    '0' 'x' hex_digits                  			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_lit - 0 x hex_digits"); }
    | '0' 'X' hex_digits                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_lit - 0 X hex_digits"); }
    | '0' 'x' '_' hex_digits                    		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_lit - 0 x _ hex_digits"); }
    | '0' 'X' '_' hex_digits                    		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_lit - 0 X _ hex_digits"); }
    ;

decimal_digits :
    decimal_digit                       			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_digits - decimal_digit.\n"); }
    | decimal_digits decimal_digit                      	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_digits - decimal_digits decimal_digit.\n"); }
    | decimal_digits '_' decimal_digit                      	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_digits - decimal_digits _ decimal_digit.\n"); }
    ;

binary_digits :
    binary_digit                       				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_digits - binary_digit.\n"); }
    | binary_digits binary_digit                      		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_digits - binary_digits binary_digit.\n"); }
    | binary_digits '_' binary_digit                      	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_digits - binary_digits _ binary_digit.\n"); }
    ;

octal_digits :
    octal_digit                       				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_digits - octal_digit.\n"); }
    | octal_digits octal_digit                      		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_digits - octal_digits octal_digit.\n"); }
    | octal_digits '_' octal_digit                      	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_digits - octal_digits _ octal_digit.\n"); }
    ;

hex_digits :
    hex_digit                       				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_digits - hex_digit.\n"); }
    | hex_digits hex_digit                      		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_digits - hex_digits hex_digit.\n"); }
    | hex_digits '_' hex_digit                      		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_digits - hex_digits _ hex_digit.\n"); }
    ;

decimal_digit :
    DIGIT                           				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_digit - digit.\n"); }
    ;

binary_digit :
    '0'                             				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_digit - 0\n"); }
    | '1'                           				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  binary_digit - 1\n"); }
    ;
 
octal_digit :
    OCT                             				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_digit - oct\n"); }
    ;

hex_digit :
    HEX                             				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_digit - hex.\n"); }
    ;

float_lit :
    decimal_float_lit                   			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  float_lit - decimal_float_lit\n"); }
    | hex_float_lit                     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  float_lit - hex_float_lit\n"); }
    ;

decimal_float_lit :
    decimal_digits '.'                      			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_float_lit - decimal_digits ..\n"); }
    | decimal_digits '.' decimal_digits                     	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_float_lit - decimal_digits . decimal_digits.\n"); }
    | decimal_digits '.' decimal_exponent                       { if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_float_lit - decimal_digits . decimal_exponent.\n"); }
    | decimal_digits '.' decimal_digits decimal_exponent        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_float_lit - decimal_digits . decimal_digits decimal_exponent.\n"); }
    | decimal_digits decimal_exponent                       	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_float_lit - decimal_digits decimal_exponent.\n"); }
    | '.' decimal_digits                        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_float_lit - . decimal_digits.\n"); }
    | '.' decimal_digits decimal_exponent                       { if (priority < IMPORTANT_OUTPUT) printf("[     ]  decimal_float_lit - . decimal_digits decimal_exponent.\n"); }
    ;

decimal_exponent :
    'e' decimal_digits                  			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  deicmal_exponent - 'e' decimal_digits.\n"); }
    | 'E' decimal_digits                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  deicmal_exponent - 'E' decimal_digits.\n"); }
    | 'e' '+' decimal_digits                    		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  deicmal_exponent - 'e' '+' decimal_digits.\n"); }
    | 'e' '-' decimal_digits                    		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  deicmal_exponent - 'e' '-' decimal_digits.\n"); }
    | 'E' '+' decimal_digits                    		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  deicmal_exponent - 'E' '+' decimal_digits.\n"); }
    | 'E' '-' decimal_digits                    		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  deicmal_exponent - 'E' '-' decimal_digits.\n"); }
    ;

hex_float_lit :
    '0' 'x' hex_mantissa hex_exponent       			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_float_lit - '0' 'x' hex_mantissa hex_exponent.\n"); }
    | '0' 'X' hex_mantissa hex_exponent     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_float_lit - '0' 'X' hex_mantissa hex_exponent.\n"); }
    ;

hex_mantissa :
    hex_digits '.'                      			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_mantissa - hex_digits ..\n"); }
    | '_' hex_digits '.'                        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_mantissa - _ hex_digits ..\n"); }
    | hex_digits '.' hex_digits                     		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_mantissa - hex_digits . hex_digits.\n"); }
    | '_' hex_digits '.' hex_digits                     	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_mantissa - _ hex_digits . hex_digits.\n"); }
    | hex_digits                        			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_mantissa - hex_digits.\n"); }
    | '_' hex_digits                        			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_mantissa - _ hex_digits.\n"); }
    | '.' hex_digits                        			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_mantissa - . hex_digits.\n"); }
    ;

hex_exponent :
    'p' decimal_digits                      			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_exponent - p decimal_digits.\n"); }
    | 'P' decimal_digits                        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_exponent - P decimal_digits.\n"); }
    | 'p' '+' decimal_digits                        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_exponent - p + decimal_digits.\n"); }
    | 'p' '-' decimal_digits                        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_exponent - p - decimal_digits.\n"); }
    | 'P' '+' decimal_digits                        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_exponent - P + decimal_digits.\n"); }
    | 'P' '-' decimal_digits                        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_exponent - P - decimal_digits.\n"); }
    ;

imaginary_lit :
    decimal_digits 'i'                  			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  imaginary_lit - decimal_digits\n"); }
    | int_lit 'i'                       			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  imaginary_lit - int_lit\n"); }
    | float_lit 'i'                     			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  imaginary_lit - float_lit\n"); }
    ;

rune_lit :
    '\'' unicode_value '\''             			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  rune_lit - ' unicode_value '.\n"); }
    | '\'' byte_value '\''              			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  rune_lit - ' byte_value '.\n"); }
    ;
    
unicode_value :
    little_u_value                      			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  unicode_value - little_u_value\n"); }
    | big_u_value                       			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  unicode_value - big_u_value\n"); }
    | escaped_char                      			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  unicode_value - escaped_char\n"); }
    ;
    
byte_value :
    octal_byte_value                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  byte_value - octal_byte_value\n"); }
    | hex_byte_value                    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  byte_value - hex_byte_value\n"); }
    ;

octal_byte_value :
    OCT_BYTE                            			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  octal_byte_value - oct_byte.\n"); }
    ;

hex_byte_value :
    HEX_BYTE                            			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  hex_byte_value - hex_byte.\n"); }
    ;

little_u_value :
    LITTLE_U                            			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  little_u_value - little_u.\n"); }
    ;

big_u_value :
    BIG_U                               			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  big_u_value - big_u.\n"); }
    ;

escaped_char :
    ESCAPED CHAR                        			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  escaped_char - escaped_char.\n"); }
    ;
    
Block :
	'{' StatementList '}'				        { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Block - { StatementList }.\n"); }
	;
    
StatementList :
    | StatementList Statement ';'          			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  StatementList - StatementList Statement ;.\n"); }
    ;

Statement :
    Declaration                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - Declaration\n"); }
    | LabeledStmt                       			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - LabeledStmt\n"); }
    | SimpleStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - SimpleStmt\n"); }
    | GoStmt                            			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - GoStmt\n"); }
    | ReturnStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - ReturnStmt\n"); }
    | BreakStmt                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - BreakStmt\n"); }
    | ContinueStmt                      			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - ContinueStmt\n"); }
    | GotoStmt                          			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - GotoStmt\n"); }
    | FallthroughStmt                   			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - FallthroughStmt\n"); }
    | Block                             			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - Block\n"); }
    | IfStmt                            			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - IfStmt\n"); }
    | SwitchStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - SwitchStmt\n"); }
    | SelectStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - SelectStmt\n"); }
    | ForStmt                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - ForStmt\n"); }
    | DeferStmt                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Statement - DeferStmt\n"); }
    ;
    
SelectStmt : 
    SELECT '{' CommClauses '}'       				{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SelectStmt - select { CommClauses }.\n"); }
    ;
    
CommClauses :
    | CommClauses CommClause                			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  CommClauses - CommClauses CommClause.\n"); }
    ;
    
CommClause :
    CommCase ':' StatementList              			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  CommClause - CommCase : StatementList.\n"); }
    ;
    
CommCase :
    CASE SendStmt                           			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  CommCase - SendStmt.\n"); }
    | CASE RecvStmt                         			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  CommCase - RecvStmt.\n"); }
    | DEFAULT                               			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  CommCase - default.\n"); }
    ;
    
RecvStmt :
    RecvExpr                                                	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  RecvStmt - RecvExpr.\n"); }
    | ExpressionList '=' RecvExpr                           	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  RecvStmt - ExpressionList = RecvExpr.\n"); }
    | IdentifierList ASSIGN RecvExpr                          	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  RecvStmt - IdentifierList := RecvExpr.\n"); }
    ;
    
RecvExpr :
    Expression                              			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  RecvExpr - Expression.\n"); }
    ;
    
PostStmt :
    SimpleStmt                          			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  PostStmt - SimpleStmt.\n"); }
    
SimpleStmt :
    EmptyStmt                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SimpleStmt - EmptyStmt\n"); }
    | ExpressionStmt                    			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SimpleStmt - ExpressionStmt\n"); }
    | SendStmt                          			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SimpleStmt - SendStmt\n"); }
    | IncDecStmt                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SimpleStmt - IncDecStmt\n"); }
    | Assignment                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SimpleStmt - Assignment\n"); }
    | ShortVarDecl                      			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SimpleStmt - ShortVarDecl\n"); }
    ;
    
TypeSwitchStmt :
    SWITCH TypeSwitchGuard '{' TypeCaseClauses '}'                          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSwitchStmt - switch TypeSwitchGuard { TypeCaseClause }.\n"); }
    | SWITCH SimpleStmt ';' TypeSwitchGuard '{' TypeCaseClauses '}'     { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSwitchStmt - switch SimpleStmt ; TypeSwitchGuard { TypeCaseClauses }.\n"); }
    ;

TypeSwitchGuard :
    PrimaryExpr '.' '(' TYPE ')'                               	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSwitchGuard - PrimaryExpr . ( Type ).\n"); }
    | IDENTIFIER ASSIGN PrimaryExpr '.' '(' TYPE ')'              { if (priority < IMPORTANT_OUTPUT) printf("[%s] TypeSwitchGuard - Identifier := PrimaryExpr . ( Type ).\n", yylval.buffer); }
    ;
   
TypeCaseClauses :
    | TypeCaseClauses TypeCaseClause                            { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeCaseClauses - TypeCaseClauses TypeCaseClause.\n"); }
    ;
    
TypeCaseClause :
    TypeSwitchCase ':' StatementList                            { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeCaseClause - TypeSwitchCase : StatementList.\n"); }
    ;
   
TypeSwitchCase :
    CASE TypeList                                               { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSwitchCase - case TypeList.\n"); }
    | DEFAULT                                                   { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSwitchCase - default.\n"); }
    ;

Declaration :
    ConstDecl                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Declaration - ConstDecl\n"); }
    | TypeDecl                          			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Declaration - TypeDecl\n"); }
    | VarDecl                           			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Declaration - VarDecl\n"); }
    ;

TopLevelDecls :
    | TopLevelDecls TopLevelDecl ';'        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  TopLevelDecls - TopLevelDecls TopLevelDecl.\n"); }
    ;

TopLevelDecl :
    Declaration                         			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  TopLevelDecl - Declaration.\n"); }
    | FunctionDecl                      			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  TopLevelDecl - FunctionDecl.\n"); }
    | MethodDecl                        			{ if (priority <= IMPORTANT_OUTPUT) printf("[     ]  TopLevelDecl - MethodDecl.\n"); }
    ;

ConstDecl :
    CONST ConstSpec                                       	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ConstDecl - const ConstSpec.\n"); }  
    | CONST '(' ConstSpecs ')'                 			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ConstDecl - const ( ConstSpecs ).\n"); }
    ;

ConstSpecs :
    | ConstSpecs ConstSpec ';'                             	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ConstSpecs - ConstSpecs ConstSpec.\n"); }
    ;

ConstSpec :
    IdentifierList                                      	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ConstSpec - IdentifierList.\n"); }
    | IdentifierList '=' ExpressionList                 	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ConstSpec - IdentifierList = ExpressionList.\n"); }
    | IdentifierList Type '=' ExpressionList            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ConstSpec - IdentifierList Type = ExpressionList.\n"); }
    ;

TypeDecl :
    TYPE TypeSpec                                               { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeDecl - type TypeSpec.\n"); }
    | TYPE '(' TypeSpecs ')'                                    { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeDecl - type ( TypeSpecs ).\n"); }
    ;
    
TypeSpecs :
    | TypeSpecs TypeSpec ';'                                       { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSpecs - TypeSpecs TypeSpec.\n"); }
    ;
    
TypeSpec :
    AliasDecl                                                   { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSpec - AliasDecl\n"); }
    | TypeDef                                                   { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeSpec - TypeDef\n"); }
    ;

AliasDecl :
    IDENTIFIER '=' Type                                         { if (priority < IMPORTANT_OUTPUT) printf("[%s] AliasDecl - identifier = Type.\n", yylval.buffer); }
    ;

TypeDef :
    IDENTIFIER Type                                             { if (priority < IMPORTANT_OUTPUT) printf("[%s] TypeDef - Identifier Type.\n", yylval.buffer); }
    | IDENTIFIER TypeParameters  Type                           { if (priority < IMPORTANT_OUTPUT) printf("[%s] TypeDef - Identifier TypeParameters Type.\n", yylval.buffer); }
    ;

MethodDecl :
    FUNC Receiver MethodName Signature                          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  MethodDecl - func Receiver MethodName Signature.\n"); }
    | FUNC Receiver MethodName Signature FunctionBody           { if (priority < IMPORTANT_OUTPUT) printf("[     ]  MethodDecl - func Receiver MethodName Signature FunctionBody.\n"); }
    ;

Conversion :
    Type '(' Expression ')'                                     { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Conversion - Type ( Expression ).\n"); }
    | Type '(' Expression ',' ')'                               { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Conversion - Type ( Expression , ).\n"); }
    ;

Receiver :
    Parameters                                                  { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Receiver - Parameters.\n"); }
    ;
    
MethodExpr :
    ReceiverType '.' MethodName                                 { if (priority < IMPORTANT_OUTPUT) printf("[     ]  MethodExpr - ReceiverType . MethodName\n"); }
    ;
    
ReceiverType :
    Type                                                        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ReceiverType - Type.\n"); }
    ;
    
LabeledStmt :
    Label ':' Statement                                         { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  LabeledStmt - Label : Statement.\n"); }
    ;

Label :
    IDENTIFIER                                                  { if (priority < IMPORTANT_OUTPUT) printf("[%s] Label - Identifier.\n", yylval.buffer); }
    ;

EmptyStmt :
                                                                { if (priority < IMPORTANT_OUTPUT) printf("[     ]  EmptyStmt - nothing.\n"); }
    ;

ExpressionStmt :
    Expression                                                  { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExpressionStmt - Expression.\n"); }
    ;

SendStmt :
    Channel ARROW_LEFT Expression                                     { if (priority < IMPORTANT_OUTPUT) printf("[     ]  SendStmt - Channel <- Expression.\n"); }
    ;

Channel :
    Expression                                                  { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Channel - Expression.\n"); }
    ;

IncDecStmt :
    Expression INCREMENT                                             { if (priority < IMPORTANT_OUTPUT) printf("[     ]  IncDecStmt - Expression ++.\n"); }
    | Expression DECREMENT                                          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  IncDecStmt - Expression --.\n"); }
    ;

Assignment :
    ExpressionList assign_op ExpressionList                     { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Assignment - ExpressionList assign_op ExpressionList.\n"); }
    ;

assign_op :
	CHANGE_ASSIGN						{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  assign_op - Change + Assign.\n"); }
	;

ShortVarDecl :
    IdentifierList ASSIGN ExpressionList                          { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ShortVarDecl - IdentifierList := ExpressionList.\n"); }
    ;

GoStmt :
    GO Expression                                               { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  GoStmt - go Expression.\n"); }
    ;

ReturnStmt :
    RETURN                                                      { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ReturnStmt - return.\n"); }
    | RETURN ExpressionList                                     { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ReturnStmt - return ExpressionList.\n"); }
    ;

BreakStmt :
    BREAK                                                       { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  BreadStmt - break.\n"); }
    | BREAK Label                                               { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  BreakStmt - break Label.\n"); }    
    ;

ContinueStmt :
    CONTINUE                                                    { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ContinueStmt - continue.\n"); }
    | CONTINUE Label                                            { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ContinueStmt - continue Label.\n"); }
    ;

GotoStmt :
    GOTO Label                                                  { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  GotoStmt - goto Label.\n"); }
    ;

FallthroughStmt :
    FALLTHROUGH                                                 { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  FallthroughStmt - fallthrough.\n"); }
    ;

IfStmt :
    IF Expression Block                                         { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  IfStmt - if Expression Block.\n"); }
    | IF SimpleStmt ';' Expression Block                            { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  IfStmt - if SimpleStmt Expression Block.\n"); }
    | IF Expression Block ELSE IfStmt                           { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  IfStmt - if Expression Block else IfStmt.\n"); }
    | IF SimpleStmt ';' Expression Block ELSE IfStmt                { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  IfStmt - if SimpleStmt Expression Block else IfStmt.\n"); }
    | IF Expression Block ELSE Block                            { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  IfStmt - if Expression Block else Block.\n"); }
    | IF SimpleStmt ';' Expression Block ELSE Block                 { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  IfStmt - if SimpleStmt Expression Block else Block.\n"); }
    ;

SwitchStmt :
    ExprSwitchStmt                                              { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SwitchStmt - ExprSwitchStmt\n"); }
    | TypeSwitchStmt                                            { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  SwitchStmt - TypeSwitchStmt\n"); }
    ;

ExprSwitchStmt :
    SWITCH '{' ExprCaseClauses '}'                              { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprSwitchStmt - switch { ExprCaseClauses }.\n"); }
    | SWITCH SimpleStmt ';' '{' ExprCaseClauses '}'                 { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprSwitchStmt - switch SimpleStmt { ExprCaseClauses }.\n"); }
    | SWITCH Expression '{' ExprCaseClauses '}'                 { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprSwitchStmt - switch Expression { ExprCaseClauses }.\n"); }
    | SWITCH SimpleStmt ';' Expression '{' ExprCaseClauses '}'      { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprSwitchStmt - switch SimpleStmt Expression { ExprCaseClauses }.\n"); }
    ;

ExprCaseClauses :
    | ExprCaseClauses ExprCaseClause                            { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprCaseClauses - ExprCaseClauses ExprCaseClause.\n"); }
    ;
    
ExprCaseClause :
    ExprSwitchCase ':' StatementList                            { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprCaseClause - ExprSwitchCase : StatementList.\n"); }
    ;

ExprSwitchCase :
    CASE ExpressionList                                         { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprSwitchCase - case ExpressionList.\n"); }
    | DEFAULT                                                   { if (priority < IMPORTANT_OUTPUT) printf("[     ]  ExprSwitchCase - default.\n"); }
    ;
    
ForStmt :
    FOR Block                                                   { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ForStmt - for Block.\n"); }
    | FOR Condition Block                                       { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ForStmt - for Condition Block.\n"); }
    | FOR ForClause Block                                       { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ForStmt - for ForClause Block.\n"); }
    | FOR RangeClause Block                                     { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  ForStmt - for RangeClause Block.\n"); }
    ;
    
DeferStmt :
    DEFER Expression                                            { if (priority <= IMPORTANT_OUTPUT) printf("[     ]  DeferStmt - defer Expression.\n"); }
    ;
    
InitStmt :
    SimpleStmt                                                  { if (priority < IMPORTANT_OUTPUT) printf("[     ]  InitStmt - SimpleStmt.\n"); }
    ;
    
RangeClause :
    RANGE Expression                                            { if (priority < IMPORTANT_OUTPUT) printf("[     ]  RangeClause - range Expression.\n"); }
    | ExpressionList '=' RANGE Expression                       { if (priority < IMPORTANT_OUTPUT) printf("[     ]  RangeClause - ExpressionList = range Expression.\n"); }
    | IdentifierList ASSIGN RANGE Expression                      { if (priority < IMPORTANT_OUTPUT) printf("[     ]  RangeClause - IdentifierList := range Expression.\n"); }
    ;
    
Condition :
    Expression              					{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Condition - Expression.\n"); }
    ;

ForClause :
               ';'           ';'                            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - ; ;.\n"); }
    | InitStmt ';'           ';'                            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - InitStmt ; ;.\n"); }
    |          ';' Condition ';'                            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - ; Condition ;.\n"); }
    |          ';'           ';' PostStmt                   	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - ; ; PostStmt.\n"); }
    | InitStmt ';' Condition ';'                            	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - InitStmt ; Condition ;.\n"); }
    |          ';' Condition ';' PostStmt                   	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - ; Condition ; PostStmt.\n"); }
    | InitStmt ';'           ';' PostStmt                   	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - InitStmt ; ; PostStmt.\n"); }
    | InitStmt ';' Condition ';' PostStmt                   	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ForClause - InitStmt ; Condition ; PostStmt.\n"); }
    ;
	
FunctionDecl :
    FUNC FunctionName Signature                                 { if (priority < IMPORTANT_OUTPUT) printf("[     ]  FunctionDecl - func FunctionName Signature.\n"); }
    | FUNC FunctionName TypeParameters Signature                { if (priority < IMPORTANT_OUTPUT) printf("[     ]  FunctionDecl - func FunctionName TypeParameters Signature.\n"); }
    | FUNC FunctionName Signature FunctionBody                  { if (priority < IMPORTANT_OUTPUT) printf("[     ]  FunctionDecl - func FunctionName Signature FunctionBody.\n"); }
    | FUNC FunctionName TypeParameters Signature FunctionBody   { if (priority < IMPORTANT_OUTPUT) printf("[     ]  FunctionDecl - func FunctionName TypeParameters Signature FunctionBody.\n"); }
    ;
    
FunctionName :
	IDENTIFIER	                                        { if (priority < IMPORTANT_OUTPUT) printf("[%s] FunctionName - identifier.\n", yylval.buffer); }
	;

FunctionBody :
	Block		                                        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  FunctionBody - Block.\n"); }
	;
	
Signature :
    Parameters							{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Signature - Parameters.\n"); }
    | Parameters Result			        		{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Signature - Parameters Result.\n"); }
	;
	
Parameters :
    '('')'                             				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Parameters - ( ).\n"); }
    | '(' ParameterList ')'             			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Parameters - ( ParametersList ).\n"); }
    | '(' ParameterList ',' ')' 	    			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  Parameters - ( ParametersList , ).\n"); }
	;
	
Result :
	Parameters 					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Result - Parameters.\n"); }
	| Type						        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  Result - Type.\n"); }
	;
	
ParameterList :
    ParameterDecl						{ if (priority < IMPORTANT_OUTPUT) printf("[     ] ParameterList - ParameterDecl.\n"); }
    | ParameterList ',' ParameterDecl				{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ParameterList - ParameterList , ParameterDecl.\n"); }
	;

ParameterDecl :
    Type                                			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ParameterDecl - Type.\n"); }
    | IdentifierList Type               			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ParameterDecl - IdentifierList Type.\n"); }
    | MULTIDOT Type                        			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ParameterDecl - ... Type.\n"); }
    | IdentifierList MULTIDOT Type         			{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  ParameterDecl - IdentifierList ... Type.\n"); }
    ;
	
TypeParameters :
    '[' TypeParamList ']'                                   	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeParameters - [ TypeParamList ].\n"); }
	'[' TypeParamList ',' ']' 		                { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeParameters - [ TypeParamList , ].\n"); }
	;
	
TypeParamList :
    TypeParamDecl                                           	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeParamList - TypeParamDecl.\n"); }
    | TypeParamList ',' TypeParamDecl                       	{ if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeParamList - TypeParamList , TypeParamDecl.\n"); }
    ;
	
TypeConstraint :
	TypeElem					        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeConstraint - TypeElem.\n"); }
	;

TypeParamDecl :
	IdentifierList TypeConstraint 			        { if (priority < IMPORTANT_OUTPUT) printf("[     ]  TypeParamDecl - IdentifierList TypeConstraint.\n"); }
	;

QualifiedIdent :
	PackageName '.' IDENTIFIER	                        { if (priority < IMPORTANT_OUTPUT) printf("[%s] QualifiedIdent - PackageName . identifier.\n", yylval.buffer); }
	;

%%

int main() {
	yylval.digit = 0;
    	yylval.buffer[FIELD] = 0;
	yyparse();
	return 0;
}
int yyerror(const char* s) {
	if (priority <= IMPORTANT_OUTPUT) printf("[     ]  Error: %s\n", s);
	return 1;
}
int yywrap() {
	return 1;
}