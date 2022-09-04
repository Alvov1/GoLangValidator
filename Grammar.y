%{
    #define FIELD 5
	#include <stdio.h>
	#include <stdlib.h>
	
	int yylex();
	int yyerror();
	int yywrap();
	
%}

%union {
	int digit;
	char buffer[5 + 1];
}

%token IDENTIFIER DIGIT LETTER VAR_TYPE BOOL_CONST NIL PACKAGE 
%token GOTO FALLTHROUGH DEFER CHAN IMPORT FUNC BREAK CASE CONST
%token CONTINUE DEFAULT ELSE FOR GO IF RANGE RETURN STRUCT 
%token HEX_BYTE LITTLE_U BIG_U ESCAPED CHAR SELECT OCT HEX OCT_BYTE
%token ASSIGN OR AND EQUALL NOT_EQUALL LESS_EQUALL GREATER_EQUALL 
%token SHIFT_LEFT SHIFT_RIGHT AND_XOR MULTIDOT INCREMENT DECREMENT
%token SWITCH TYPE VAR STRING ARROW_LEFT INTERFACE MAP 

%%

SourceFile :
    PackageClause ImportDecls TopLevelDecls         { printf("[     ]  SourceFile - PackageClause ImportDecls TopLevelDecls.\n"); exit(0); }
    ;
    
PackageClause : 
    PACKAGE PackageName                             { printf("[     ]  PackageClause - package PackageName.\n"); }
    ;
    
PackageName :
	IDENTIFIER			            { printf("[%s] PackageName - identifier.\n", yylval.buffer); }
	;
	
ImportDecls :
    ImportDecl					    { printf("[     ]  ImportDecls - ImportDecl.\n"); }
    | ImportDecls ImportDecl                        { printf("[     ]  ImportDecls - ImportDecls ImportDecl.\n"); }
    ;
    
ImportDecl :
    IMPORT ImportSpec                               { printf("[     ]  ImportDecl - import ImportSpec.\n"); }
    | IMPORT '(' ImportSpecs ')'                    { printf("[     ]  ImportDecl - import ( ImportSpec ).\n"); }
    ;
    
ImportSpecs :
    | ImportSpecs ImportSpec                        { printf("[     ]  ImportSpecs - ImportSpecsImportSpec.\n"); }
    ;

ImportSpec :
    ImportPath                                      { printf("[     ]  ImportSpec - ImportPath.\n"); }
    | '.' ImportPath                                { printf("[     ]  ImportSpec - . ImportPath.\n"); }
    | PackageName ImportPath                        { printf("[     ]  ImportSpec - PackageName ImportPath.\n"); }
    ;
    
ImportPath :
    STRING                                          { printf("[     ]  ImportPath - string.\n"); }
    ;

VarDecl :
	VAR VarSpec				    { printf("[     ]  VarDecl - VAR VarSpec.\n"); exit(0); }
	| VAR '(' VarSpecs ')'	                    { printf("[     ]  VarDecl - VAR ( VarSpec ; ).\n"); exit(0); }
	;
	
VarSpecs :
    | VarSpecs VarSpec                              { printf("[     ]  VarSpecs - VarSpecs VarSpec.\n"); }
    ;
    
VarSpec :
    IdentifierList Type                             { printf("[     ]  VarSpec: IdentifierList Type.\n"); }
    | IdentifierList Type '=' ExpressionList        { printf("[     ]  VarSpec: IdentifierList Type = ExpressionList.\n"); }
    | IdentifierList '=' ExpressionList             { printf("[     ]  VarSpec: IdentifierList = ExpressionList.\n"); }
    ;
	
ExpressionList :
    Expression                                      { printf("[     ]  ExpressionList - Expression.\n"); }
    | ExpressionList ',' Expression                 { printf("[     ]  ExpressionList - ExpressionList , Expression.\n"); }
    ;

Expression :
	UnaryExpr					{ printf("[     ]  Expression - UnaryExpr.\n"); }
	| Expression binary_op Expression		{ printf("[     ]  Expression - Expression binary_op Expression.\n"); }
	;
	
UnaryExpr :
	PrimaryExpr					{ printf("[     ]  UnaryExpr - PrimaryExpr.\n"); }
	| unary_op UnaryExpr				{ printf("[     ]  UnaryExpr - unary_op UnaryExpr.\n"); }
	;
	
binary_op :
	OR						{ printf("[     ]  Binary_operator - ||.\n"); }
	| AND						{ printf("[     ]  Binary_operator - &&.\n"); }
	| rel_op					        { printf("[     ]  Binary_operator - Rel_op.\n"); }
	| add_op					        { printf("[     ]  Binary_operator - Add_op.\n"); }
	| mul_op						    { printf("[     ]  Binary_operator - Mul_op.\n"); }
	;
	
rel_op :
	EQUALL							    { printf("[     ]  Relativeness_operator - ==.\n"); }
	| NOT_EQUALL					{ printf("[     ]  Relativeness_operator - !=.\n"); }
	| "<"							    { printf("[     ]  Relativeness_operator - <.\n"); }
	| LESS_EQUALL							    { printf("[     ]  Relativeness_operator - <=.\n"); }
	| '>'							    { printf("[     ]  Relativeness_operator - >.\n"); }
	| GREATER_EQUALL							    { printf("[     ]  Relativeness_operator - >=.\n"); }
	;
	
add_op :
	'+'							    { printf("[     ]  Addition_operator - +.\n"); }
	| '-'							    { printf("[     ]  Addition_operator - -.\n"); }
	| '|'							    { printf("[     ]  Addition_operator - |.\n"); }
	| '^'							    { printf("[     ]  Addition_operator - ^.\n"); }
	;
	
mul_op :
	'*'							    { printf("[     ]  Multiply_operator - *.\n"); }
	| '/'							    { printf("[     ]  Multiply_operator - /.\n"); }
	| '%'							    { printf("[     ]  Multiply_operator - %%.\n"); }
	| SHIFT_LEFT							    { printf("[     ]  Multiply_operator - <<.\n"); }
	| SHIFT_RIGHT							    { printf("[     ]  Multiply_operator - >>.\n"); }
	| '&'							    { printf("[     ]  Multiply_operator - &.\n"); }
	| AND_XOR							    { printf("[     ]  Multiply_operator - &^.\n"); }
	;
	
unary_op :
	'+'							    { printf("[     ]  Unary_operator - +.\n"); }
	| '-'							    { printf("[     ]  Unary_operator - -.\n"); }
	| '!'							    { printf("[     ]  Unary_operator - !.\n"); }
	| '^'							    { printf("[     ]  Unary_operator - ^.\n"); }
	| '*'							    { printf("[     ]  Unary_operator - *.\n"); }
	| '&'							    { printf("[     ]  Unary_operator - &.\n"); }
	| ARROW_LEFT							    { printf("[     ]  Unary_operator - <-.\n"); }
	;

PrimaryExpr :
	Operand							{ printf("[     ]  PrimaryExpr - Operand.\n"); }
	| Conversion						{ printf("[     ]  PrimaryExpr - Conversion.\n"); }
	| MethodExpr						{ printf("[     ]  PrimaryExpr - MethodExpr.\n"); }
	| PrimaryExpr Selector					{ printf("[     ]  PrimaryExpr - PrimaryExpr Selector.\n"); }
	| PrimaryExpr Index					{ printf("[     ]  PrimaryExpr - PrimaryExpr Index.\n"); }
	| PrimaryExpr Slice					{ printf("[     ]  PrimaryExpr - PrimaryExpr Slice.\n"); }
	| PrimaryExpr TypeAssertion				{ printf("[     ]  PrimaryExpr - PrimaryExpr TypeAssertion.\n"); }
	| PrimaryExpr Arguments					{ printf("[     ]  PrimaryExpr - PrimaryExpr Arguments.\n"); }
	;
	
Selector :
	'.' IDENTIFIER						{ printf("[%s] Selector - . identifier.\n", yylval.buffer);}
	;

Index :
    | Expression						{ printf("[     ]  Index - Expression.\n"); }
	;
	
Slice :
    '[' ':' ']'                                 		{ printf("[     ]  Slice - [ : ].\n"); }
    | '[' Expression ':' ']'                                    { printf("[     ]  Slice - [ Expression : ].\n"); }
    | '[' ':' Expression ']'                                    { printf("[     ]  Slice - [ : Expression ].\n"); }
    | '[' Expression ':' Expression ']'                         { printf("[     ]  Slice - [ Expression : Expression ].\n"); }
    | '[' ':' Expression ':' Expression ']'                     { printf("[     ]  Slice - [ : Expression : Expression ].\n"); }
    | '[' Expression ':' Expression ':' Expression ']'          { printf("[     ]  Slice - [ Expression : Expression : Expression ].\n"); }
    ;
	
TypeAssertion :
	'.' '(' Type ')'				    { printf("[     ]  TypeAssertion - . ( Type ).\n"); }
	;
	
Arguments :
    '(' ')'                                 			{ printf("[     ]  Arguments: ( ).\n");  }
    | '(' ExpressionList ')'                                    { printf("[     ]  Arguments: ( ExpressionList ).\n");  }
    | '(' ExpressionList MULTIDOT ')'                              { printf("[     ]  Arguments: ( ExpressionList ... ).\n");  }
    | '(' ExpressionList ',' ')'                                { printf("[     ]  Arguments: ( ExpressionList , ).\n");  }
    | '(' ExpressionList MULTIDOT ',' ')'                          { printf("[     ]  Arguments: ( ExpressionList ... , ).\n");  }    
    | '(' Type ')'                                  		{ printf("[     ]  Arguments: ( Type ).\n");  }
    | '(' Type MULTIDOT ')'                                    	{ printf("[     ]  Arguments: ( Type ... ).\n");  }
    | '(' Type ',' ')'                                  	{ printf("[     ]  Arguments: ( Type , ).\n");  }
    | '(' Type MULTIDOT ',' ')'                                    { printf("[     ]  Arguments: ( Type ... , ).\n");  }   
    | '(' Type ',' ExpressionList ')'                           { printf("[     ]  Arguments: ( Type , ExpressionList ).\n");  }
    | '(' Type ',' ExpressionList MULTIDOT ')'                     { printf("[     ]  Arguments: ( Type , ExpressionList ... ).\n");  }
    | '(' Type ',' ExpressionList ',' ')'                       { printf("[     ]  Arguments: ( Type ExpressionList , ).\n");  }
    | '(' Type ',' ExpressionList MULTIDOT ',' ')'                 { printf("[     ]  Arguments: ( Type ExpressionList ... , ).\n");  }
    ;
	
Operand :
	Literal						        { printf("[     ]  Operand - Literal.\n"); }
	| OperandName                       			{ printf("[     ]  Operand - OperandName.\n"); }
	| OperandName TypeArgs		        		{ printf("[     ]  Operand - OperandName TypeArgs.\n"); } 
	| '(' Expression ')'					{ printf("[     ]  Operand - ( Expression ).\n"); }
	;
	
Literal :
	BasicLit				            	{ printf("[     ]  Literal - BasicLit.\n"); } 
	| CompositeLit				        	{ printf("[     ]  Literal - CompositeLit.\n"); } 
	| FunctionLit				        	{ printf("[     ]  Literal - FunctionLit.\n"); } 
	;
	
FunctionLit :
    FUNC Signature FunctionBody         			{ printf("[     ]  FunctionLit - func Signature FucntionBody.\n"); }
    ;
	
BasicLit :
	int_lit					            	{ printf("[     ]  BasicLit - Int_lit.\n"); }
	| float_lit				            	{ printf("[     ]  BasicLit - Float_lit.\n"); }
	| imaginary_lit				        	{ printf("[     ]  BasicLit - Imaginary_lit.\n"); }
	| rune_lit				            	{ printf("[     ]  BasicLit - Rune_lit.\n"); }
	| STRING				            	{ printf("[     ]  BasicLit - String_lit.\n"); }
	;
	
OperandName :
	IDENTIFIER				            	{ printf("[%s] OperandName - Identifier.\n", yylval.buffer); }
	| QualifiedIdent			        	{ printf("[     ]  OperandName - QualifiedIdent.\n"); }
	
CompositeLit :
	LiteralType LiteralValue				{ printf("[     ]  CompositeLit - LiteralType LiteralValue.\n"); }
	;
	
LiteralType :
	StructType					        { printf("[     ]  LiteralType - StructType.\n"); }
	| ArrayType					        { printf("[     ]  LiteralType - ArrayType.\n"); }
	| '[' MULTIDOT ']' ElementType				{ printf("[     ]  LiteralType - [ ... ] ElementType.\n"); }
	| SliceType					        { printf("[     ]  LiteralType - SliceType.\n"); }
	| MapType					        { printf("[     ]  LiteralType - MapType.\n"); }
	| TypeName                          			{ printf("[     ]  LiteralType - TypeName.\n"); }
	| TypeName TypeArgs     				{ printf("[     ]  LiteralType - TypeName TypeArgs.\n"); }
	;
	
LiteralValue :
    '{' '}'                         				{ printf("[     ]  LiteralValue - { }.\n"); }
    | '{' ElementList '}'                           		{ printf("[     ]  LiteralValue - { ElementList }.\n"); }
    | '{' ElementList ',' '}'                           	{ printf("[     ]  LiteralValue - { ElementList , }.\n"); }
    ;
	
ElementList :
	KeyedElement						{ printf("[     ]  ElementList - KeyedElement.\n"); }
	| ElementList ',' KeyedElement				{ printf("[     ]  ElementList - ElementList , KeyedElement.\n"); }
	;
    
KeyedElement :
    Element                             			{ printf("[     ]  KeyedElement - Element.\n"); }
    | Key ':' Element                               		{ printf("[     ]  KeyedElement - Key : Element.\n"); }
    ;
 	
Key :
 	FieldName					        { printf("[     ]  Key - FieldName.\n"); } 
 	| Expression					    	{ printf("[     ]  Key - Expression.\n"); } 
 	| LiteralValue					    	{ printf("[     ]  Key - LiteralValue.\n"); }
 	;
 	
FieldName : 
	IDENTIFIER					        { printf("[%s] FieldName - identifier.\n", yylval.buffer); }
	;
	
Element : 
	Expression					        { printf("[     ]  Element - Expression.\n"); }
	| LiteralValue					    	{ printf("[     ]  Element - LiteralValue.\n"); }
	;

Type :
    VAR_TYPE                            			{ printf("[%s]  Type - VAR_TYPE.\n", yylval.buffer); }
    | TypeName                          			{ printf("[     ]  Type - TypeName.\n"); }
	| TypeName TypeArgs        	        		{ printf("[     ]  Type - Typename TypeArgs.\n"); }
	| TypeLit			                	{ printf("[     ]  Type - TypeLit.\n"); }
	| '(' Type ')'			            		{ printf("[     ]  Type - ( Type ).\n"); }
	;

TypeName :
	IDENTIFIER 			                	{ printf("[%s] TypeName - identifier.\n", yylval.buffer); }
	| QualifiedIdent 		            		{ printf("[     ]  TypeName - QualifiedIdent.\n"); }
	;

TypeArgs :
    '[' TypeList ']'                    			{ printf("[     ]  TypeArgs - [ TypeList ].\n"); }
	'[' TypeList ',' ']' 	    				{ printf("[     ]  TypeArgs - [ TypeList , ].\n"); }
	;
	
TypeList :
    Type                        				{ printf("[     ]  TypeList - Type.\n"); }
    | TypeList ',' Type                     			{ printf("[     ]  TypeList - TypeList , Type.\n"); }
    ;
	
TypeLit :
	ArrayType			                	{ printf("[     ]  TypeLit - ArrayType.\n");}
	| FunctionType			            		{ printf("[     ]  TypeLit - FunctionType.\n"); }
	| StructType			            		{ printf("[     ]  TypeLit - StructType.\n"); }
	| PointerType			            		{ printf("[     ]  TypeLit - PointerType.\n"); } 
	| InterfaceType		                		{ printf("[     ]  TypeLit - InterfaceType.\n"); } 
	| SliceType			                	{ printf("[     ]  TypeLit - SliceType.\n"); }
	| MapType			                	{ printf("[     ]  TypeLit - MapType.\n"); }
	| ChannelType			            		{ printf("[     ]  TypeLit - ChannelType.\n"); }
	;

ArrayType :
	'[' ArrayLength ']' ElementType     			{ printf("[     ]  ArrayType - [ ArrayLength ] ElemenType.\n"); }
	;

ArrayLength :
	Expression			                	{ printf("[     ]  ArrayLength - Expression.\n"); }
	;
	
ElementType :
	Type				                	{ printf("[     ]  ElementType - Type.\n"); }
	;

FunctionType :
	FUNC Signature 		                		{ printf("[     ]  FunctionType - func Signature.\n"); }
	;
	
StructType :
	STRUCT '{' FieldDecls '}'				{ printf("[     ]  StructType - struct { FieldDecls }.\n"); }
	;
	
FieldDecls :
    | FieldDecls FieldDecl              			{ printf("[     ]  FieldDecls - FieldDecls FieldDecl.\n"); }
    ;
    
FieldDecl :
    IdentifierList Type                 			{ printf("[     ]  FieldDecl - IdentifierList Type.\n"); }
	| IdentifierList Type Tag     	    			{ printf("[     ]  FieldDecl - IdentifierList Type Tag.\n"); }
	| EmbeddedField                     			{ printf("[     ]  FieldDecl - EmbeddedField.\n"); }
	| EmbeddedField Tag     				{ printf("[     ]  FieldDecl - EmbeddedField Tag.\n"); }
	;
	
EmbeddedField :
    TypeName                        				{ printf("[     ]  EmbeddedField - TypeName.\n"); }
    | '*' TypeName                      			{ printf("[     ]  EmbeddedField - * TypeName.\n"); }
    | TypeName TypeArgs                     			{ printf("[     ]  EmbeddedField - TypeName TypeArgs.\n"); }
    | '*' TypeName TypeArgs                     		{ printf("[     ]  EmbeddedField - * TypeName TypeArgs.\n"); }
    ;
 	
Tag : 
	STRING					            	{ printf("[     ]  Tag - string.\n"); }
	;
	
PointerType :
    '*' BaseType                        			{ printf("[     ]  PointerType - * BaseType.\n"); }
    ;

BaseType :
    Type                                			{ printf("[     ]  BaseType - Type.\n"); }
    ;
    
InterfaceType :
    INTERFACE '{' InterfaceElems '}'         			{ printf("[     ]  InterfaceType - interface { InterfaceElems }.\n"); }
    ;

InterfaceElems :
    | InterfaceElems InterfaceElem                  		{ printf("[     ]  InterfaceElems - InterfaceElems InterfaceElem.\n"); }
    ;
    
InterfaceElem :
    MethodElem                          			{ printf("[     ]  InterfaceElem - MethodElem.\n"); }
    | TypeElem                          			{ printf("[     ]  InterfaceElem - TypeElem.\n"); }
    ;

MethodElem :
    MethodName Signature                			{ printf("[     ]  MethodElem - MethodName Signature.\n"); }
    ;
    
MethodName :
    IDENTIFIER                          			{ printf("[%s] MethodName - identifier.\n", yylval.buffer); }
    ;
    
TypeElem :
    TypeTerm                            			{ printf("[     ]  TypeElem - TypeTerm.\n"); }
    | TypeElem '|' TypeTerm             			{ printf("[     ]  TypeElem - TypeElem | TypeTerm.\n"); }
    ;
    
TypeTerm :
    Type                                			{ printf("[     ]  TypeTerm - Type\n"); }
    | UnderlyingType                    			{ printf("[     ]  TypeTerm - UnderlyingType\n"); }
    ;

UnderlyingType :
    '~' Type                            			{ printf("[     ]  UnderlyingType - ~ Type.\n"); }
    ;
    
SliceType :
    '[' ']' ElementType                 			{ printf("[     ]  SliceType - [ ] ElemType.\n"); }
    ;

MapType :
    MAP '[' KeyType ']' ElementType     			{ printf("[     ]  MapType - map [ KeyType ] ElementType.\n"); }
    ;

KeyType :
    Type                                			{ printf("[     ]  KeyType - Type.\n"); }
    ;

ChannelType :    
    CHAN ElementType                    			{ printf("[     ]  ChannelType - chan ElementType.\n"); }
    | CHAN ARROW_LEFT ElementType       			{ printf("[     ]  ChannelType - chan <- ElementType.\n"); }
    | ARROW_LEFT CHAN ElementType       			{ printf("[     ]  ChannelType - <- chan ElementType.\n"); }
    ;

int_lit :
    decimal_lit                         			{ printf("[     ]  int_lit - decimal_lit.\n"); }
    | binary_lit                        			{ printf("[     ]  int_lit - binary_lit.\n"); }           
    | octal_lit                         			{ printf("[     ]  int_lit - octal_lit.\n"); }
    | hex_lit                           			{ printf("[     ]  int_lit - hex_lit.\n"); }
    ;

decimal_lit :
    '0'                                 			{ printf("[     ]  decimal_lit - 0.\n"); }                      
    | DIGIT                             			{ printf("[%d]  decimal_lit - digit.\n", yylval.digit); }
    | DIGIT decimal_digits              			{ printf("[%d]  decimal_lit - digit decimal_digits.\n", yylval.digit); }                 
    | DIGIT '_' decimal_digits          			{ printf("[%d]  decimal_lit - digit _ decimal_digits.\n", yylval.digit); }                         
    ;

binary_lit :
    '0' 'b' binary_digits                   			{ printf("[     ]  binary_lit - 0 b binary_digits.\n"); }
    | '0' 'B' binary_digits                 			{ printf("[     ]  binary_lit - 0 B binary_digits.\n"); }
    | '0' 'b' '_' binary_digits                 		{ printf("[     ]  binary_lit - 0 b _ binary_digits.\n"); }
    | '0' 'B' '_' binary_digits                 		{ printf("[     ]  binary_lit - 0 B _ binary_digits.\n"); }
    ;

octal_lit :
    '0' 'o' octal_digits                    			{ printf("[     ]  octal_lit - 0 o octal_digits.\n"); }
    | '0' 'O' octal_digits                  			{ printf("[     ]  octal_lit - 0 O octal_digits.\n"); }
    | '0' 'o' '_' octal_digits                  		{ printf("[     ]  octal_lit - 0 o _ octal_digits.\n"); }
    | '0' 'O' '_' octal_digits                  		{ printf("[     ]  octal_lit - 0 O _ octal_digits.\n"); }
    ;

hex_lit :
    '0' 'x' hex_digits                  			{ printf("[     ]  hex_lit - 0 x hex_digits"); }
    | '0' 'X' hex_digits                    			{ printf("[     ]  hex_lit - 0 X hex_digits"); }
    | '0' 'x' '_' hex_digits                    		{ printf("[     ]  hex_lit - 0 x _ hex_digits"); }
    | '0' 'X' '_' hex_digits                    		{ printf("[     ]  hex_lit - 0 X _ hex_digits"); }
    ;

decimal_digits :
    decimal_digit                       			{ printf("[     ]  decimal_digits - decimal_digit.\n"); }
    | decimal_digits decimal_digit                      	{ printf("[     ]  decimal_digits - decimal_digits decimal_digit.\n"); }
    | decimal_digits '_' decimal_digit                      	{ printf("[     ]  decimal_digits - decimal_digits _ decimal_digit.\n"); }
    ;

binary_digits :
    binary_digit                       				{ printf("[     ]  binary_digits - binary_digit.\n"); }
    | binary_digits binary_digit                      		{ printf("[     ]  binary_digits - binary_digits binary_digit.\n"); }
    | binary_digits '_' binary_digit                      	{ printf("[     ]  binary_digits - binary_digits _ binary_digit.\n"); }
    ;

octal_digits :
    octal_digit                       				{ printf("[     ]  octal_digits - octal_digit.\n"); }
    | octal_digits octal_digit                      		{ printf("[     ]  octal_digits - octal_digits octal_digit.\n"); }
    | octal_digits '_' octal_digit                      	{ printf("[     ]  octal_digits - octal_digits _ octal_digit.\n"); }
    ;

hex_digits :
    hex_digit                       				{ printf("[     ]  hex_digits - hex_digit.\n"); }
    | hex_digits hex_digit                      		{ printf("[     ]  hex_digits - hex_digits hex_digit.\n"); }
    | hex_digits '_' hex_digit                      		{ printf("[     ]  hex_digits - hex_digits _ hex_digit.\n"); }
    ;

decimal_digit :
    DIGIT                           				{ printf("[%d]  decimal_digit - digit.\n", yylval.digit); }
    ;

binary_digit :
    '0'                             				{ printf("[     ]  binary_digit - 0\n"); }
    | '1'                           				{ printf("[     ]  binary_digit - 1\n"); }
    ;

octal_digit :
    OCT                             				{ printf("[     ]  octal_digit - oct\n"); }
    ;

hex_digit :
    HEX                             				{ printf("[     ]  hex_digit - hex.\n"); }
    ;

float_lit :
    decimal_float_lit                   			{ printf("[     ]  float_lit - decimal_float_lit\n"); }
    | hex_float_lit                     			{ printf("[     ]  float_lit - hex_float_lit\n"); }
    ;

decimal_float_lit :
    decimal_digits '.'                      			{ printf("[     ]  decimal_float_lit - decimal_digits ..\n"); }
    | decimal_digits '.' decimal_digits                     	{ printf("[     ]  decimal_float_lit - decimal_digits . decimal_digits.\n"); }
    | decimal_digits '.' decimal_exponent                       { printf("[     ]  decimal_float_lit - decimal_digits . decimal_exponent.\n"); }
    | decimal_digits '.' decimal_digits decimal_exponent        { printf("[     ]  decimal_float_lit - decimal_digits . decimal_digits decimal_exponent.\n"); }
    | decimal_digits decimal_exponent                       	{ printf("[     ]  decimal_float_lit - decimal_digits decimal_exponent.\n"); }
    | '.' decimal_digits                        		{ printf("[     ]  decimal_float_lit - . decimal_digits.\n"); }
    | '.' decimal_digits decimal_exponent                       { printf("[     ]  decimal_float_lit - . decimal_digits decimal_exponent.\n"); }
    ;

decimal_exponent :
    'e' decimal_digits                  			{ printf("[     ]  deicmal_exponent - 'e' decimal_digits.\n"); }
    | 'E' decimal_digits                    			{ printf("[     ]  deicmal_exponent - 'E' decimal_digits.\n"); }
    | 'e' '+' decimal_digits                    		{ printf("[     ]  deicmal_exponent - 'e' '+' decimal_digits.\n"); }
    | 'e' '-' decimal_digits                    		{ printf("[     ]  deicmal_exponent - 'e' '-' decimal_digits.\n"); }
    | 'E' '+' decimal_digits                    		{ printf("[     ]  deicmal_exponent - 'E' '+' decimal_digits.\n"); }
    | 'E' '-' decimal_digits                    		{ printf("[     ]  deicmal_exponent - 'E' '-' decimal_digits.\n"); }
    ;

hex_float_lit :
    '0' 'x' hex_mantissa hex_exponent       			{ printf("[     ]  hex_float_lit - '0' 'x' hex_mantissa hex_exponent.\n"); }
    | '0' 'X' hex_mantissa hex_exponent     			{ printf("[     ]  hex_float_lit - '0' 'X' hex_mantissa hex_exponent.\n"); }
    ;

hex_mantissa :
    hex_digits '.'                      			{ printf("[     ]  hex_mantissa - hex_digits ..\n"); }
    | '_' hex_digits '.'                        		{ printf("[     ]  hex_mantissa - _ hex_digits ..\n"); }
    | hex_digits '.' hex_digits                     		{ printf("[     ]  hex_mantissa - hex_digits . hex_digits.\n"); }
    | '_' hex_digits '.' hex_digits                     	{ printf("[     ]  hex_mantissa - _ hex_digits . hex_digits.\n"); }
    | hex_digits                        			{ printf("[     ]  hex_mantissa - hex_digits.\n"); }
    | '_' hex_digits                        			{ printf("[     ]  hex_mantissa - _ hex_digits.\n"); }
    | '.' hex_digits                        			{ printf("[     ]  hex_mantissa - . hex_digits.\n"); }
    ;

hex_exponent :
    'p' decimal_digits                      			{ printf("[     ]  hex_exponent - p decimal_digits.\n"); }
    | 'P' decimal_digits                        		{ printf("[     ]  hex_exponent - P decimal_digits.\n"); }
    | 'p' '+' decimal_digits                        		{ printf("[     ]  hex_exponent - p + decimal_digits.\n"); }
    | 'p' '-' decimal_digits                        		{ printf("[     ]  hex_exponent - p - decimal_digits.\n"); }
    | 'P' '+' decimal_digits                        		{ printf("[     ]  hex_exponent - P + decimal_digits.\n"); }
    | 'P' '-' decimal_digits                        		{ printf("[     ]  hex_exponent - P - decimal_digits.\n"); }
    ;

imaginary_lit :
    decimal_digits 'i'                  			{ printf("[     ]  imaginary_lit - decimal_digits\n"); }
    | int_lit 'i'                       			{ printf("[     ]  imaginary_lit - int_lit\n"); }
    | float_lit 'i'                     			{ printf("[     ]  imaginary_lit - float_lit\n"); }
    ;

rune_lit :
    '\'' unicode_value '\''             			{ printf("[     ]  rune_lit - ' unicode_value '.\n"); }
    | '\'' byte_value '\''              			{ printf("[     ]  rune_lit - ' byte_value '.\n"); }
    ;
    
unicode_value :
    little_u_value                      			{ printf("[     ]  unicode_value - little_u_value\n"); }
    | big_u_value                       			{ printf("[     ]  unicode_value - big_u_value\n"); }
    | escaped_char                      			{ printf("[     ]  unicode_value - escaped_char\n"); }
    ;
    
byte_value :
    octal_byte_value                    			{ printf("[     ]  byte_value - octal_byte_value\n"); }
    | hex_byte_value                    			{ printf("[     ]  byte_value - hex_byte_value\n"); }
    ;

octal_byte_value :
    OCT_BYTE                            			{ printf("[     ]  octal_byte_value - oct_byte.\n"); }
    ;

hex_byte_value :
    HEX_BYTE                            			{ printf("[     ]  hex_byte_value - hex_byte.\n"); }
    ;

little_u_value :
    LITTLE_U                            			{ printf("[     ]  little_u_value - little_u.\n"); }
    ;

big_u_value :
    BIG_U                               			{ printf("[     ]  big_u_value - big_u.\n"); }
    ;

escaped_char :
    ESCAPED CHAR                        			{ printf("[     ]  escaped_char - escaped_char.\n"); }
    ;
    
Block :
	'{' StatementLists '}'				        { printf("[     ]  Block - { StatementLists }.\n"); }
	;
	
StatementLists :
    | StatementLists StatementList      			{ printf("[     ]  StatementLists - StatementLists StatementList.\n"); }
    ;
    
StatementList :
    Statement                           			{ printf("[     ]  StatementList - Statement.\n"); }
    | StatementList Statement           			{ printf("[     ]  StatementList - StatementList Statement.\n"); }
    ;

Statement :
    Declaration                         			{ printf("[     ]  Statement - Declaration\n"); }
    | LabeledStmt                       			{ printf("[     ]  Statement - LabeledStmt\n"); }
    | SimpleStmt                        			{ printf("[     ]  Statement - SimpleStmt\n"); }
    | GoStmt                            			{ printf("[     ]  Statement - GoStmt\n"); }
    | ReturnStmt                        			{ printf("[     ]  Statement - ReturnStmt\n"); }
    | BreakStmt                         			{ printf("[     ]  Statement - BreakStmt\n"); }
    | ContinueStmt                      			{ printf("[     ]  Statement - ContinueStmt\n"); }
    | GotoStmt                          			{ printf("[     ]  Statement - GotoStmt\n"); }
    | FallthroughStmt                   			{ printf("[     ]  Statement - FallthroughStmt\n"); }
    | Block                             			{ printf("[     ]  Statement - Block\n"); }
    | IfStmt                            			{ printf("[     ]  Statement - IfStmt\n"); }
    | SwitchStmt                        			{ printf("[     ]  Statement - SwitchStmt\n"); }
    | SelectStmt                        			{ printf("[     ]  Statement - SelectStmt\n"); }
    | ForStmt                           			{ printf("[     ]  Statement - ForStmt\n"); }
    | DeferStmt                         			{ printf("[     ]  Statement - DeferStmt\n"); }
    ;
    
SelectStmt : 
    SELECT '{' CommClauses '}'       				{ printf("[     ]  SelectStmt - select { CommClauses }.\n"); }
    ;
    
CommClauses :
    | CommClauses CommClause                			{ printf("[     ]  CommClauses - CommClauses CommClause.\n"); }
    ;
    
CommClause :
    CommCase ':' StatementList              			{ printf("[     ]  CommClause - CommCase : StatementList.\n"); }
    ;
    
CommCase :
    CASE SendStmt                           			{ printf("[     ]  CommCase - SendStmt.\n"); }
    | CASE RecvStmt                         			{ printf("[     ]  CommCase - RecvStmt.\n"); }
    | DEFAULT                               			{ printf("[     ]  CommCase - default.\n"); }
    ;
    
RecvStmt :
    RecvExpr                                                	{ printf("[     ]  RecvStmt - RecvExpr.\n"); }
    | ExpressionList '=' RecvExpr                           	{ printf("[     ]  RecvStmt - ExpressionList = RecvExpr.\n"); }
    | IdentifierList ASSIGN RecvExpr                          	{ printf("[     ]  RecvStmt - IdentifierList := RecvExpr.\n"); }
    ;
    
RecvExpr :
    Expression                              			{ printf("[     ]  RecvExpr - Expression.\n"); }
    ;
    
PostStmt :
    SimpleStmt                          			{ printf("[     ]  PostStmt - SimpleStmt.\n"); }
    
SimpleStmt :
    EmptyStmt                           			{ printf("[     ]  SimpleStmt - EmptyStmt\n"); }
    | ExpressionStmt                    			{ printf("[     ]  SimpleStmt - ExpressionStmt\n"); }
    | SendStmt                          			{ printf("[     ]  SimpleStmt - SendStmt\n"); }
    | IncDecStmt                        			{ printf("[     ]  SimpleStmt - IncDecStmt\n"); }
    | Assignment                        			{ printf("[     ]  SimpleStmt - Assignment\n"); }
    | ShortVarDecl                      			{ printf("[     ]  SimpleStmt - ShortVarDecl\n"); }
    ;
    
TypeSwitchStmt :
    SWITCH TypeSwitchGuard '{' TypeCaseClauses                          { printf("[     ]  TypeSwitchStmt - switch TypeSwitchGuard { TypeCaseClause }.\n"); }
    | SWITCH SimpleStmt ';' TypeSwitchGuard '{' TypeCaseClauses '}'     { printf("[     ]  TypeSwitchStmt - switch SimpleStmt ; TypeSwitchGuard { TypeCaseClauses }.\n"); }
    ;

TypeSwitchGuard :
    PrimaryExpr '.' '(' TYPE ')'                               	{ printf("[     ]  TypeSwitchGuard - PrimaryExpr . ( Type ).\n"); }
    | IDENTIFIER ASSIGN PrimaryExpr '.' '(' TYPE ')'              { printf("[%s] TypeSwitchGuard - Identifier := PrimaryExpr . ( Type ).\n", yylval.buffer); }
    ;
   
TypeCaseClauses :
    | TypeCaseClauses TypeCaseClause                            { printf("[     ]  TypeCaseClauses - TypeCaseClauses TypeCaseClause.\n"); }
    ;
    
TypeCaseClause :
    TypeSwitchCase ':' StatementList                            { printf("[     ]  TypeCaseClause - TypeSwitchCase : StatementList.\n"); }
    ;
   
TypeSwitchCase :
    CASE TypeList                                               { printf("[     ]  TypeSwitchCase - case TypeList.\n"); }
    | DEFAULT                                                   { printf("[     ]  TypeSwitchCase - default.\n"); }
    ;

Declaration :
    ConstDecl                           			{ printf("[     ]  Declaration - ConstDecl\n"); }
    | TypeDecl                          			{ printf("[     ]  Declaration - TypeDecl\n"); }
    | VarDecl                           			{ printf("[     ]  Declaration - VarDecl\n"); }
    ;

TopLevelDecls :
    | TopLevelDecls TopLevelDecl        			{ printf("[     ]  TopLevelDecls - TopLevelDecls TopLevelDecl.\n"); }
    ;

TopLevelDecl :
    Declaration                         			{ printf("[     ]  TopLevelDecl - Declaration.\n"); }
    | FunctionDecl                      			{ printf("[     ]  TopLevelDecl - FunctionDecl.\n"); }
    | MethodDecl                        			{ printf("[     ]  TopLevelDecl - MethodDecl.\n"); }
    ;

ConstDecl :
    CONST ConstSpec                                       	{ printf("[     ]  ConstDecl - const ConstSpec.\n"); }  
    | CONST '(' ConstSpecs ')'                 			{ printf("[     ]  ConstDecl - const ( ConstSpecs ).\n"); }
    ;

ConstSpecs :
    | ConstSpecs ConstSpec                              	{ printf("[     ]  ConstSpecs - ConstSpecs ConstSpec.\n"); }
    ;

ConstSpec :
    IdentifierList                                      	{ printf("[     ]  ConstSpec - IdentifierList.\n"); }
    | IdentifierList '=' ExpressionList                 	{ printf("[     ]  ConstSpec - IdentifierList = ExpressionList.\n"); }
    | IdentifierList Type '=' ExpressionList            	{ printf("[     ]  ConstSpec - IdentifierList Type = ExpressionList.\n"); }
    ;

TypeDecl :
    TYPE TypeSpec                                               { printf("[     ]  TypeDecl - type TypeSpec.\n"); }
    | TYPE '(' TypeSpecs ')'                                    { printf("[     ]  TypeDecl - type ( TypeSpecs ).\n"); }
    ;
    
TypeSpecs :
    | TypeSpecs TypeSpec                                        { printf("[     ]  TypeSpecs - TypeSpecs TypeSpec.\n"); }
    ;
    
TypeSpec :
    AliasDecl                                                   { printf("[     ]  TypeSpec - AliasDecl\n"); }
    | TypeDef                                                   { printf("[     ]  TypeSpec - TypeDef\n"); }
    ;

AliasDecl :
    IDENTIFIER '=' Type                                         { printf("[%s] AliasDecl - identifier = Type.\n", yylval.buffer); }
    ;

TypeDef :
    IDENTIFIER Type                                             { printf("[%s] TypeDef - Identifier Type.\n", yylval.buffer); }
    | IDENTIFIER TypeParameters  Type                           { printf("[%s] TypeDef - Identifier TypeParameters Type.\n", yylval.buffer); }
    ;

MethodDecl :
    FUNC Receiver MethodName Signature                          { printf("[     ]  MethodDecl - func Receiver MethodName Signature.\n"); }
    | FUNC Receiver MethodName Signature FunctionBody           { printf("[     ]  MethodDecl - func Receiver MethodName Signature FunctionBody.\n"); }
    ;

Conversion :
    Type '(' Expression ')'                                     { printf("[     ]  Conversion - Type ( Expression ).\n"); }
    | Type '(' Expression ',' ')'                               { printf("[     ]  Conversion - Type ( Expression , ).\n"); }
    ;

Receiver :
    Parameters                                                  { printf("[     ]  Receiver - Parameters.\n"); }
    ;
    
MethodExpr :
    ReceiverType '.' MethodName                                 { printf("[     ]  MethodExpr - ReceiverType . MethodName\n"); }
    ;
    
ReceiverType :
    Type                                                        { printf("[     ]  ReceiverType - Type.\n"); }
    ;
    
LabeledStmt :
    Label ':' Statement                                         { printf("[     ]  LabeledStmt - Label : Statement.\n"); }
    ;

Label :
    IDENTIFIER                                                  { printf("[%s] Label - Identifier.\n", yylval.buffer); }
    ;

EmptyStmt :
                                                                { printf("[     ]  EmptyStmt - nothing.\n"); }
    ;

ExpressionStmt :
    Expression                                                  { printf("[     ]  ExpressionStmt - Expression.\n"); }
    ;

SendStmt :
    Channel ARROW_LEFT Expression                                     { printf("[     ]  SendStmt - Channel <- Expression.\n"); }
    ;

Channel :
    Expression                                                  { printf("[     ]  Channel - Expression.\n"); }
    ;

IncDecStmt :
    Expression INCREMENT                                             { printf("[     ]  IncDecStmt - Expression ++.\n"); }
    | Expression DECREMENT                                          { printf("[     ]  IncDecStmt - Expression --.\n"); }
    ;

Assignment :
    ExpressionList assign_op ExpressionList                     { printf("[     ]  Assignment - ExpressionList assign_op ExpressionList.\n"); }
    ;

assign_op :
    add_op '='                                                  { printf("[     ]  assign_op - add_op =.\n"); }
    | mul_op '='                                                { printf("[     ]  assign_op - add_op =.\n"); }
    ;

ShortVarDecl :
    IdentifierList ASSIGN ExpressionList                          { printf("[     ]  ShortVarDecl - IdentifierList := ExpressionList.\n"); }
    ;

GoStmt :
    GO Expression                                               { printf("[     ]  GoStmt - go Expression.\n"); }
    ;

ReturnStmt :
    RETURN                                                      { printf("[     ]  ReturnStmt - return.\n"); }
    | RETURN ExpressionList                                     { printf("[     ]  ReturnStmt - return ExpressionList.\n"); }
    ;

BreakStmt :
    BREAK                                                       { printf("[     ]  BreadStmt - break.\n"); }
    | BREAK Label                                               { printf("[     ]  BreakStmt - break Label.\n"); }    
    ;

ContinueStmt :
    CONTINUE                                                    { printf("[     ]  ContinueStmt - continue.\n"); }
    | CONTINUE Label                                            { printf("[     ]  ContinueStmt - continue Label.\n"); }
    ;

GotoStmt :
    GOTO Label                                                  { printf("[     ]  GotoStmt - goto Label.\n"); }
    ;

FallthroughStmt :
    FALLTHROUGH                                                 { printf("[     ]  FallthroughStmt - fallthrough.\n"); }
    ;

IfStmt :
    IF Expression Block                                         { printf("[     ]  IfStmt - if Expression Block.\n"); }
    | IF SimpleStmt Expression Block                            { printf("[     ]  IfStmt - if SimpleStmt Expression Block.\n"); }
    | IF Expression Block ELSE IfStmt                           { printf("[     ]  IfStmt - if Expression Block else IfStmt.\n"); }
    | IF SimpleStmt Expression Block ELSE IfStmt                { printf("[     ]  IfStmt - if SimpleStmt Expression Block else IfStmt.\n"); }
    | IF Expression Block ELSE Block                            { printf("[     ]  IfStmt - if Expression Block else Block.\n"); }
    | IF SimpleStmt Expression Block ELSE Block                 { printf("[     ]  IfStmt - if SimpleStmt Expression Block else Block.\n"); }
    ;

SwitchStmt :
    ExprSwitchStmt                                              { printf("[     ]  SwitchStmt - ExprSwitchStmt\n"); }
    | TypeSwitchStmt                                            { printf("[     ]  SwitchStmt - TypeSwitchStmt\n"); }
    ;

ExprSwitchStmt :
    SWITCH '{' ExprCaseClauses '}'                              { printf("[     ]  ExprSwitchStmt - switch { ExprCaseClauses }.\n"); }
    | SWITCH SimpleStmt '{' ExprCaseClauses '}'                 { printf("[     ]  ExprSwitchStmt - switch SimpleStmt { ExprCaseClauses }.\n"); }
    | SWITCH Expression '{' ExprCaseClauses '}'                 { printf("[     ]  ExprSwitchStmt - switch Expression { ExprCaseClauses }.\n"); }
    | SWITCH SimpleStmt Expression '{' ExprCaseClauses '}'      { printf("[     ]  ExprSwitchStmt - switch SimpleStmt Expression { ExprCaseClauses }.\n"); }
    ;

ExprCaseClauses :
    | ExprCaseClauses ExprCaseClause                            { printf("[     ]  ExprCaseClauses - ExprCaseClauses ExprCaseClause.\n"); }
    ;
    
ExprCaseClause :
    ExprSwitchCase ':' StatementList                            { printf("[     ]  ExprCaseClause - ExprSwitchCase : StatementList.\n"); }
    ;

ExprSwitchCase :
    CASE ExpressionList                                         { printf("[     ]  ExprSwitchCase - case ExpressionList.\n"); }
    | DEFAULT                                                   { printf("[     ]  ExprSwitchCase - default.\n"); }
    ;
    
ForStmt :
    FOR Block                                                   { printf("[     ]  ForStmt - for Block.\n"); }
    | FOR Condition Block                                       { printf("[     ]  ForStmt - for Condition Block.\n"); }
    | FOR ForClause Block                                       { printf("[     ]  ForStmt - for ForClause Block.\n"); }
    | FOR RangeClause Block                                     { printf("[     ]  ForStmt - for RangeClause Block.\n"); }
    ;
    
DeferStmt :
    DEFER Expression                                            { printf("[     ]  DeferStmt - defer Expression.\n"); }
    ;
    
InitStmt :
    SimpleStmt                                                  { printf("[     ]  InitStmt - SimpleStmt.\n"); }
    ;
    
RangeClause :
    RANGE Expression                                            { printf("[     ]  RangeClause - range Expression.\n"); }
    | ExpressionList '=' RANGE Expression                       { printf("[     ]  RangeClause - ExpressionList = range Expression.\n"); }
    | IdentifierList ASSIGN RANGE Expression                      { printf("[     ]  RangeClause - IdentifierList := range Expression.\n"); }
    ;
    
Condition :
    Expression              					{ printf("[     ]  Condition - Expression.\n"); }
    ;

ForClause :
               ';'           ';'                            	{ printf("[     ]  ForClause - ; ;.\n"); }
    | InitStmt ';'           ';'                            	{ printf("[     ]  ForClause - InitStmt ; ;.\n"); }
    |          ';' Condition ';'                            	{ printf("[     ]  ForClause - ; Condition ;.\n"); }
    |          ';'           ';' PostStmt                   	{ printf("[     ]  ForClause - ; ; PostStmt.\n"); }
    | InitStmt ';' Condition ';'                            	{ printf("[     ]  ForClause - InitStmt ; Condition ;.\n"); }
    |          ';' Condition ';' PostStmt                   	{ printf("[     ]  ForClause - ; Condition ; PostStmt.\n"); }
    | InitStmt ';'           ';' PostStmt                   	{ printf("[     ]  ForClause - InitStmt ; ; PostStmt.\n"); }
    | InitStmt ';' Condition ';' PostStmt                   	{ printf("[     ]  ForClause - InitStmt ; Condition ; PostStmt.\n"); }
    ;
	
IdentifierList :
    IDENTIFIER                                              	{ printf("[%s] IdentifierList - Identifier.\n", yylval.buffer); }
    | IdentifierList ',' IDENTIFIER                         	{ printf("[%s] IdentifierList - IdentifierList Identifier.\n", yylval.buffer); }
    ;
	
FunctionDecl :
    FUNC FunctionName Signature                                 { printf("[     ]  FunctionDecl - func FunctionName Signature.\n"); }
    | FUNC FunctionName TypeParameters Signature                { printf("[     ]  FunctionDecl - func FunctionName TypeParameters Signature.\n"); }
    | FUNC FunctionName Signature FunctionBody                  { printf("[     ]  FunctionDecl - func FunctionName Signature FunctionBody.\n"); }
    | FUNC FunctionName TypeParameters Signature FunctionBody   { printf("[     ]  FunctionDecl - func FunctionName TypeParameters Signature FunctionBody.\n"); }
    ;
    
FunctionName :
	IDENTIFIER	                                        { printf("[%s] FunctionName - identifier.\n", yylval.buffer); }
	;

FunctionBody :
	Block		                                        { printf("[     ]  FunctionBody - Block.\n"); }
	;
	
Signature :
    Parameters							{ printf("[     ]  Signature - Parameters.\n"); }
    | Parameters Result			        		{ printf("[     ]  Signature - Parameters Result.\n"); }
	;
	
Parameters :
    '('')'                             				{ printf("[     ]  Parameters - ( ).\n"); }
    | '(' ParameterList ')'             			{ printf("[     ]  Parameters - ( ParametersList ).\n"); }
    | '(' ParameterList ',' ')' 	    			{ printf("[     ]  Parameters - ( ParametersList , ).\n"); }
	;
	
Result :
	Parameters 					        { printf("[     ]  Result - Parameters.\n"); }
	| Type						        { printf("[     ]  Result - Type.\n"); }
	;
	
ParameterList :
    ParameterDecl						{ printf("[     ] ParameterList - ParameterDecl.\n"); }
    | ParameterList ',' ParameterDecl				{ printf("[     ]  ParameterList - ParameterList , ParameterDecl.\n"); }
	;

ParameterDecl :
    Type                                			{ printf("[     ]  ParameterDecl - Type.\n"); }
    | IdentifierList Type               			{ printf("[     ]  ParameterDecl - IdentifierList Type.\n"); }
    | MULTIDOT Type                        			{ printf("[     ]  ParameterDecl - ... Type.\n"); }
    | IdentifierList MULTIDOT Type         			{ printf("[     ]  ParameterDecl - IdentifierList ... Type.\n"); }
    ;
	
TypeParameters :
    '[' TypeParamList ']'                                   	{ printf("[     ]  TypeParameters - [ TypeParamList ].\n"); }
	'[' TypeParamList ',' ']' 		                { printf("[     ]  TypeParameters - [ TypeParamList , ].\n"); }
	;
	
TypeParamList :
    TypeParamDecl                                           	{ printf("[     ]  TypeParamList - TypeParamDecl.\n"); }
    | TypeParamList ',' TypeParamDecl                       	{ printf("[     ]  TypeParamList - TypeParamList , TypeParamDecl.\n"); }
    ;
	
TypeConstraint :
	TypeElem					        { printf("[     ]  TypeConstraint - TypeElem.\n"); }
	;

TypeParamDecl :
	IdentifierList TypeConstraint 			        { printf("[     ]  TypeParamDecl - IdentifierList TypeConstraint.\n"); }
	;

QualifiedIdent :
	PackageName '.' IDENTIFIER	                        { printf("[%s] QualifiedIdent - PackageName . identifier.\n", yylval.buffer); }
	;

%%

int main() {
    yylval.buffer[FIELD] = 0;
    printf("[     ]  \n\n");
	yyparse();
	return 0;
}
int yyerror(const char* s) {
	printf("[     ]  Error: %s\n", s);
	return 1;
}
int yywrap() {
	return 1;
}