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
%token GOTO FALLTHROUGH DEFER CHAN IMPORT FUNC BREAK CASE CONST STRING ARROW_LEFT
%token CONTINUE DEFAULT ELSE FOR GO IF RANGE RETURN STRUCT SWITCH TYPE VAR
%token HEX_BYTE LITTLE_U BIG_U ESCAPED CHAR SELECT OCT HEX OCT_BYTE SIGNATURE


%%

SourceFile :
    PackageClause ImportDecls TopLevelDecls         { printf("SourceFile.\n"); exit(0); }
    ;
    
PackageClause : 
    PACKAGE PackageName                             { printf("PackageClause.\n"); }
    ;
    
PackageName :
	IDENTIFIER			                            { printf("PackageName - identifier.\n"); }
	;
	
ImportDecls :
    ImportDecl
    //| ImportDecls ImportDecl                        { printf("ImportDecls.\n"); }
    ;
    
ImportDecl :
    IMPORT ImportSpec                               { printf("ImportDecl - import ImportSpec.\n"); }
    //| IMPORT '(' ImportSpecs ')'                    { printf("ImportDecl - import ( ImportSpec ).\n"); }
    ;
    
ImportSpecs :
    | ImportSpecs ImportSpec                        { printf("ImportSpecs.\n"); }
    ;

ImportSpec :
    ImportPath                                      { printf("ImportSpec: ImportPath.\n"); }
    //| '.' ImportPath                                { printf("ImportSpec: . ImportPath.\n"); }
    //| PackageName ImportPath                        { printf("ImportSpec: PackageName ImportPath.\n"); }
    ;
    
ImportPath :
    STRING                                          { printf("ImportPath.\n"); }
    ;

VarDecl :
	VAR VarSpec				                        { printf("VarDecl - VAR VarSpec.\n"); exit(0); }
	| VAR '(' VarSpecs ')'	                        { printf("VarDecl - VAR ( VarSpec ; ).\n"); exit(0); }
	;
	
VarSpecs :
    | VarSpecs VarSpec                              { printf("VarSpecs.\n"); }
    ;
    
VarSpec :
    IdentifierList Type                 { printf("VarSpec: IdentifierList Type.\n"); }
    | IdentifierList Type '=' ExpressionList                    { printf("VarSpec: IdentifierList Type = ExpressionList.\n"); }
    | IdentifierList '=' ExpressionList                 { printf("VarSpec: IdentifierList = ExpressionList.\n"); }
    ;
	
ExpressionList :
    Expression                              { printf("ExpressionList - Expression.\n"); }
    | ExpressionList ',' Expression                             { printf("ExpressionList - ExpressionList , Expression.\n"); }
    ;

Expression :
	UnaryExpr						                { printf("Expression - UnaryExpr.\n"); }
	| Expression binary_op Expression			    { printf("Expression - Expression op Expression.\n"); }
	;
	
UnaryExpr :
	PrimaryExpr						                { printf("UnaryExpr - PrimaryExpr.\n"); }
	| unary_op UnaryExpr					        { printf("UnaryExpr - op UnaryExpr.\n"); }
	;
	
binary_op :
	"||"							    { printf("Binary_operator: ||.\n"); }
	| "&&"							    { printf("Binary_operator: &&.\n"); }
	| rel_op					        { printf("Binary_operator: Rel_op.\n"); }
	| add_op					        { printf("Binary_operator: Add_op.\n"); }
	| mul_op						    { printf("Binary_operator: Mul_op.\n"); }
	;
	
rel_op :
	"=="							    { printf("Relativeness_operator: ==.\n"); }
	| "!="							    { printf("Relativeness_operator: !=.\n"); }
	| "<"							    { printf("Relativeness_operator: <.\n"); }
	| "<="							    { printf("Relativeness_operator: <=.\n"); }
	| '>'							    { printf("Relativeness_operator: >.\n"); }
	| ">="							    { printf("Relativeness_operator: >=.\n"); }
	;
	
add_op :
	'+'							        { printf("Addition_operator: +.\n"); }
	| '-'							    { printf("Addition_operator: -.\n"); }
	| '|'							    { printf("Addition_operator: |.\n"); }
	| '^'							    { printf("Addition_operator: ^.\n"); }
	;
	
mul_op :
	'*'							        { printf("Multiply_operator: *.\n"); }
	| '/'							    { printf("Multiply_operator: /.\n"); }
	| '%'							    { printf("Multiply_operator: %%.\n"); }
	| "<<"							    { printf("Multiply_operator: <<.\n"); }
	| ">>"							    { printf("Multiply_operator: >>.\n"); }
	| '&'							    { printf("Multiply_operator: &.\n"); }
	| "&^"							    { printf("Multiply_operator: &^.\n"); }
	;
	
unary_op :
	'+'							        { printf("Unary_operator: +.\n"); }
	| '-'							    { printf("Unary_operator: -.\n"); }
	| '!'							    { printf("Unary_operator: !.\n"); }
	| '^'							    { printf("Unary_operator: ^.\n"); }
	| '*'							    { printf("Unary_operator: *.\n"); }
	| '&'							    { printf("Unary_operator: &.\n"); }
	| "<-"							    { printf("Unary_operator: <-.\n"); }
	;

PrimaryExpr :
	Operand							    { printf("PrimaryExpr - Operand.\n"); }
	| Conversion						{ printf("PrimaryExpr - Conversion.\n"); }
	| MethodExpr						{ printf("PrimaryExpr - MethodExpr.\n"); }
	| PrimaryExpr Selector				{ printf("PrimaryExpr - PrimaryExpr Selector.\n"); }
	| PrimaryExpr Index					{ printf("PrimaryExpr - PrimaryExpr Index.\n"); }
	| PrimaryExpr Slice					{ printf("PrimaryExpr - PrimaryExpr Slice.\n"); }
	| PrimaryExpr TypeAssertion			{ printf("PrimaryExpr - PrimaryExpr TypeAssertion.\n"); }
	| PrimaryExpr Arguments				{ printf("PrimaryExpr - PrimaryExpr Arguments.\n"); }
	;
	
Selector :
	'.' IDENTIFIER						{ printf("Selector.\n");}
	;

Index :
    | Expression					{ printf("Index.\n"); }
	;
	
Slice :
    '[' ':' ']'                                 { printf("Slice - [ : ].\n"); }
    | '[' Expression ':' ']'                                    { printf("Slice - [ Expression : ].\n"); }
    | '[' ':' Expression ']'                                    { printf("Slice - [ : Expression ].\n"); }
    | '[' Expression ':' Expression ']'                                 { printf("Slice - [ Expression : Expression ].\n"); }
    | '[' ':' Expression ':' Expression ']'                                 { printf("Slice - [ : Expression : Expression ].\n"); }
    | '[' Expression ':' Expression ':' Expression ']'                                  { printf("Slice - [ Expression : Expression : Expression ].\n"); }
    ;
	
TypeAssertion :
	'.' '(' Type ')'				    { printf("TypeAssertion.\n"); }
	;
	
Arguments :
    '(' ')'                                 { printf("Arguments: ( ).\n");  }
    | '(' ExpressionList ')'                                    { printf("Arguments: ( ExpressionList ).\n");  }
    | '(' ExpressionList "..." ')'                                  { printf("Arguments: ( ExpressionList ... ).\n");  }
    | '(' ExpressionList ',' ')'                                    { printf("Arguments: ( ExpressionList , ).\n");  }
    | '(' ExpressionList "..." ',' ')'                                  { printf("Arguments: ( ExpressionList ... , ).\n");  }    
    | '(' Type ')'                                  { printf("Arguments: ( Type ).\n");  }
    | '(' Type "..." ')'                                    { printf("Arguments: ( Type ... ).\n");  }
    | '(' Type ',' ')'                                  { printf("Arguments: ( Type , ).\n");  }
    | '(' Type "..." ',' ')'                                    { printf("Arguments: ( Type ... , ).\n");  }   
    | '(' Type ',' ExpressionList ')'                                   { printf("Arguments: ( Type , ExpressionList ).\n");  }
    | '(' Type ',' ExpressionList "..." ')'                                     { printf("Arguments: ( Type , ExpressionList ... ).\n");  }
    | '(' Type ',' ExpressionList ',' ')'                                   { printf("Arguments: ( Type ExpressionList , ).\n");  }
    | '(' Type ',' ExpressionList "..." ',' ')'                                 { printf("Arguments: ( Type ExpressionList ... , ).\n");  }
    ;
	
Operand :
	Literal						        { printf("Operand - Literal.\n"); }
	| OperandName                       { printf("Operand - OperandName.\n"); }
	| OperandName TypeArgs		        { printf("Operand - OperandName TypeArgs.\n"); } 
	| '(' Expression ')'				{ printf("Operand - ( Expression ).\n"); }
	;
	
Literal :
	BasicLit				            { printf("Literal - BasicLit.\n"); } 
	| CompositeLit				        { printf("Literal - CompositeLit.\n"); } 
	| FunctionLit				        { printf("Literal - FunctionLit.\n"); } 
	;
	
FunctionLit :
    FUNC Signature FunctionBody         { printf("FunctionLit.\n"); }
    ;
	
BasicLit :
	int_lit					            { printf("BasicLit - Int_lit.\n"); }
	| float_lit				            { printf("BasicLit - Float_lit.\n"); }
	| imaginary_lit				        { printf("BasicLit - Imaginary_lit.\n"); }
	| rune_lit				            { printf("BasicLit - Rune_lit.\n"); }
	| STRING				            { printf("BasicLit - String_lit.\n"); }
	;
	
OperandName :
	IDENTIFIER				            { printf("OparandName - Identifier.\n"); }
	| QualifiedIdent			        { printf("OperandName - QualifiedIdent.\n"); }
	
CompositeLit :
	LiteralType LiteralValue
	;
	
LiteralType :
	StructType					        { printf("LiteralType - StructType.\n"); }
	| ArrayType					        { printf("LiteralType - ArrayType.\n"); }
	| '[' "..." ']' ElementType			{ printf("LiteralType - [ ... ] ElementType.\n"); }
	| SliceType					        { printf("LiteralType - SliceType.\n"); }
	| MapType					        { printf("LiteralType - MapType.\n"); }
	| TypeName                          { printf("LiteralType - TypeName.\n"); }
	| TypeName TypeArgs     			{ printf("LiteralType - TypeName TypeArgs.\n"); }
	;
	
LiteralValue :
    '{' '}'                         { printf("LiteralValue - { }.\n"); }
    | '{' ElementList '}'                           { printf("LiteralValue - { ElementList }.\n"); }
    | '{' ElementList ',' '}'                           { printf("LiteralValue - { ElementList , }.\n"); }
    ;
	
ElementList :
	KeyedElement KeyedElements		{ printf("ElementList.\n"); }
	;
	
KeyedElements :
    | KeyedElements KeyedElement                { printf("KeyedElements.\n"); }
    ;
    
KeyedElement :
    Element                             { printf("KeyedElement.\n"); }
    | Key ':' Element                               { printf("KeyedElement.\n"); }
    ;
 	
Key :
 	FieldName					        { printf("Key - FieldName.\n"); } 
 	| Expression					    { printf("Key - Expression.\n"); } 
 	| LiteralValue					    { printf("Key - LiteralValue.\n"); }
 	;
 	
FieldName : 
	IDENTIFIER					        { printf("FieldName.\n"); }
	;
	
Element : 
	Expression					        { printf("Element - Expression.\n"); }
	| LiteralValue					    { printf("Element - LiteralValue.\n"); }
	;

Type :
    TypeName                            { printf("Type - TypeName.\n"); }
	| TypeName TypeArgs        	        { printf("Type - Typename TypeArgs.\n"); }
	| TypeLit			                { printf("Type - TypeLit.\n"); }
	| '(' Type ')'			            { printf("Type - ( Type ).\n"); }
	;

TypeName :
	IDENTIFIER 			                { printf("TypeName - identifier.\n"); }
	| QualifiedIdent 		            { printf("TypeName - QualifiedIdent.\n"); }
	;

TypeArgs :
    '[' TypeList ']'                    { printf("TypeArgs - [ TypeList ].\n"); }
	'[' TypeList ',' ']' 	    { printf("TypeArgs - [ TypeList , ].\n"); }
	;
	
TypeList :
    Type                        { printf("TypeList - Type.\n"); }
    | TypeList ',' Type                     { printf("TypeList - TypeList , Type.\n"); }
    ;
	
TypeLit :
	ArrayType			                { printf("TypeLit - ArrayType.\n");}
	| FunctionType			            { printf("TypeLit - FunctionType.\n"); }
	| StructType			            { printf("TypeLit - StructType.\n"); }
	| PointerType			            { printf("TypeLit - PointerType.\n"); } 
	| InterfaceType		                { printf("TypeLit - InterfaceType.\n"); } 
	| SliceType			                { printf("TypeLit - SliceType.\n"); }
	| MapType			                { printf("TypeLit - MapType.\n"); }
	| ChannelType			            { printf("TypeLit - ChannelType.\n"); }
	;

ArrayType :
	'[' ArrayLength ']' ElementType     { printf("ArrayType.\n"); }
	;

ArrayLength :
	Expression			                { printf("ArrayLength.\n"); }
	;
	
ElementType :
	Type				                { printf("ElementType.\n"); }
	;

FunctionType :
	FUNC Signature 		                { printf("FunctionType.\n"); }
	;
	
StructType :
	STRUCT '{' FieldDecls '}'		{ printf("StructType.\n"); }
	;
	
FieldDecls :
    | FieldDecls FieldDecl              { printf("FieldDecls.\n"); }
    ;
    
FieldDecl :
    IdentifierList Type                 { printf("FieldDecl - IdentifierList Type.\n"); }
	| IdentifierList Type Tag     	    { printf("FieldDecl - IdentifierList Type [ Tag ].\n"); }
	| EmbeddedField                     { printf("FieldDecl - EmbeddedField.\n"); }
	| EmbeddedField Tag     			{ printf("FieldDecl - EmbeddedField [ Tag ].\n"); }
	;
	
EmbeddedField :
    TypeName                        { printf("EmbeddedField - TypeName.\n"); }
    | "*" TypeName                      { printf("EmbeddedField - * TypeName.\n"); }
    | TypeName TypeArgs                     { printf("EmbeddedField - TypeName TypeArgs.\n"); }
    | "*" TypeName TypeArgs                     { printf("EmbeddedField - * TypeName TypeArgs.\n"); }
    ;
 	
Tag : 
	STRING					            { printf("Tag.\n"); }
	;
	
PointerType :
    "*" BaseType                        { printf("PointerType.\n"); }
    ;

BaseType :
    Type                                { printf("BaseType.\n"); }
    ;
    
InterfaceType :
    INTERFACE '{' InterfaceElems '}'         { printf("InterfaceType.\n"); };
    ;

InterfaceElems :
    | InterfaceElems InterfaceElem                  { printf("InterfaceElems.\n"); }
    ;
    
InterfaceElem :
    MethodElem                          { printf("InterfaceElem - MethodElem.\n"); }
    | TypeElem                          { printf("InterfaceElem - TypeElem.\n"); }
    ;

MethodElem :
    MethodName SIGNATURE                { printf("MethodElem\n"); }
    ;
    
MethodName :
    IDENTIFIER                          { printf("MethodName\n"); }
    ;
    
TypeElem :
    TypeTerm                            { printf("TypeElem.\n"); }
    | TypeElem "|" TypeTerm             { printf("TypeElem\n"); }
    ;
    
TypeTerm :
    Type                                { printf("TypeTerm - Type\n"); }
    | UnderlyingType                    { printf("TypeTerm - UnderlyingType\n"); }
    ;

UnderlyingType :
    "~" Type                            { printf("UnderlyingType\n"); }
    ;
    
SliceType :
    '[' ']' ElementType                 { printf("SliceType\n"); }
    ;

MapType :
    "map" '[' KeyType ']' ElementType   { printf("MapType\n"); }
    ;

KeyType :
    Type                                { printf("KeyType\n"); }
    ;

ChannelType :    
    CHAN ElementType                    { printf("ChannelType - chan\n"); }
    | CHAN ARROW_LEFT ElementType       { printf("ChannelType - chan <-\n"); }
    | ARROW_LEFT CHAN ElementType       { printf("ChannelType - chan ElementType\n"); }
    ;

int_lit :
    decimal_lit                         { printf("int_lit - decimal_lit\n"); }
    | binary_lit                        { printf("int_lit - binary_lit\n"); }           
    | octal_lit                         { printf("int_lit - octal_lit\n"); }
    | hex_lit                           { printf("int_lit - hex_lit\n"); }
    ;

decimal_lit :
    "0"                                             { printf("decimal_lit - 0\n"); }
    | DIGIT '[' '[' "_" ']' decimal_digits ']'      { printf("decimal_lit - 1...9\n"); }
    ;

binary_lit :
    "0" '(' "b"                           { printf("binary_lit - 0 b\n"); }
    | "B" ')' '[' "_" ']' binary_digits   { printf("binary_lit - B binary_digits\n"); }
    ;

octal_lit :
    "0" '[' "o"                               { printf("binary_lit - 0 o\n"); }
    | "O" ']' '[' "_" ']' octal_digits        { printf("binary_lit - 0 octal_digits\n"); }
    ;

hex_lit :
    "0" '(' "x"                                 { printf("hex_lit - 0 x\n"); }
    | "X" ')' '[' "_" ']' hex_digits            { printf("hex_lit - X hex_digits\n"); }
    ;

decimal_digits :
    decimal_digit '{' '[' "_" ']' decimal_digit '}'     { printf("decimal_digits\n"); }
    ;

binary_digits :
    binary_digit '{' '[' "_" ']' binary_digit '}'       { printf("binary_digits\n"); }
    ;

octal_digits :
    octal_digit '{' '[' "_" ']' octal_digit '}'         { printf("octal_digits\n"); }
    ;

hex_digits :
    hex_digit '{' '[' "_" ']' hex_digit '}'             { printf("hex_digits\n"); }
    ;

decimal_digit :
    DIGIT                           { printf("decimal_digit\n"); }
    ;

binary_digit :
    "0"                             { printf("binary_digit - 0\n"); }
    | "1"                           { printf("binary_digit - 1\n"); }
    ;

octal_digit :
    OCT                             { printf("octal_digit\n"); }
    ;

hex_digit :
    HEX                             { printf("hex_digit.\n"); }
    ;

float_lit :
    decimal_float_lit                   { printf("float_lit - decimal_float_lit\n"); }
    | hex_float_lit                     { printf("float_lit - decimal_float_lit\n"); }
    ;

decimal_float_lit :
    decimal_digits '.' '[' decimal_digits ']' '[' decimal_exponent ']'      { printf("decimal_float_lit - 1\n"); } 
    | decimal_digits decimal_exponent                                       { printf("decimal_float_lit - 2\n"); }
    | '.' decimal_digits '[' decimal_exponent ']'                             { printf("decimal_float_lit - 3\n"); }
    ;

decimal_exponent :
    '(' "e"                                 { printf("decimal_exponent - e\n"); }
    | "E" ')' '[' "+"                       { printf("decimal_exponent - E\n"); }
    | "-" ']' decimal_digits                { printf("decimal_exponent - decimal_digits\n"); }
    ;

hex_float_lit :
    "0" '(' "x"                             { printf("hex_float_lit - 0\n"); }
    | "X" ')' hex_mantissa hex_exponent     { printf("hex_float_lit - X\n"); }
    ;

hex_mantissa :
    '[' "_" ']' hex_digits '.' '[' hex_digits ']'   { printf("hex_mantissa - 1\n"); }
    | '[' "_" ']' hex_digits                        { printf("hex_mantissa - 2\n"); }
    | '.' hex_digits                                { printf("hex_mantissa - 3\n"); }
    ;

hex_exponent :
    '(' "p"                                         { printf("hex_exponent - 1\n"); }
    | "P" ')' '[' "+" | "-" ']' decimal_digits      { printf("hex_exponent - 2\n"); }
    ;

imaginary_lit :
    decimal_digits "i"                  { printf("imaginary_lit - decimal_digits\n"); }
    | int_lit "i"                       { printf("imaginary_lit - int_lit\n"); }
    | float_lit "i"                     { printf("imaginary_lit - float_lit\n"); }
    ;

rune_lit :
    "'" '(' unicode_value               { printf("rune_lit - unicode_value\n"); }
    | byte_value ')' "'"                { printf("rune_lit - byte_value\n"); }
    ;

unicode_value :
    little_u_value                      { printf("unicode_value - little_u_value\n"); }
    | big_u_value                       { printf("unicode_value - big_u_value\n"); }
    | escaped_char                      { printf("unicode_value - escaped_char\n"); }
    ;
    
byte_value :
    octal_byte_value                    { printf("byte_value - octal_byte_value\n"); }
    | hex_byte_value                    { printf("byte_value - hex_byte_value\n"); }
    ;

octal_byte_value :
    OCT_BYTE                            { printf("octal_byte_value\n"); }
    ;

hex_byte_value :
    HEX_BYTE                            { printf("hex_byte_value\n"); }
    ;

little_u_value :
    LITTLE_U                            { printf("little_u_value\n"); }
    ;

big_u_value :
    BIG_U                               { printf("big_u_value\n"); }
    ;

escaped_char :
    ESCAPED CHAR                        { printf("escaped_char.\n"); }
    ;
    
Block :
	'{' StatementLists '}'				        { printf("Block.\n"); }
	;
	
StatementLists :
    | StatementLists StatementList      { printf("StatementLists.\n"); }
    ;
    
StatementList :
    Statement                           { printf("StatementList - Statement.\n"); }
    | StatementList Statement           { printf("StatementList - StatementList Statement.\n"); }
    ;

Statement :
    Declaration                         { printf("Statement - Declaration\n"); }
    | LabeledStmt                       { printf("Statement - LabeledStmt\n"); }
    | SimpleStmt                        { printf("Statement - SimpleStmt\n"); }
    | GoStmt                            { printf("Statement - GoStmt\n"); }
    | ReturnStmt                        { printf("Statement - ReturnStmt\n"); }
    | BreakStmt                         { printf("Statement - BreakStmt\n"); }
    | ContinueStmt                      { printf("Statement - ContinueStmt\n"); }
    | GotoStmt                          { printf("Statement - GotoStmt\n"); }
    | FallthroughStmt                   { printf("Statement - FallthroughStmt\n"); }
    | Block                             { printf("Statement - Block\n"); }
    | IfStmt                            { printf("Statement - IfStmt\n"); }
    | SwitchStmt                        { printf("Statement - SwitchStmt\n"); }
    | SelectStmt                        { printf("Statement - SelectStmt\n"); }
    | ForStmt                           { printf("Statement - ForStmt\n"); }
    | DeferStmt                         { printf("Statement - DeferStmt\n"); }
    ;
    
SelectStmt : 
    SELECT '{' CommClauses '}'       { printf("SelectStmt.\n"); }
    ;
    
CommClauses :
    | CommClauses CommClause                { printf("CommClauses.\n"); }
    ;
    
CommClause :
    CommCase ':' StatementList              { printf("CommClause.\n"); }
    ;
    
CommCase :
    CASE SendStmt                           { printf("CommCase - SendStmt.\n"); }
    | CASE RecvStmt                         { printf("CommCase - RecvStmt.\n"); }
    | DEFAULT                               { printf("CommCase - default.\n"); }
    ;
    
RecvStmt :
    RecvExpr                                                { printf("RecvStmt - RecvExpr.\n"); }
    | ExpressionList '=' RecvExpr                           { printf("RecvStmt - ExpressionList = RecvExpr.\n"); }
    | IdentifierList ":=" RecvExpr                          { printf("RecvStmt - IdentifierList := RecvExpr.\n"); }
    ;
    
RecvExpr :
    Expression                              { printf("RecvExpr.\n"); }
    ;
    
PostStmt :
    SimpleStmt                          { printf("PostStmt.\n"); }
    
SimpleStmt :
    EmptyStmt                           { printf("SimpleStmt - EmptyStmt\n"); }
    | ExpressionStmt                    { printf("SimpleStmt - ExpressionStmt\n"); }
    | SendStmt                          { printf("SimpleStmt - SendStmt\n"); }
    | IncDecStmt                        { printf("SimpleStmt - IncDecStmt\n"); }
    | Assignment                        { printf("SimpleStmt - Assignment\n"); }
    | ShortVarDecl                      { printf("SimpleStmt - ShortVarDecl\n"); }
    ;
    
TypeSwitchStmt :
    SWITCH TypeSwitchGuard '{' TypeCaseClauses                          { printf("TypeSwitchStmt.\n"); }
    | SWITCH SimpleStmt ';' TypeSwitchGuard '{' TypeCaseClauses '}'     { printf("TypeSwitchStmt.\n"); }
    ;

TypeSwitchGuard :
    PrimaryExpr '.' '(' TYPE ')'                                        { printf("TypeSwitchGuard - PrimaryExpr . ( Type ).\n"); }
    | IDENTIFIER ":=" PrimaryExpr '.' '(' TYPE ')'                      { printf("TypeSwitchGuard - Identifier := PrimaryExpr . ( Type ).\n"); }
    ;
   
TypeCaseClauses :
    | TypeCaseClauses TypeCaseClause                                                { printf("TypeCaseClauses.\n"); }
    ;
    
TypeCaseClause :
    TypeSwitchCase ':' StatementList                                                { printf("TypeCaseClause.\n"); }
    ;
   
TypeSwitchCase :
    CASE TypeList                                                                   { printf("TypeSwitchCase - case TypeList.\n"); }
    | DEFAULT                                                                       { printf("TypeSwitchCase - default.\n"); }
    ;

Declaration :
    ConstDecl                           { printf("Declaration - ConstDecl\n"); }
    | TypeDecl                          { printf("Declaration - TypeDecl\n"); }
    | VarDecl                           { printf("Declaration - VarDecl\n"); }
    ;

TopLevelDecls :
    | TopLevelDecls TopLevelDecl        { printf("TopLevelDecls.\n"); }
    ;

TopLevelDecl :
    Declaration                         { printf("TopLevelDecl - Declaration\n"); }
    | FunctionDecl                      { printf("TopLevelDecl - FunctionDecl\n"); }
    | MethodDecl                        { printf("TopLevelDecl - MethodDecl\n"); }
    ;

ConstDecl :
    CONST ConstSpec                                       { printf("ConstDecl - 1\n"); }  
    | CONST '(' ConstSpecs ')'                 { printf("ConstDecl - 2\n"); }
    ;

ConstSpecs :
    | ConstSpecs ConstSpec                              { printf("ConstSpecs.\n"); }
    ;

ConstSpec :
    IdentifierList                                      { printf("ConstSpec - IdentifierList.\n"); }
    | IdentifierList '=' ExpressionList                 { printf("ConstSpec - IdentifierList = ExpressionList.\n"); }
    | IdentifierList Type '=' ExpressionList            { printf("ConstSpec - IdentifierList Type = ExpressionList.\n"); }
    ;

TypeDecl :
    TYPE TypeSpec                                               { printf("TypeDecl - 1\n"); }
    | TYPE '(' TypeSpecs ')'                                    { printf("TypeDecl - 2\n"); }
    ;
    
TypeSpecs :
    | TypeSpecs TypeSpec                                        { printf("TypeSpecs.\n"); }
    ;
    
TypeSpec :
    AliasDecl                                                   { printf("TypeSpec - AliasDecl\n"); }
    | TypeDef                                                   { printf("TypeSpec - TypeDef\n"); }
    ;

AliasDecl :
    IDENTIFIER '=' Type                                         { printf("AliasDecl\n"); }
    ;

TypeDef :
    IDENTIFIER Type                                             { printf("TypeDef - Identifier Type.\n"); }
    | IDENTIFIER TypeParameters  Type                           { printf("TypeDef - Identifier TypeParameters Type.\n"); }
    ;

MethodDecl :
    FUNC Receiver MethodName Signature                          { printf("MethodDecl - func Receiver MethodName Signature.\n"); }
    | FUNC Receiver MethodName Signature FunctionBody           { printf("MethodDecl - func Receiver MethodName Signature FunctionBody.\n"); }
    ;

Conversion :
    Type '(' Expression ')'                                     { printf("Conversion - Type ( Expression ).\n"); }
    | Type '(' Expression ',' ')'                               { printf("Conversion - Type ( Expression , ).\n"); }
    ;

Receiver :
    Parameters                                                  { printf("Receiver\n"); }
    ;
    
MethodExpr :
    ReceiverType '.' MethodName                                 { printf("MethodExpr.\n"); }
    ;
    
ReceiverType :
    Type                                                        { printf("ReceiverType.\n"); }
    ;
    
LabeledStmt :
    Label ':' Statement                                         { printf("LabeledStmt\n"); }
    ;

Label :
    IDENTIFIER                                                  { printf("Label\n"); }
    ;

EmptyStmt :
                                                                { printf("EmptyStmt\n"); }
    ;

ExpressionStmt :
    Expression                                                  { printf("ExpressionStmt\n"); }
    ;

SendStmt :
    Channel "<-" Expression                                     { printf("SendStmt\n"); }
    ;

Channel :
    Expression                                                  { printf("Channel\n"); }
    ;

IncDecStmt :
    Expression "++"                                             { printf("IncDecStmt - Expression ++.\n"); }
    | Expression "--"                                           { printf("IncDecStmt - Expression --.\n"); }
    ;

Assignment :
    ExpressionList assign_op ExpressionList                     { printf("Assignment\n"); }
    ;

assign_op :
    add_op '='                                                  { printf("assign_op - add_op =.\n"); }
    | mul_op '='                                                { printf("assign_op - add_op =.\n"); }
    ;

ShortVarDecl :
    IdentifierList ":=" ExpressionList                          { printf("ShortVarDecl\n"); }
    ;

GoStmt :
    GO Expression                                               { printf("GoStmt\n"); }
    ;

ReturnStmt :
    RETURN                                                      { printf("ReturnStmt - return.\n"); }
    | RETURN ExpressionList                                     { printf("ReturnStmt - return ExpressionList.\n"); }
    ;

BreakStmt :
    BREAK                                                       { printf("BreadStmt - break.\n"); }
    | BREAK Label                                               { printf("BreakStmt - break Label.\n"); }    
    ;

ContinueStmt :
    CONTINUE                                                    { printf("ContinueStmt - continue.\n"); }
    | CONTINUE Label                                            { printf("ContinueStmt - continue Label.\n"); }
    ;

GotoStmt :
    GOTO Label                                                  { printf("GotoStmt\n"); }
    ;

FallthroughStmt :
    FALLTHROUGH                                                 { printf("FallthroughStmt\n"); }
    ;

IfStmt :
    IF Expression Block                                         { printf("IfStmt - if Expression Block.\n"); }
    | IF SimpleStmt Expression Block                            { printf("IfStmt - if SimpleStmt Expression Block.\n"); }
    | IF Expression Block ELSE IfStmt                           { printf("IfStmt - if Expression Block else IfStmt.\n"); }
    | IF SimpleStmt Expression Block ELSE IfStmt                { printf("IfStmt - if SimpleStmt Expression Block else IfStmt.\n"); }
    | IF Expression Block ELSE Block                            { printf("IfStmt - if Expression Block else Block.\n"); }
    | IF SimpleStmt Expression Block ELSE Block                 { printf("IfStmt - if SimpleStmt Expression Block else Block.\n"); }
    ;

SwitchStmt :
    ExprSwitchStmt                                              { printf("SwitchStmt - ExprSwitchStmt\n"); }
    | TypeSwitchStmt                                            { printf("SwitchStmt - TypeSwitchStmt\n"); }
    ;

ExprSwitchStmt :
    SWITCH '{' ExprCaseClauses '}'                              { printf("ExprSwitchStmt - switch { ExprCaseClauses }.\n"); }
    | SWITCH SimpleStmt '{' ExprCaseClauses '}'                 { printf("ExprSwitchStmt - switch SimpleStmt { ExprCaseClauses }.\n"); }
    | SWITCH Expression '{' ExprCaseClauses '}'                 { printf("ExprSwitchStmt - switch Expression { ExprCaseClauses }.\n"); }
    | SWITCH SimpleStmt Expression '{' ExprCaseClauses '}'      { printf("ExprSwitchStmt - switch SimpleStmt Expression { ExprCaseClauses }.\n"); }
    ;

ExprCaseClauses :
    | ExprCaseClauses ExprCaseClause                            { printf("ExprCaseClauses.\n"); }
    ;
    
ExprCaseClause :
    ExprSwitchCase ':' StatementList                            { printf("ExprCaseClause\n"); }
    ;

ExprSwitchCase :
    CASE ExpressionList                                         { printf("ExprSwitchCase - ExpressionList\n"); }
    | DEFAULT                                                   { printf("ExprSwitchCase - DEFAULT\n"); }
    ;
    
ForStmt :
    FOR Block                                                   { printf("ForStmt - for Block.\n"); }
    | FOR Condition Block                                       { printf("ForStmt - for Condition Block.\n"); }
    | FOR ForClause Block                                       { printf("ForStmt - for ForClause Block.\n"); }
    | FOR RangeClause Block                                     { printf("ForStmt - for RangeClause Block.\n"); }
    ;
    
DeferStmt :
    DEFER Expression                                            { printf("DeferStmt.\n"); }
    ;
    
InitStmt :
    SimpleStmt                                                  { printf("InitStmt.\n"); }
    ;
    
RangeClause :
    RANGE Expression                                            { printf("RangeClause - range Expression.\n"); }
    | ExpressionList '=' RANGE Expression                       { printf("RangeClause - ExpressionList = range Expression.\n"); }
    | IdentifierList ":=" RANGE Expression                      { printf("RangeClause - IdentifierList := range Expression.\n"); }
    ;
    
Condition :
    Expression              { printf("Condition.\n"); }
    ;

ForClause :
               ';'           ';'                            { printf("ForClause - ; ;.\n"); }
    | InitStmt ';'           ';'                            { printf("ForClause - InitStmt ; ;.\n"); }
    |          ';' Condition ';'                            { printf("ForClause - ; Condition ;.\n"); }
    |          ';'           ';' PostStmt                   { printf("ForClause - ; ; PostStmt.\n"); }
    | InitStmt ';' Condition ';'                            { printf("ForClause - InitStmt ; Condition ;.\n"); }
    |          ';' Condition ';' PostStmt                   { printf("ForClause - ; Condition ; PostStmt.\n"); }
    | InitStmt ';'           ';' PostStmt                   { printf("ForClause - InitStmt ; ; PostStmt.\n"); }
    | InitStmt ';' Condition ';' PostStmt                   { printf("ForClause - InitStmt ; Condition ; PostStmt.\n"); }
    ;
	
IdentifierList :
    IDENTIFIER                                              { printf("IdentifierList - Identifier."); }
    | IdentifierList ',' IDENTIFIER                         { printf("IdentifierList - IdentifierList Identifier.\n"); }
    ;
	
FunctionDecl :
    FUNC FunctionName Signature                                 { printf("FunctionDecl - func FunctionName Signature.\n"); }
    | FUNC FunctionName TypeParameters Signature                { printf("FunctionDecl - func FunctionName TypeParameters Signature.\n"); }
    | FUNC FunctionName Signature FunctionBody                  { printf("FunctionDecl - func FunctionName Signature FunctionBody.\n"); }
    | FUNC FunctionName TypeParameters Signature FunctionBody   { printf("FunctionDecl - func FunctionName TypeParameters Signature FunctionBody.\n"); }
    ;
    
FunctionName :
	IDENTIFIER	                                            { printf("FunctionName.\n"); }
	;

FunctionBody :
	Block		                                            { printf("FunctionBody.\n"); }
	;
	
Signature :
    Parameters
    | Parameters Result			        { printf("Signature - Parameters Result.\n"); }
	;
	
Parameters :
    '('')'                             { printf("Parameters - ( ).\n"); }
    | '(' ParameterList ')'             { printf("Parameters - ( ParametersList ).\n"); }
    | '(' ParameterList ',' ')' 	    { printf("Parameters - ( ParametersList , ).\n"); }
	;
	
Result :
	Parameters 					        { printf("Result - Parameters.\n"); }
	| Type						        { printf("Result - Type.\n"); }
	;
	
ParameterList :
    ParameterDecl
    | ParameterList ',' ParameterDecl	{ printf("ParameterList.\n"); }
	;

ParameterDecl :
    Type                                { printf("ParameterDecl - Type.\n"); }
    | IdentifierList Type               { printf("ParameterDecl - IdentifierList Type.\n"); }
    | "..." Type                        { printf("ParameterDecl - ... Type.\n"); }
    | IdentifierList "..." Type         { printf("ParameterDecl - IdentifierList ... Type.\n"); }
    ;
	
TypeParameters :
    '[' TypeParamList ']'                                   { printf("TypeParameters - [ TypeParamList ].\n"); }
	'[' TypeParamList ',' ']' 		                        { printf("TypeParameters - [ TypeParamList , ].\n"); }
	;
	
TypeParamList :
    TypeParamDecl                                           { printf("TypeParamList - TypeParamDecl.\n"); }
    | TypeParamList ',' TypeParamDecl                       { printf("TypeParamList - TypeParamList , TypeParamDecl.\n"); }
    ;
	
TypeConstraint :
	TypeElem					                            { printf("TypeConstraint.\n"); }
	;
	
TypeElem :
    TypeTerm                                                { printf("TypeElem - TypeTerm.\n"); }
    | TypeElem "|" TypeTerm                                 { printf("TypeElem - TypeElem | TypeTerm.\n"); }
    ;

TypeTerm :
	Type						                            { printf("TypeTerm - Type.\n"); }
	| UnderlyingType				                        { printf("TypeTerm - UnderlyingType.\n"); }
	;

UnderlyingType :
	"~" Type					                            { printf("UnderlyingType.\n"); }
	;

TypeParamDecl :
	IdentifierList TypeConstraint 			                { printf("TypeParamDecl.\n"); }
	;

QualifiedIdent :
	PackageName '.' IDENTIFIER	                            { printf("QualifiedIdent - PackageName . identifier.\n"); }
	;

%%

int main() {
    printf("\n\n");
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