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


/* Pointer Types */	
PointerType :
    "*" BaseType                { printf("PointerType.\n"); }
    ;

BaseType :
    Type                        { printf("BaseType.\n"); }
    ;
/**/


/* Interface Types */	
InterfaceType :
    INTERFACE "{" '{' InterfaceElem ";" '}' "}"         { printf("InterfaceType.\n"); };

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
    TypeTerm '{' "|" TypeTerm '}'       { printf("TypeElem\n"); }
    ;

TypeTerm :
    Type                                { printf("TypeTerm - Type\n"); }
    | UnderlyingType                    { printf("TypeTerm - UnderlyingType\n"); }
    ;

UnderlyingType :
    "~" Type                            { printf("UnderlyingType\n"); }
/**/


/* Slice Types */
SliceType :
    "[" "]" ElementType                 { printf("SliceType\n"); }
    ;
/**/


/* Map Types */
MapType :
    "map" "[" KeyType "]" ElementType   { printf("MapType\n"); }
    ;

KeyType :
    Type                                { printf("KeyType\n"); }
    ;
/**/


/* Channel Types */
ChannelType :
    '(' CHAN                          { printf("ChannelType - chan\n"); }
    | CHAN "<-"                       { printf("ChannelType - chan <-\n"); }
    | "<-" CHAN ')' ElementType       { printf("ChannelType - chan ElementType\n"); }
    ;
/**/

/* int_lit Types */
int_lit :
    decimal_lit                         { printf("int_lit - decimal_lit\n"); }
    | binary_lit                        { printf("int_lit - binary_lit\n"); }           
    | octal_lit                         { printf("int_lit - octal_lit\n"); }
    | hex_lit                           { printf("int_lit - hex_lit\n"); }
    ;

decimal_lit :
    "0"                                                         { printf("decimal_lit - 0\n"); }
    | '(' "1" … "9" ')' '[' '[' "_" ']' decimal_digits ']'      { printf("decimal_lit - 1...9\n"); }
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
    decimal_digit '{' '[' "_" ']' decimal_digit '}'         { printf("decimal_digits\n"); }
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
    "0" … "9"                           { printf("decimal_digit\n"); }
    ;

binary_digit :
    "0"                                 { printf("binary_digit - 0\n"); }
    | "1"                               { printf("binary_digit - 1\n"); }
    ;

octal_digit :
    "0" … "7"                           { printf("octal_digit\n"); }
    ;

hex_digit :
    "0" … "9"                           { printf("hex_digit - 0 ... 9 \n"); }
    | "A" … "F"                         { printf("hex_digit - A ... F \n"); }
    | "a" … "f"                         { printf("hex_digit - a ... f \n"); }
    ;
/**/


/* int_lit Types */
float_lit :
    decimal_float_lit                   { printf("float_lit - decimal_float_lit\n"); }
    | hex_float_lit                     { printf("float_lit - decimal_float_lit\n"); }
    ;

decimal_float_lit :
    decimal_digits "." '[' decimal_digits ']' '[' decimal_exponent ']'      { printf("decimal_float_lit - 1\n"); } 
    | decimal_digits decimal_exponent                                       { printf("decimal_float_lit - 2\n"); }
    | "." decimal_digits [ decimal_exponent ']'                             { printf("decimal_float_lit - 3\n"); }
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
    '[' "_" ']' hex_digits "." '[' hex_digits ']'   { printf("hex_mantissa - 1\n"); }
    | '[' "_" ']' hex_digits                        { printf("hex_mantissa - 2\n"); }
    | "." hex_digits                                { printf("hex_mantissa - 3\n"); }
    ;

hex_exponent :
    '(' "p"                                         { printf("hex_exponent - 1\n"); }
    | "P" ')' '[' "+" | "-" ']' decimal_digits      { printf("hex_exponent - 2\n"); }
    ;
/**/


/* imaginary_lit Type*/
imaginary_lit :
    '('decimal_digits           { printf("imaginary_lit - decimal_digits\n"); }
    | int_lit                   { printf("imaginary_lit - int_lit\n"); }
    | float_lit) "i"            { printf("imaginary_lit - float_lit\n"); }
    ;
/**/


/* rune_lit Types*/
rune_lit :
    "'" '(' unicode_value       { printf("rune_lit - unicode_value\n"); }
    | byte_value ')' "'"        { printf("rune_lit - byte_value\n"); }
    ;

unicode_value :
    unicode_char                { printf("unicode_value - unicode_char\n"); }
    | little_u_value            { printf("unicode_value - little_u_value\n"); }
    | big_u_value               { printf("unicode_value - big_u_value\n"); }
    | escaped_char              { printf("unicode_value - escaped_char\n"); }
    ;
    
byte_value :
    octal_byte_value        { printf("byte_value - octal_byte_value\n"); }
    | hex_byte_value        { printf("byte_value - hex_byte_value\n"); }
    ;

octal_byte_value :
    `\` octal_digit octal_digit octal_digit             { printf("octal_byte_value\n"); }
    ;

hex_byte_value :
    `\` "x" hex_digit hex_digit                         { printf("hex_byte_value\n"); }
    ;

little_u_value :
    `\` "u" hex_digit hex_digit hex_digit hex_digit     { printf("little_u_value\n"); }
    ;

big_u_value :
    `\` "U" hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit         { printf("big_u_value\n"); }
    ;

escaped_char :
    `\` '(' "a"             { printf("escaped_char - a\n"); }
    | "b"                   { printf("escaped_char - b\n"); }
    | "f"                   { printf("escaped_char - f\n"); }
    | "n"                   { printf("escaped_char - n\n"); }
    | "r"                   { printf("escaped_char - r\n"); }
    | "t"                   { printf("escaped_char - t\n"); }
    | "v"                   { printf("escaped_char - v\n"); }
    | `\`                   { printf("escaped_char - slash\n"); }
    | "'"                   { printf("escaped_char - '\n"); }
    | `"` ')'               { printf("escaped_char - \"\n"); }
    ;
/**/


/* string_lit Types*/
string_lit :
    raw_string_lit                      { printf("string_lit - raw_string_lit\n"); }
    | interpreted_string_lit            { printf("string_lit - interpreted_string_lit\n"); }
    ;

raw_string_lit :
    "`" '{' unicode_char                { printf("raw_string_lit - unicode_char\n"); }
    | newline '}' "`"                   { printf("raw_string_lit - newline\n"); }
    ;

interpreted_string_lit :
    `"` '{' unicode_value                 { printf("interpreted_string_lit - unicode_value\n"); }
    | byte_value '}' `"`                  { printf("interpreted_string_lit - byte_value\n"); }
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

/*Statement Types*/
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

SimpleStmt :
    EmptyStmt                           { printf("SimpleStmt - EmptyStmt\n"); }
    | ExpressionStmt                    { printf("SimpleStmt - ExpressionStmt\n"); }
    | SendStmt                          { printf("SimpleStmt - SendStmt\n"); }
    | IncDecStmt                        { printf("SimpleStmt - IncDecStmt\n"); }
    | Assignment                        { printf("SimpleStmt - Assignment\n"); }
    | ShortVarDecl                      { printf("SimpleStmt - ShortVarDecl\n"); }
    ;

Declaration :
    ConstDecl                           { printf("Declaration - ConstDecl\n"); }
    | TypeDecl                          { printf("Declaration - TypeDecl\n"); }
    | VarDecl                           { printf("Declaration - VarDecl\n"); }
    ;

TopLevelDecl :
    Declaration                         { printf("TopLevelDecl - Declaration\n"); }
    | FunctionDecl                      { printf("TopLevelDecl - FunctionDecl\n"); }
    | MethodDecl                        { printf("TopLevelDecl - MethodDecl\n"); }
    ;

ConstDecl :
    "const" '(' ConstSpec                                       { printf("ConstDecl - 1\n"); }  
    | "(" '{' ConstSpec ";" '}' ")" ')'                         { printf("ConstDecl - 2\n"); }
    ;

ConstSpec :
    IdentifierList '[' '[' Type ']' "=" ExpressionList ']'      { printf("ConstSpec\n"); }
    ;

TypeDecl :
    Type ( TypeSpec                                             { printf("TypeDecl - 1\n"); }
    | "(" '{' TypeSpec ";" '}' ")" ')'                          { printf("TypeDecl - 2\n"); }
    ;

TypeSpec :
    AliasDecl                                                   { printf("TypeSpec - AliasDecl\n"); }
    | TypeDef                                                   { printf("TypeSpec - TypeDef\n"); }
    ;

AliasDecl :
    IDENTIFIER "=" Type                                         { printf("AliasDecl\n"); }
    ;

TypeDef :
    IDENTIFIER '[' TypeParameters ']' Type                      { printf("TypeDef\n"); }
    ;

MethodDecl :
    FUNC Receiver MethodName Signature '[' FunctionBody ']'     { printf("MethodDecl\n"); }
    ;

Receiver :
    Parameters                                                  { printf("Receiver\n"); }
    ;

LabeledStmt :
    Label ":" Statement                                         { printf("LabeledStmt\n"); }
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
    Expression '(' "++"                                         { printf("IncDecStmt - 1\n"); }
    | "--" ')'                                                  { printf("IncDecStmt - 2\n"); }
    ;

Assignment :
    ExpressionList assign_op ExpressionList                     { printf("Assignment\n"); }
    ;

assign_op :
    '[' add_op                                                  { printf("assign_op - 1\n"); }
    | mul_op ']' "="                                            { printf("assign_op - 2\n"); }
    ;

ShortVarDecl :
    IdentifierList ":=" ExpressionList                          { printf("ShortVarDecl\n"); }
    ;

GoStmt :
    GO Expression                                               { printf("GoStmt\n"); }
    ;

ReturnStmt :
    RETURN '[' ExpressionList ']'                               { printf("ReturnStmt\n"); }
    ;

BreakStmt :
    BREAK '[' Label ']'                                         { printf("BreakStmt\n"); }    
    ;

ContinueStmt : 
    CONTINUE '[' Label ']'                                      { printf("ContinueStmt\n"); }
    ;

GotoStmt :
    GOTO Label                                                  { printf("GotoStmt\n"); }
    ;

FallthroughStmt :
    FALLTHROUGH                                                 { printf("FallthroughStmt\n"); }
    ;

Block :
    "{" StatementList "}"                                       { printf("Block\n"); }
    ;

IfStmt :
    "if" '[' SimpleStmt ";" ']' Expression Block '[' "else" '(' IfStmt      { printf("IfStmt - if\n"); }
    | Block ')' ']'                                                         { printf("IfStmt - Block\n"); }
    ;

SwitchStmt :
    ExprSwitchStmt                                              { printf("SwitchStmt - ExprSwitchStmt\n"); }
    | TypeSwitchStmt                                            { printf("SwitchStmt - TypeSwitchStmt\n"); }
    ;

ExprSwitchStmt :
    "switch" '[' SimpleStmt ";" ']' '[' Expression ']' "{" '{' ExprCaseClause '}' "}"   { printf("ExprSwitchStmt\n"); }
    ;

ExprCaseClause :
    ExprSwitchCase ":" StatementList                            { printf("ExprCaseClause\n"); }
    ;

ExprSwitchCase :
    CASE ExpressionList                                         { printf("ExprSwitchCase - ExpressionList\n"); }
    | DEFAULT                                                   { printf("ExprSwitchCase - DEFAULT\n"); }
    ;
/**/
	
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