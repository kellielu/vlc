%{ open Ast
    exception Invalid_type of string
    let string_to_variable_type = function
	| "string" -> String
	| "int" -> Integer
	| dtype -> raise (Invalid_type dtype)
%}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN COLON COMMA
%token ASSIGNMENT
%token RETURN
%token PRINT
%token <string> DATATYPE

%token <string> STRING_LITERAL
%token <int> INTEGER_LITERAL

%token <string> IDENTIFIER

%token EOF

%right ASSIGNMENT

%start top_level
%type <Ast.statement list> top_level

%%

top_level:
    | top_level_statement top_level                         { $1 :: $2 }
    | top_level_statement                                   { [$1] }

top_level_statement:
    | variable_declaration TERMINATOR                       { Variable_Declaration($1) }
    | variable_type identifier LPAREN parameter_list RPAREN COLON TERMINATOR INDENT function_body DEDENT
                                                            { Function_Declaration($1, $2, $4, $9) }
parameter_list:
    | { [] }

function_body:
    | { [] }
    | statement function_body    { $1::$2 }

variable_declaration:
    | variable_type identifier                              { Declaration($1, $2) } 

statement:
    | expression TERMINATOR                                          { Expression($1) }
    | variable_type identifier ASSIGNMENT expression TERMINATOR        { Declaration_Assignment($1, $2, $4) }  
    | variable_declaration TERMINATOR                                 { Variable_Declaration($1) } 
    | identifier ASSIGNMENT expression TERMINATOR                      { Assignment($1, $3) }
    | RETURN expression  TERMINATOR                                  { Return($2) }

expression:
	| identifier LPAREN expression_list RPAREN 			    { Function_Call($1,$3) }
	| STRING_LITERAL        							    { String_Literal($1) }
	| INTEGER_LITERAL									    { Integer_Literal($1) }

identifier:
	| IDENTIFIER                                            { Identifier($1) }

expression_list:
    |  { [] }
    | nonempty_expression_list                              { $1 }

nonempty_expression_list:
    | expression COMMA expression_list                      { $1 :: $3 }
    | expression                                            { [$1] }

variable_type:
    | DATATYPE                                            { string_to_variable_type $1 }

	