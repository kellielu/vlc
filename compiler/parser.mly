%{ open Ast
    exception Invalid_type of string
    let string_to_variable_type = function
	| "string" -> String
	| "int" -> Integer
	| dtype -> raise (Invalid_type dtype)
%}

%token LPAREN RPAREN INDENT DEDENT
%token COLON COMMA TERMINATOR
%token ASSIGNMENT DEF
%token RETURN
%token <int> DEDENT_EOF
%token <int> DEDENT_COUNT
%token EOF

%token <int> INTEGER_LITERAL
%token <string> IDENTIFIER DATATYPE STRING_LITERAL

%right ASSIGNMENT

%start top_level
%type <Ast.statement list> top_level

%%

top_level:
    |  /* nothing */                                            { [] }
    | top_level_statement top_level                             { $1 :: $2 }

top_level_statement:
    | declaration TERMINATOR                                    { Declaration($1) }
    | variable_type DEF identifier LPAREN parameter_list RPAREN COLON TERMINATOR INDENT function_body DEDENT
                                                                { Function_Declaration($1, $3, $5, $10) }
    | variable_type identifier ASSIGNMENT expression TERMINATOR { Variable_Declaration_Assignment($1, $2, $4) }
                                            
    
parameter_list:
    | /* nothing */                                             { [] }
    | nonempty_parameter_list                                   { $1 }

nonempty_parameter_list:
    | declaration COMMA nonempty_parameter_list                 {$1 :: $3}
    | declaration                                               { [$1] }

function_body:
    | /* nothing */                                             { [] }
    | statement function_body                                   { $1::$2 }

declaration:
    | variable_type identifier                                  { Variable_Declaration ($1, $2) }

statement:
    | expression TERMINATOR                                     { Expression($1) }
    | declaration TERMINATOR                                    { Declaration($1) }  
    | RETURN expression TERMINATOR                              { Return($2) }
    | variable_type identifier ASSIGNMENT expression TERMINATOR { Variable_Declaration_Assignment($1, $2, $4) }

expression:
	| identifier LPAREN expression_list RPAREN 			        { Function_Call($1,$3) }
	| STRING_LITERAL        							        { String_Literal($1) }
	| INTEGER_LITERAL									        { Integer_Literal($1) }
	| identifier ASSIGNMENT expression                          { Assignment($1, $3) }

expression_list:
    | /* nothing */                                             { [] }   
    | nonempty_expression_list                                  { $1 }

nonempty_expression_list:
    | expression COMMA nonempty_expression_list                 { $1 :: $3 }
    | expression                                                { [$1] }

variable_type:
    | DATATYPE                                                  { string_to_variable_type $1 }

identifier:
    | IDENTIFIER                                                { Identifier($1) }
	