%{ open Ast

    exception LexErr of string
    exception ParseErr of string
    

    
    exception Invalid_type of string
    let string_to_variable_type = function
	| "string" -> String
	| "int" -> Integer
	| dtype -> raise (Invalid_type dtype)
%}

%token LPAREN RPAREN LBRACKET RBRACKET LCURLY RCURLY INDENT DEDENT COLON TERMINATOR EOF COMMA
%token DEF RETURN
%token <int> DEDENT_EOF, DEDENT_COUNT
%token ASSIGNMENT 

%token <int> INTEGER_LITERAL
%token <string> DATATYPE STRING_LITERAL
%token <string> IDENTIFIER

%right ASSIGNMENT

%start program  
%type <Ast.program> program

%%

program:
    |  /* nothing */                                                { [], [] }
    | program vdecl TERMINATOR                                      { ($2 :: fst $1), snd $1 }
    | program fdecl                                                 { fst $1, ($2 :: snd $1) }

fdecl:
    | variable_type DEF identifier LPAREN parameter_list RPAREN COLON INDENT function_body DEDENT
                                                                    {{ 
                                                                        r_type = $1;
                                                                        name = $3;
                                                                        params = $5;
                                                                        body = $9;
                                                                    }}

parameter_list:
    | /* nothing */                                                 { [] }
    | nonempty_parameter_list                                       { $1 }

nonempty_parameter_list:
    | vdecl COMMA nonempty_parameter_list                           {$1 :: $3}
    | vdecl                                                         { [$1] }

vdecl:
    | variable_type identifier                                      {{ 
                                                                        v_type = $1;
                                                                        name = $2;
                                                                    }}

identifier:
    | IDENTIFIER                                                    { Identifier($1)}

function_body:
    | /* nothing */                                                 { [] }
    | statement function_body                                       { $1::$2 }
    
statement:
    | expression TERMINATOR                                         { Expression($1) }
    | vdecl TERMINATOR                                              { Declaration($1) }  
    | RETURN expression TERMINATOR                                  { Return($2) }
    | identifier ASSIGNMENT expression TERMINATOR                   { Assignment( $1, $3 ) }
    | vdecl ASSIGNMENT expression TERMINATOR                        { Initialization ($1, $3) }
    
expression_list:
    | /* nothing */                                                 { [] }   
    | nonempty_expression_list                                      { $1 }

nonempty_expression_list:
    | expression COMMA nonempty_expression_list                     { $1 :: $3 }
    | expression                                                    { [$1] }

expression:
	| identifier LPAREN expression_list RPAREN 			            { Function_Call($1,$3) }
	| STRING_LITERAL        							            { String_Literal($1) }
	| INTEGER_LITERAL									            { Integer_Literal($1) }
	| identifier                                                    { Identifier_Expression($1) }
    | LCURLY expression_list RCURLY                                 { Array_Literal($2)}

variable_type:
    | DATATYPE                                                      { string_to_variable_type $1 }
    | variable_type identifier LBRACKET INTEGER_LITERAL RBRACKET    { Array($1,$4) }
    
