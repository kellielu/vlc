%{ open Ast;; open Exceptions;;

    
    (* Converts keywords to appropriate datatype*)
    let string_to_data_type = function
	| "string" -> String
	| "int" -> Integer
    | "void" -> Void
	| dtype -> raise (Exceptions.Invalid_data_type dtype)

    (* Functions for accessing a triple tuple - used for program *)
    let triple_fst (a,_,_) = a
    let triple_snd (_,a,_) = a
    let triple_trd (_,_,a) = a

%}

%token LPAREN RPAREN LBRACKET RBRACKET LCURLY RCURLY INDENT DEDENT COLON TERMINATOR EOF COMMA
%token DEF DEFG RETURN
%token <int> DEDENT_EOF, DEDENT_COUNT

%token ADD SUBTRACT MULTIPLY DIVIDE MODULO

%token CONSTS

%token ASSIGNMENT 

%token <int> INTEGER_LITERAL
%token <string> DATATYPE STRING_LITERAL
%token <string> IDENTIFIER

%right ASSIGNMENT
%left ADD SUBTRACT
%left MULTIPLY DIVIDE MODULO

%start program  
%type <Ast.program> program

%%

program:
    |  /* nothing */                                             { [], [] } /* variable statements, function declarations */ 
    | program variable_statement TERMINATOR                      { List.rev ($2 :: List.rev (fst $1)), snd $1 }
    | program fdecl                                              { fst $1, List.rev($2 :: List.rev(snd $1))  }

identifier:
    | IDENTIFIER                                    { Identifier($1)}

/* Kernel and host function declarations */
fdecl:
    | variable_type DEF identifier LPAREN parameter_list RPAREN COLON INDENT function_body DEDENT
                                                    {{ 
                                                        is_kernel_function = false;
                                                        return_type = $1;
                                                        name = $3;
                                                        params = $5;
                                                        body = $9;
                                                    }}
    | variable_type DEFG identifier LPAREN parameter_list RPAREN COLON INDENT function_body DEDENT
                                                    {{
                                                        is_kernel_function = true;
                                                        return_type = $1;
                                                        name = $3;
                                                        params = $5;
                                                        body = $9;
                                                    }}

/* Constant parameters for higher order function calls */
constant:
    | identifier ASSIGNMENT expression                  {Constant($1,$3)}
    
constant_list:
    |  /* nothing */                                    { [] }
    | nonempty_constant_list                            { $1 }

nonempty_constant_list:
    | constant COMMA nonempty_constant_list             {$1 :: $3}
    | constant                                          { [$1] }


/* Higher order function calls */
higher_order_function_call:
    | identifier LPAREN identifier COMMA CONSTS LPAREN constant_list RPAREN COMMA nonempty_expression_list RPAREN
        {{
            higher_order_function_type = $1;
            kernel_function_name = $3;
            constants = $7;
            input_arrays = $10;
        }}
    | identifier LPAREN identifier COMMA nonempty_expression_list RPAREN
        {{
            higher_order_function_type = $1;
            kernel_function_name = $3;
            constants = [];
            input_arrays = $5;
        }}


/* Parameters for normal host functions and kernel functions */
parameter_list:
    | /* nothing */                                 { [] }
    | nonempty_parameter_list                       { $1 }

nonempty_parameter_list:
    | vdecl COMMA nonempty_parameter_list           {$1 :: $3}
    | vdecl                                         { [$1] }

vdecl:
    | variable_type identifier                      { Variable_Declaration($1,$2)}

/* Statements, expressions, and variable types*/ 
variable_statement:
    | vdecl TERMINATOR                              { Declaration($1) }
    | identifier ASSIGNMENT expression TERMINATOR   { Assignment( $1, $3 ) }
    | vdecl ASSIGNMENT expression TERMINATOR        { Initialization ($1, $3) }

function_body:
    | /* nothing */                                 { [] }
    | statement function_body                       { $1::$2 }
    
statement:
    | expression TERMINATOR                         { Expression($1) } 
    | RETURN expression TERMINATOR                  { Return($2) }
    | RETURN TERMINATOR                             { Return_Void }
    | variable_statement                            { Variable_Statement($1) } 

expression_list:
    | /* nothing */                                 { [] }   
    | nonempty_expression_list                      { $1 }

nonempty_expression_list:
    | expression COMMA nonempty_expression_list     { $1 :: $3 }
    | expression                                    { [$1] }

expression:
    | identifier LPAREN expression_list RPAREN      { Function_Call($1,$3) }
    | STRING_LITERAL                                { String_Literal($1) }
    | INTEGER_LITERAL                               { Integer_Literal($1) }
    | identifier                                    { Identifier_Expression($1) }
    | LCURLY expression_list RCURLY                 { Array_Literal($2)}
    | expression ADD expression                     { Binop($1, Add, $3) }
    | expression SUBTRACT expression                { Binop($1, Subtract, $3) }
    | expression MULTIPLY expression                { Binop($1, Multiply, $3) }
    | expression DIVIDE expression                  { Binop($1, Divide, $3) }
    | expression MODULO expression                  { Binop($1, Modulo, $3)}
    | higher_order_function_call                    { Higher_Order_Function_Call($1)}

data_type:
    | DATATYPE                                      { string_to_data_type $1 }
variable_type:
    | data_type                                     { Primitive($1) }
    | variable_type array_dimension_list                            
        { 
            let rec create_array vtype dim_list= 
                match dim_list with
                    | [] -> raise (Exceptions.Array_parsing_error)
                    | head::[] -> Array(vtype,head)
                    | head::tail -> Array((create_array vtype tail),head)
            in create_array $1 $2
             
        }
array_dimension:
    | LBRACKET INTEGER_LITERAL RBRACKET                                 { $2 }

array_dimension_list:
    | array_dimension                                                   {[$1]}
    | array_dimension array_dimension_list                              { $1 :: $2 }

    
