%{ open Ast;; (*open Exceptions;;*)

    
    (* Converts keywords to appropriate datatype *)
    let string_to_data_type = function
	| "string" -> String
    | "bool" -> Boolean
    | "void" -> Void
    | "ubyte" -> Unsigned_Byte
    | "byte" -> Byte
    | "uint" -> Unsigned_Integer
	| "int" -> Integer
    | "ulong" -> Unsigned_Long
    | "long" -> Long
    | "float" -> Float 
    | "double" -> Double
	| dtype -> raise (Exceptions.Invalid_data_type dtype)

%}

%token LPAREN RPAREN LBRACKET RBRACKET LCURLY RCURLY INDENT DEDENT COLON TERMINATOR EOF COMMA
%token DEF DEFG RETURN CONSTS TILDA
%token <int> DEDENT_EOF, DEDENT_COUNT

%token ADD SUBTRACT MULTIPLY DIVIDE MODULO
%token PLUS_PLUS MINUS_MINUS
%token BITSHIFT_RIGHT BITSHIFT_LEFT
%token AND OR NOT XOR
%token EQUAL NOT_EQUAL GREATER_THAN GREATER_THAN_EQUAL LESS_THAN LESS_THAN_EQUAL
%token IF ELSE WHILE FOR
%token CONTINUE BREAK 

%token ASSIGNMENT 

%token <int> INTEGER_LITERAL
%token <string> STRING_LITERAL
%token <float> FLOATING_POINT_LITERAL
%token <bool> BOOLEAN_LITERAL

%token <string> IDENTIFIER
%token <string> DATATYPE

%nonassoc ELSE NOELSE
%right ASSIGNMENT
%left IF
%left LBRACKET RBRACKET
%left EQUAL NOT_EQUAL GREATER_THAN GREATER_THAN_EQUAL LESS_THAN LESS_THAN_EQUAL
%left AND NOT OR XOR
%left BITSHIFT_RIGHT BITSHIFT_LEFT
%left ADD SUBTRACT PLUS_PLUS MINUS_MINUS
%left MULTIPLY DIVIDE MODULO
%right NEGATE 

%start program  
%type <Ast.program> program

%%

program:
    |  /* nothing */                                                    { [], [] } /* variable statements, function declarations */ 
    | program variable_statement TERMINATOR                             { List.rev ($2 :: List.rev (fst $1)), snd $1 }
    | program fdecl                                                     { fst $1, List.rev($2 :: List.rev(snd $1))  }

identifier:
    | IDENTIFIER                                                        { Identifier($1)}

/* Kernel and host function declarations */
fdecl:
    | variable_type DEF identifier LPAREN parameter_list RPAREN COLON indent_block
                                                                        {{ 
                                                                            is_kernel_function = false;
                                                                            return_type = $1;
                                                                            name = $3;
                                                                            params = $5;
                                                                            body = $8;
                                                                        }}
    | variable_type DEFG identifier LPAREN parameter_list RPAREN COLON indent_block
                                                                        {{
                                                                            is_kernel_function = true;
                                                                            return_type = $1;
                                                                            name = $3;
                                                                            params = $5;
                                                                            body = $8;
                                                                        }}

/* Constant parameters for higher order function calls */
constant:
    | identifier ASSIGNMENT expression                                  {Constant($1,$3)}
    
constant_list:
    |  /* nothing */                                                    { [] }
    | nonempty_constant_list                                            { $1 }

nonempty_constant_list:
    | constant COMMA nonempty_constant_list                             {$1 :: $3}
    | constant                                                          { [$1] }


/* Higher order function calls */
higher_order_function_call:
    | TILDA identifier LPAREN identifier COMMA CONSTS LPAREN constant_list RPAREN COMMA nonempty_array_expression_list RPAREN
                                                                        {{
                                                                            hof_type = $2;
                                                                            kernel_function_name = $4;
                                                                            constants = $8;
                                                                            input_arrays = $11;
                                                                        }}
    | TILDA identifier LPAREN identifier COMMA nonempty_array_expression_list RPAREN
                                                                        {{
                                                                            hof_type = $2;
                                                                            kernel_function_name = $4;
                                                                            constants = [];
                                                                            input_arrays = $6;
                                                                        }}



/* Parameters for normal host functions and kernel functions */
vdecl:
    | variable_type identifier                                          { Variable_Declaration($1,$2)}

nonempty_parameter_list:
    | vdecl                                                             { [$1] }
    | nonempty_parameter_list COMMA vdecl                               {$3 :: $1}

parameter_list:
    | /* nothing */                                                     { [] }
    | nonempty_parameter_list                                           { $1 }



/* Statements */ 
variable_statement:
    | vdecl TERMINATOR                                                  { Declaration($1) }
    | assignment_expression ASSIGNMENT expression TERMINATOR            { Assignment( $1, $3 ) }
    | vdecl ASSIGNMENT expression TERMINATOR                            { Initialization ($1, $3) }

for_statement:
    | assignment_expression ASSIGNMENT expression                       { Variable_Statement(Assignment($1,$3 ))}
    | vdecl ASSIGNMENT expression                                       { Variable_Statement(Initialization($1,$3))}
    
statement:
    | expression TERMINATOR                                             { Expression($1) } 
    | RETURN expression TERMINATOR                                      { Return($2) }
    | RETURN TERMINATOR                                                 { Return_Void }
    | CONTINUE TERMINATOR                                                                       { Continue }
    | BREAK TERMINATOR                                                                          { Break }
    | IF LPAREN expression RPAREN COLON indent_block %prec NOELSE                               { If($3, Block($6), Block([])) }   
    | IF LPAREN expression RPAREN COLON indent_block ELSE COLON indent_block                    { If($3, Block($6), Block($9)) }
    | FOR LPAREN for_statement COMMA expression COMMA for_statement RPAREN COLON indent_block   { For($3,$5,$7,Block($10)) }
    | WHILE LPAREN expression RPAREN COLON indent_block                                         { While($3, Block($6)) }
    | variable_statement                                                                        { Variable_Statement($1) } 

nonempty_statement_list:
    | statement                                                         { [$1] }
    | nonempty_statement_list statement                                 { List.rev($2 :: List.rev($1)) }

    /* Group of statements */
indent_block:
    | /* nothing */                                                     { [] }
    | INDENT nonempty_statement_list DEDENT                             { $2 }



/* Expressions */
expression:
    | identifier LPAREN expression_list RPAREN                          { Function_Call($1,$3) }
    | higher_order_function_call                                        { Higher_Order_Function_Call($1)}

    | LPAREN expression RPAREN                                          { $2 }

    | STRING_LITERAL                                                    { String_Literal($1) }
    | INTEGER_LITERAL                                                   { Integer_Literal($1) }
    | BOOLEAN_LITERAL                                                   { Boolean_Literal($1) }
    | FLOATING_POINT_LITERAL                                            { Floating_Point_Literal($1) }
    | array_literal                                                     { $1 }
    | identifier                                                        { Identifier_Literal($1)}

    | expression AND expression                                         { Binop($1, And, $3) }
    | expression OR  expression                                         { Binop($1, Or, $3) }
    | expression XOR expression                                         { Binop($1, Xor, $3) }
    | NOT expression                                                    { Unop($2, Not) }

    | expression EQUAL expression                                       { Binop($1, Equal, $3) }
    | expression NOT_EQUAL expression                                   { Binop($1, Not_Equal, $3 )}
    | expression GREATER_THAN expression                                { Binop($1, Greater_Than, $3 )}
    | expression GREATER_THAN_EQUAL expression                          { Binop($1, Greater_Than_Equal, $3 )}
    | expression LESS_THAN expression                                   { Binop($1, Less_Than, $3) }
    | expression LESS_THAN_EQUAL expression                             { Binop($1, Less_Than_Equal, $3)}

    | SUBTRACT expression                                               { Unop($2,Negate) }
    | expression ADD expression                                         { Binop($1, Add, $3) }
    | expression PLUS_PLUS                                              { Unop($1,Plus_Plus)}
    | expression MINUS_MINUS                                            { Unop($1, Minus_Minus)}
    | expression SUBTRACT expression                                    { Binop($1, Subtract, $3) }
    | expression MULTIPLY expression                                    { Binop($1, Multiply, $3) }
    | expression DIVIDE expression                                      { Binop($1, Divide, $3) }
    | expression MODULO expression                                      { Binop($1, Modulo, $3)}
    | expression BITSHIFT_RIGHT expression                              { Binop($1, Bitshift_Right,$3) }
    | expression BITSHIFT_LEFT expression                               { Binop($1, Bitshift_Left,$3) }
    | variable_type LPAREN expression RPAREN                            { Cast($1, $3)}

    | expression IF LPAREN expression RPAREN ELSE expression            { Ternary($1,$4,$7) }
    | array_expression nonempty_array_accessor_list                     { Array_Accessor($1,$2) }


nonempty_expression_list:
    | expression COMMA nonempty_expression_list                         { $1 :: $3 }
    | expression                                                        { [$1] }

expression_list:
    | /* nothing */                                                     { [] }   
    | nonempty_expression_list                                          { $1 }


array_accessor:
    | LBRACKET expression RBRACKET                                      { $2 }

nonempty_array_accessor_list:
    | nonempty_array_accessor_list array_accessor                       { $2 :: $1 }
    | array_accessor                                                    { [$1] } 

array_literal:
    | LCURLY nonempty_expression_list RCURLY                            { Array_Literal($2)}

array_expression:
    | identifier                                                        { Identifier_Literal($1) }
    | array_literal                                                     { $1 }


nonempty_array_expression_list:
    | array_expression                                                  { [$1] }
    | nonempty_array_expression_list COMMA array_expression             { $3 :: $1 }

 /* Expressions that can be assigned on the right side of the assignment statement */
assignment_expression:
    | identifier                                                        { Identifier_Literal($1) }
    | array_expression nonempty_array_accessor_list                     { Array_Accessor($1,$2) }


/* Variable types  and Data types */
data_type:
    | DATATYPE                                                          { string_to_data_type $1 }
variable_type:
    | data_type                                                         { Primitive($1) }
    | data_type array_dimension_list                            
        { 
            let rec create_array vtype dim_list= 
                match dim_list with
                    | [] -> raise (Exceptions.Array_parsing_error)
                    | head::[] -> Array(Primitive(vtype),head)
                    | head::tail -> Array((create_array vtype tail),head)
            in create_array $1 $2
             
        }
array_dimension:
    | LBRACKET INTEGER_LITERAL RBRACKET                                 { $2 }

array_dimension_list:
    | array_dimension                                                   { [$1] }
    | array_dimension array_dimension_list                              { $1 :: $2 }

    
