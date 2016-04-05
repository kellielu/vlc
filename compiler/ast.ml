(* Function Declaration *)

type identifier = 
    Identifier of string

type variable_type = 
	| String
	| Integer
	| Array of variable_type * int
(* 	| Struct of variable_type list * expression list * int *)

type vdecl = {
    v_type   : variable_type;   
    name     : identifier;
}

type expression =
	| String_Literal of string
	| Integer_Literal of int
    | Array_Literal of expression list 
	| Function_Call of identifier * expression list
	| Identifier_Expression of identifier

type statement = 
    | Declaration of vdecl
    | Expression of expression
    | Assignment of identifier * expression
    | Return of expression
    | Initialization of vdecl * expression
	
type fdecl = {
    r_type      : variable_type;
    name        : identifier;
    params      : vdecl list;    
    body        : statement list;
}

type program = vdecl list * fdecl list
