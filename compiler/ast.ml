(* Function Declaration *)
type variable_type = 
	| String
	| Integer
(* 	| Array of variable_type * int
	| Struct of variable_type list * expression list * int *)

type vdecl = {
    v_type   : variable_type;   
    name     : string;
}

type expression =
	| String_Literal of string
	| Integer_Literal of int
	| Function_Call of string * expression list
	| Identifier of string

type statement = 
    | Declaration of vdecl
    | Expression of expression
    | Assignment of string * expression
    | Return of expression
    | Initialization of vdecl * expression
	
type fdecl = {
    r_type      : variable_type;
    name        : string;
    params      : vdecl list;    
    body        : statement list;
}

type program = vdecl list * fdecl list
