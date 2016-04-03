
type identifier = Identifier of string

type expression =
	| String_Literal of string
	| Integer_Literal of int
	| Function_Call of identifier * expression list
	| Assignment of identifier * expression


(*DATA TYPES*)
type variable_type = 
	| String
	| Integer
(* 	| Array of variable_type * int
	| Struct of variable_type list * expression list * int *)


(*STATEMENTS AND DECLARATIONS*)
type declaration =
    | Variable_Declaration of variable_type * identifier

type statement = 
    | Declaration of declaration
	| Expression of expression
	| Variable_Declaration_Assignment of variable_type * identifier * expression
	| Function_Declaration of variable_type * identifier * declaration list * statement list
	| Return of expression


(* 	| "void" -> Void *) 