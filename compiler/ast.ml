type identifier = Identifier of string

type expression =
	| String_Literal of string
	| Integer_Literal of int
	| Function_Call of identifier * expression list
	(* | Assignment of identifier * expression *)


(*DATA TYPES*)
type variable_type = 
	| String
	| Integer
(* 	| Array of variable_type * int
	| Struct of variable_type list * expression list * int *)


(*STATEMENTS AND DECLARATIONS*)
type variable_declaration = 
	| Declaration of variable_type * identifier

type statement = 
	| Expression of expression
	| Variable_Declaration_Assignment of variable_type * identifier * expression
	| Variable_Assignment of identifier * expression
	| Variable_Declaration of variable_declaration
	| Function_Declaration of variable_type * identifier * variable_declaration list * statement list
	| Return of expression


(* 	| "void" -> Void *) 