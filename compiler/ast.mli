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
(* 	| Array of datatype * int
	| Struct of datatype list * expression list * int *)


(*STATEMENTS AND DECLARATIONS*)
type variable_declaration = 
	| Declaration of variable_type * identifier

type statement = 
	| Expression of expression
	| Declaration_Assignment of variable_type * identifier * expression
	| Assignment of identifier * expression
	| Variable_Declaration of variable_declaration
	| Function_Declaration of variable_type * identifier * variable_declaration list * statement list
	| Return of expression


(* 	| "void" -> Void *) 