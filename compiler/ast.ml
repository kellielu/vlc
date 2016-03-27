type program = variable_statement list * function_declaration list

type identifier = Identifier of string

type expression =
	| String_Literal of string
	| Integer_Literal of int
	| Function_Call of identifier * expression list
	(* | Assignment of identifier * expression *)




(*DATA TYPES*)
type datatype = 
	| String
	| Integer
(* 	| Void
 *)
type variable_type = 
	| datatype
(* 	| Array of datatype * int
	| Struct of datatype list * expression list * int *)






(*STATEMENTS AND DECLARATIONS*)
type variable_declaration = 
	| Declaration of variable_type * identifier

type variable_statement = 
	(* | Declaration of variable_type * identifier
	| Assignment of identifier * expression *)
	| Declaration_Assignment of variable_type * identifier * expression

type statement = 
	| Expression of expression
	(* | Variable_Statement of variable_statement *)
	| Return of expression





(*FUNCTIONS*)
type function_declaration = {
    return_type     	: datatype;
    function_name       : identifier;
    formal_arguments    : variable_declaration list;    
    body       			: statement list;
}

let string_to_variable_type = function
	| "string" -> String
	| "int" -> Integer
(* 	| "void" -> Void *)