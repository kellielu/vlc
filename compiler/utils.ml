open Ast
open Parser

let type_to_string = function
	| String -> "string"
	| Integer -> "int"


let token_to_string = function
    TERMINATOR -> "TERMINATOR" | INDENT -> "INDENT"
  | DEDENT -> "DEDENT" | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN" | COLON -> "COLON"
  | COMMA -> "COMMA" 
  | DEF -> "DEF"
  | ASSIGNMENT -> "ASSIGNMENT" 
  | EOF -> "EOF" 
  | IDENTIFIER(s) -> "IDENTIFIER(" ^ s ^ ")"
  | INTEGER_LITERAL(i) -> "INTEGER_LITERAL(" ^ string_of_int i ^ ")"
  | DEDENT_COUNT(i) -> "DEDENT_COUNT(" ^ string_of_int i ^ ")"
  | STRING_LITERAL(s) -> "STRINGLITERAL(" ^ s ^ ")"
  | RETURN -> "RETURN"
  | DATATYPE(a) -> "DATATYPE(" ^ a ^ ")"
  | DEDENT_EOF(i) -> "DEDENT_EOF(" ^ string_of_int i ^ ")"

 let token_list_to_string token_list = 
 	let rec helper token_list acc_string = 
 		if(List.length (token_list)) = 0 then
 			acc_string
 		else
 			helper (List.tl token_list) ((token_to_string(List.hd token_list)) ^ "\n" ^ acc_string)
 	in 
 	helper token_list ""

let identifier_to_string = function
	| Identifier(id) -> id

let rec expression_to_string = function
	| String_Literal(s) -> s
	| Integer_Literal(i) -> string_of_int i
	| Function_Call(id, e_list) -> (identifier_to_string id) ^ "(" ^ (String.concat "," (List.map expression_to_string e_list)) ^ ")" ^ "\n"

let variable_type_to_string = function
	| String -> "string"
	| Integer -> "int"

let variable_declaration_to_string = function
	| Declaration(var_type,id)-> (variable_type_to_string var_type) ^ (identifier_to_string id)

let rec statement_to_string = function
	| Expression(e) -> (expression_to_string e) ^ "\n"
	| Variable_Declaration_Assignment(vtype,id,e) -> (variable_type_to_string vtype) ^ (identifier_to_string id) ^ (expression_to_string e) ^ "\n"
	| Variable_Declaration(vdecl) -> (variable_declaration_to_string vdecl) ^ "\n"
	| Function_Declaration(vtype,id,vdecl_list,smtm_list) -> (variable_type_to_string vtype) ^ "def" ^ (identifier_to_string id) ^ "(" ^(String.concat "," (List.map variable_declaration_to_string vdecl_list)) ^ "):\n" ^ (String.concat "\n" (List.map statement_to_string smtm_list)) ^ "\n"
	| Return(e) -> (expression_to_string e) ^ "\n"

let program_to_string statements = 
	String.concat "\n" (List.map statement_to_string statements) ^ "\n"
