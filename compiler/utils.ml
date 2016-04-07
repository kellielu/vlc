open Ast
open Parser

(* let type_to_string = function
	| String -> "string"
	| Integer -> "int" *)


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
  | LCURLY -> "LCURLY" | RCURLY -> "RCURLY" | LBRACKET -> "LBRACKET" | RBRACKET -> "RBRACKET"

 let token_list_to_string token_list = 
 	let rec helper token_list acc_string = 
 		if(List.length (token_list)) = 0 then
 			acc_string
 		else
 			helper (List.tl token_list) ((token_to_string(List.hd token_list)) ^ "\n" ^ acc_string)
 	in 
 	helper token_list ""


let idtos = function
  | Identifier(s) -> s

let rec expression_to_string = function
	| String_Literal(s) -> "\"" ^ s ^ "\""
	| Integer_Literal(i) -> string_of_int i
  | Array_Literal(e_list) -> "{" ^ String.concat "," (List.map expression_to_string e_list) ^ "}"
	| Function_Call(id, e_list) -> (idtos id) ^ "(" ^ (String.concat "," (List.map expression_to_string e_list)) ^ ")" 
	| Identifier_Expression(id) -> (idtos id)

let rec variable_type_to_string = function
	| String -> "string"
	| Integer -> "int"
  | Array(vtype,size) -> (variable_type_to_string vtype) ^ "[" ^ (string_of_int size) ^ "]" 

let vdecl_to_string vdecl = (variable_type_to_string vdecl.v_type) ^ " " ^ (idtos vdecl.name)

let statement_to_string = function
	| Expression(e) -> (expression_to_string e) ^ "\n"
 	| Declaration(vdecl) -> (vdecl_to_string vdecl) ^ "\n" 
	| Return(e) -> "return " ^ (expression_to_string e) ^ "\n"
	| Assignment(id,e) -> (idtos id) ^ "=" ^ (expression_to_string e) ^ "\n"
	| Initialization(vdecl,e) -> (vdecl_to_string vdecl) ^ "=" ^ (expression_to_string e) ^ "\n"

let fdecl_to_string fdecl = (variable_type_to_string fdecl.r_type) ^ " def " ^ (idtos fdecl.name) ^ "(" ^(String.concat "," (List.map vdecl_to_string fdecl.params)) ^ "):\n" ^ (String.concat "" (List.map statement_to_string fdecl.body)) ^ "\n"

let program_to_string program = 
	(String.concat "\n" (List.map vdecl_to_string (fst(program)))) ^ "\n" ^(String.concat "\n" (List.map fdecl_to_string (snd(program))))
