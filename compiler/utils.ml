open Ast
open Parser
(* open Sast *)

(* let type_to_string = function
	| String -> "string"
	| Integer -> "int" *)

(*------------------------------------------------------------General Helper Functions------------------------------------------------------------*)

(* Used to access members of our sast 'program' type, which is a triple tuple *)
let triple_fst (a,_,_) = a
let triple_snd (_,a,_) = a
let triple_trd (_,_,a) = a

(*------------------------------------------------------------Parser Debugging Functions------------------------------------------------------------*)
let token_to_string = function
    TERMINATOR -> "TERMINATOR" | INDENT -> "INDENT"
  | DEDENT -> "DEDENT" | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN" | COLON -> "COLON"
  | COMMA -> "COMMA" 
  | DEF -> "DEF" | DEFG -> "DEFG"
  | ASSIGNMENT -> "ASSIGNMENT" 
  | ADD -> "ADD" | SUBTRACT -> "SUBTRACT" | MULTIPLY -> "MULTIPLY" 
  | DIVIDE -> "DIVIDE" | MODULO -> "MODULO"
  | EOF -> "EOF" 
  | IDENTIFIER(s) -> "IDENTIFIER(" ^ s ^ ")"
  | INTEGER_LITERAL(i) -> "INTEGER_LITERAL(" ^ string_of_int i ^ ")"
  | DEDENT_COUNT(i) -> "DEDENT_COUNT(" ^ string_of_int i ^ ")"
  | STRING_LITERAL(s) -> "STRINGLITERAL(" ^ s ^ ")"
  | FLOATING_POINT_LITERAL(f) -> "FLOATING_POINT_LITERAL(" ^ string_of_float f ^ ")"
  | BOOLEAN_LITERAL(b) -> "BOOLEAN_LITERAL" ^ string_of_bool b ^ ")"
  | RETURN -> "RETURN"
  | DATATYPE(a) -> "DATATYPE(" ^ a ^ ")"
  | DEDENT_EOF(i) -> "DEDENT_EOF(" ^ string_of_int i ^ ")"
  | LCURLY -> "LCURLY" | RCURLY -> "RCURLY" | LBRACKET -> "LBRACKET" | RBRACKET -> "RBRACKET"
  | CONSTS -> "CONSTS" | TILDA -> "TILDA"
  | BITSHIFT_RIGHT -> "BITSHIFT_RIGHT" | BITSHIFT_LEFT -> "BITSHIFT_LEFT"
  | AND -> "AND" | OR -> "OR" | NOT -> "NOT"
  | EQUAL -> "EQUAL" | NOT_EQUAL -> "NOT_EQUAL" 
  | GREATER_THAN -> "GREATER_THAN" | GREATER_THAN_EQUAL -> "GREATER_THAN_EQUAL"
  | LESS_THAN -> "LESS_THAN" | LESS_THAN_EQUAL -> "LESS_THAN_EQUAL"
  | IF -> "IF" | ELSE -> "ELSE" | WHILE -> "WHILE" | FOR -> "FOR"
  | CONTINUE -> "CONTINUE" | BREAK -> "BREAK"


let token_list_to_string token_list = 
 	let rec helper token_list acc_string = 
 		if(List.length (token_list)) = 0 then
 			acc_string
 		else
 			helper (List.tl token_list) ((token_to_string(List.hd token_list)) ^ "\n" ^ acc_string)
 	in 
 	helper token_list ""

(*------------------------------------------------------------Code Generation Helper Functions and Program(Ast) Printing Functions------------------------------------------------------------*)
let binary_operator_to_string = function
  | Ast.Add -> "+"
  | Ast.Subtract -> "-"
  | Ast.Multiply -> "*"
  | Ast.Divide -> "/"
  | Ast.Modulo -> "%"
  | Ast.And -> "and"
  | Ast.Or -> "or"
  | Ast.Equal -> "=="
  | Ast.Not_Equal -> "!="
  | Ast.Greater_Than -> ">"
  | Ast.Greater_Than_Equal -> ">="
  | Ast.Less_Than -> "<"
  | Ast.Less_Than_Equal -> "<="
  | Ast.Bitshift_Left -> "<<"
  | Ast.Bitshift_Right -> "<<"

let unary_operator_to_string = function
  | Ast.Not -> "not"

let idtos = function
  | Ast.Identifier(s) -> s

let data_type_to_string = function 
  | Ast.String -> "string"
  | Ast.Integer -> "int"
  | Ast.Void -> "void"
  | Ast.Boolean -> "bool"
  | Ast.Float -> "float"


let rec variable_type_to_string = function
  | Ast.Primitive(p) -> data_type_to_string p
  | Ast.Array(vtype,size) -> (variable_type_to_string vtype) ^ "[" ^ (string_of_int size) ^ "]" 

let vdecl_to_string = function 
  | Ast.Variable_Declaration(vtype,name) ->(variable_type_to_string vtype) ^ " " ^ (idtos name)

let rec expression_to_string = function
  | Ast.Function_Call(id, e_list) -> (idtos id) ^ "(" ^ (String.concat "," (List.map expression_to_string e_list)) ^ ")" 
  | Ast.Higher_Order_Function_Call(fcall) -> higher_order_function_call_to_string fcall
  | Ast.String_Literal(s) -> "\"" ^ s ^ "\""
	| Ast.Integer_Literal(i) -> string_of_int i
  | Ast.Boolean_Literal(b) -> string_of_bool b
  | Ast.Floating_Point_Literal(f)-> string_of_float f
  | Ast.Array_Literal(e_list) -> "{" ^ String.concat "," (List.map expression_to_string e_list) ^ "}"
  | Ast.Identifier_Literal(id) -> (idtos id)
  | Ast.Cast(vtype,e) -> (variable_type_to_string vtype) ^ "(" ^ expression_to_string e ^ ")"
  | Ast.Binop(e1, o, e2) -> (expression_to_string e1) ^ (binary_operator_to_string o) ^ (expression_to_string e2)
  | Ast.Unop(e, o) -> 
    (match o with 
        | Ast.Not -> (unary_operator_to_string o) ^ (expression_to_string e)
        | _ -> (expression_to_string e) ^ (unary_operator_to_string o))
  | Ast.Array_Accessor(e,e_list) -> (expression_to_string e) ^ "[" ^ (String.concat "][" (List.map expression_to_string e_list)) ^ "]"
  | Ast.Ternary(e1,e2,e3) -> (expression_to_string e1) ^ " if(" ^ (expression_to_string e2) ^ ") else " ^ (expression_to_string e3)
and constant_to_string = function
  | Ast.Constant(id,e) -> (idtos id) ^ "=" ^ (expression_to_string e)
and higher_order_function_call_to_string fcall = (idtos fcall.higher_order_function_type) ^ "(" ^ idtos(fcall.kernel_function_name) ^ "," ^ "consts(" ^ (String.concat "," (List.map constant_to_string fcall.constants)) ^ ")," ^ (String.concat "," (List.map expression_to_string fcall.input_arrays)) ^ ")"



let variable_statement_to_string = function
  | Ast.Declaration(vdecl) -> (vdecl_to_string vdecl) ^ "\n" 
  | Ast.Assignment(e1,e2) -> (expression_to_string e1) ^ "=" ^ (expression_to_string e2) ^ "\n"
  | Ast.Initialization(vdecl,e) -> (vdecl_to_string vdecl) ^ "=" ^ (expression_to_string e) ^ "\n"

let rec statement_to_string = function
  | Ast.Variable_Statement(vstmt) -> (variable_statement_to_string vstmt)
	| Ast.Expression(e) -> (expression_to_string e) ^ "\n"
  | Ast.Block(smtm_list) -> "\t" ^ String.concat "\t" (List.map statement_to_string smtm_list)
  | Ast.If(e,block1, block2) -> 
      (match block2 with
      | Block([]) -> "if(" ^ (expression_to_string e) ^ "):\n" ^ (statement_to_string block1)
      | _ -> "if(" ^ (expression_to_string e) ^ "):\n" ^ (statement_to_string block1) ^ "else:\n" ^ (statement_to_string block2))
  | Ast.While(e,block) -> 
      "while(" ^ (expression_to_string e) ^ "):\n" ^ (statement_to_string block)
  | Ast.For(smtm1, e, smtm2, block) -> "for(" ^ (statement_to_string smtm1) ^ "," ^ (expression_to_string e) ^ "," ^ (statement_to_string smtm2) ^ "):\n" ^ (statement_to_string block)
	| Ast.Return(e) -> "return " ^ (expression_to_string e) ^ "\n"
  | Ast.Return_Void -> "return" ^ "\n"
  | Ast.Continue -> "continue" ^ "\n"
  | Ast.Break -> "break" ^ "\n"


let fdecl_to_string fdecl = 
  (variable_type_to_string fdecl.return_type) ^ 
  (if fdecl.is_kernel_function = false then " def " else " defg ") ^ 
  (idtos fdecl.name) ^ "(" ^(String.concat "," (List.map vdecl_to_string fdecl.params)) ^ "):\n\t" ^ (String.concat "\t" (List.map statement_to_string fdecl.body)) ^ "\n"

(* let kernel_fdecl_to_string kernel_fdecl = (variable_type_to_string kernel_fdecl.kernel_r_type) ^ " defg " ^ (idtos kernel_fdecl.kernel_name) ^ "(" ^(String.concat "," (List.map vdecl_to_string kernel_fdecl.kernel_params)) ^ "):\n\t" ^ (String.concat "\t" (List.map statement_to_string kernel_fdecl.kernel_body)) ^ "\n"
 *)
let program_to_string program = 
	(String.concat "\n" (List.map variable_statement_to_string (fst(program)))) ^ "\n" ^
  (String.concat "\n" (List.map fdecl_to_string (snd(program))))

(* ------------------------------------------------------------Sast Helper Functions ------------------------------------------------------------*)
let sast_to_string sast = program_to_string sast


(* let s_variable_statement_to_string = function
  | Sast.Declaration(vdecl) -> (s_vdecl_to_string vdecl) ^ "\n" 
  | Sast.Assignment(id,e) -> (idtos id) ^ "=" ^ (s_expression_to_string e) ^ "\n"
  | Sast.Initialization(vdecl,e) -> (s_vdecl_to_string vdecl) ^ "=" ^ (s_expression_to_string e) ^ "\n"

let s_ptx_fdecl_to_string ptx_fdecl = 

let s_fdecl_to_string c_fdecl = 
  (s_variable_type_to_string c_fdecl.c_fdecl_return_type) ^ " def " ^ 
  (idtos c_fdecl.c_fdecl_name) ^ "(" ^(String.concat "," (List.map s_vdecl_to_string c_fdecl.c_fdecl_params)) ^ "):\n\t" ^ (String.concat "\t" (List.map s_statement_to_string c_fdecl.c_fdecl_body)) ^ "\n"

let sast_to_string sast = 
  (String.concat "\n" (List.map s_variable_statement_to_string (Utils.triple_fst(program))) ^ "\n" ^
  (String.concat "\n" (List.map s_ptx_fdecl_to_string (Utils.triple_snd(program ))) ^ "\n" ^ 
  (String.concat "\n" (List.map s_fdecl_to_string (Utils.triple_trd(program))))
 *)