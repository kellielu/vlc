(* open Ast *)
(* Contains sast type definitions for conversions during semantic analysis *)


type identifier = 
    Identifier of string

(* -----------------------------------------PTX types -----------------------------------------*)
type ptx_operator =
    | Ptx_Add | Ptx_Subtract | Ptx_Multiply | Ptx_Divide | Ptx_Modulo

type ptx_data_type = 
	| Ptx_Integer

type ptx_variable_type = 
	| Ptx_Primitive of ptx_data_type
	| Ptx_Array of ptx_variable_type * int (* 'int' refers to the length of the array *)
	| Ptx_Pointer of ptx_variable_type * int (* 'int' refers to size of memory pointed by the pointer *)

type ptx_register_decl = 
	| Register_Declaration of ptx_data_type * string * int (* type, name, number of registers *)

type ptx_register = 
	| Register of string * int (* register name,  register number *)
	| Typed_Register of ptx_data_type * string * int (* type, register name, register number *)
	| Special_Register of string (* register name *)

type ptx_vdecl = 
    | Ptx_Vdecl of ptx_variable_type * identifier

type ptx_expression =
    | Ptx_Binop of ptx_expression * ptx_operator * ptx_expression
	| Ptx_Integer_Literal of int
    | Ptx_Array_Literal of ptx_expression list 
	| Ptx_Function_Call of identifier * ptx_expression list
	| Ptx_Identifier_Expression of identifier

type ptx_statement = 
    | Ptx_Declaration of ptx_vdecl
    | Ptx_Initialization of ptx_vdecl * ptx_expression
    | Ptx_Assignment of identifier * ptx_expression
    | Ptx_Expression of ptx_expression
    | Ptx_Return of ptx_expression
    | Ptx_Return_Void

type ptx_function_type = 
	| Global 
	| Device 

(* type ptx_constant = 
{
	name : identifier;
	variable_type: ptx_variable_type;
} *)
type ptx_kernel_fdecl = {
	f_type : ptx_function_type;
	r_type : ptx_variable_type;
	name : identifier;
	params : ptx_vdecl list;
	(* List of constants that function needs to know - aka variables that aren't in scope of function when it goes through semantic analyzer 
		If this constant list doesn't match the constant list of the higher order function, throw error in semant.ml *)
(* 	consts : ptx_vdecl list; *) 
	register_decls : ptx_register_decl list;
	body : ptx_statement list;
}

(* -----------------------------------------C types -----------------------------------------*)

(*---------------------------------- Unnecessary?????????---------------------------------- *)
type c_operator =
    | Add | Subtract | Multiply | Divide | Modulo

type c_data_type = 
	| String 
	| Integer 
	| Void 

type c_variable_type = 
	| Primitive of c_data_type
	| Array of c_variable_type * int
(* 	| Struct of variable_type list * expression list * int *)

type c_vdecl = 
    Variable_Declaration of c_variable_type * identifier

(* ----------------------------------Necessary---------------------------------- *)

type c_constant = {
	host_constant_name		: identifier; 
	kernel_constant_name 	: identifier;
	variable_type 			: c_variable_type;

}

type c_higher_order_function_call = {
	f_type 								: identifier; (* Map or reduce *)
	constants 							: c_constant list;
	(* Size of input and return arrays *)
	array_length 						: int;
	(* Input array information 
		--If an array has no name (just simply passed in as something like {1,2,3}) then it is given a temporary generated name *)
	input_array_types 					: c_variable_type list; (* List of types of arrays being passed in*)
	host_input_array_names				: identifier list;
    kernel_input_array_names			: identifier list;
    (* Return array information *)
    return_array_type               	: c_variable_type;
    host_return_array_name 				: identifier;
    kernel_return_array_name 			: identifier;
    (* Name of kernel function that is called from host (would be kernel function corresponding to map/reduce) *)
    kernel_function    					: identifier;
    constants               			: identifier list;
}

(* Type for calling defg functions directly from host *)
type c_kernel_function_call = {
	kernel_function 				: identifier; (* Name of the function that is called from the host *)
	(* Input argument information*)
	host_input_arg_values 			: expression list; (* Host names for arguments. If none, the arbitrary names are given.*)
	host_input_arg_types 			: c_variable_type list;
	host_input_arg_names 			: identifier list;

	(* Return type information*)
	kernel_input_arg_names 			: identifier list;
}

type c_expression =
    | Binop of c_expression * operator * c_expression
	| String_Literal of string
	| Integer_Literal of int
    | Array_Literal of c_expression list 
	| Function_Call of identifier * c_expression list
	| Identifier_Expression of identifier
    | Higher_Order_Function_Call of c_higher_order_function_call
    | Kernel_Function_Call of c_kernel_function_call

type c_variable_statement = 
    | Declaration of c_vdecl
    | Initialization of c_vdecl * c_expression
    | Assignment of identifier * c_expression

type c_statement = 
    | Variable_Statement of c_variable_statement
    | Expression of c_expression
    | Return of c_expression
    | Return_Void

type c_fdecl = {
    r_type      : c_variable_type;
    name        : identifier;
    params      : c_vdecl list;    
    body        : c_statement list;
}

(* Overall Program *)
type program = c_variable_statement list * ptx_kernel_fdecl list * c_fdecl list