open Ast
(* Contains sast type definitions for conversions during semantic analysis *)
(* -----------------------------------------PTX types -----------------------------------------*)
type ptx_operator =
    | Ptx_Add | Ptx_Subtract | Ptx_Multiply | Ptx_Divide | Ptx_Modulo | Ptx_Sqrt

type ptx_data_type = 
	| Ptx_Integer

type ptx_variable_type = 
	| Ptx_Primitive of ptx_data_type
	| Ptx_Array of ptx_variable_type * int 					(* 'int' refers to the length of the array *)
	| Ptx_Pointer of ptx_variable_type * int 				(* 'int' refers to size of memory pointed by the pointer *)

type ptx_register_decl = 
	| Register_Declaration of ptx_data_type * string * int 	(* type, name, number of registers *)

type ptx_register = 
	| Register of string * int 								(* register name,  register number *)
	| Typed_Register of ptx_data_type * string * int 		(* type, register name, register number *)
	| Special_Register of string 							(* register name *)

type ptx_vdecl = 
    | Ptx_Vdecl of ptx_variable_type * Ast.identifier

type ptx_expression =
    | Ptx_Binop of ptx_expression * ptx_operator * ptx_expression
	| Ptx_Integer_Literal of int
    | Ptx_Array_Literal of ptx_expression list 
	| Ptx_Function_Call of Ast.identifier * ptx_expression list
	| Ptx_Identifier_Expression of Ast.identifier

type ptx_statement = 
    | Ptx_Declaration of ptx_vdecl
    | Ptx_Initialization of ptx_vdecl * ptx_expression
    | Ptx_Assignment of Ast.identifier * ptx_expression
    | Ptx_Expression of ptx_expression
    | Ptx_Return of ptx_expression
    | Ptx_Return_Void

type ptx_function_type = 
	| Global 
	| Device 

type ptx_constant = 
{
	ptx_constant_name 							: Ast.identifier;
	ptx_constant_variable_type					: ptx_variable_type;
}

type ptx_fdecl = {
	(* Global or Device *)
	ptx_fdecl_type 								: ptx_function_type;
	(* Return type *)
	ptx_fdecl_return_type 						: ptx_variable_type;
	(* Name of the function *)
	ptx_fdecl_name 								: Ast.identifier;
	(* Expected parameters of the function *)
	ptx_fdecl_params 							: ptx_vdecl list;
	(* List of constants that function needs to know - aka variables that aren't in scope of function when it goes through semantic analyzer 
		If this constant list doesn't match the constant list of the higher order function, throw error in semant.ml *)
	ptx_consts 									: ptx_constant list; 
	(* Declares the virtual registers that are needed for the function *)
	register_decls 								: ptx_register_decl list;
	(* Statements within the function *)
	ptx_fdecl_body 								: ptx_statement list;
	(* Functions that are called within this function*)
	ptx_dependent_functions 					: identifier list;
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
    Variable_Declaration of c_variable_type * Ast.identifier

(* ----------------------------------Necessary---------------------------------- *)

type c_kernel_variable_info = {
	variable_type 			: c_variable_type;
	host_name 				: Ast.identifier;
	kernel_name 			: Ast.identifier;
}

type c_higher_order_function_call = {
	(* Map or reduce *)
	higher_order_function_type 				: Ast.identifier; 
	(* Name of kernel function that is called from host (would be kernel function corresponding to map/reduce) *)
    applied_kernel_function    				: Ast.identifier;
	(* List of constants passed into map and reduce *)
	constants 								: c_kernel_variable_info list;
	(* Size of input and return arrays *)
	array_length 							: int;
	(* Input array information 
		--If an array has no name (just simply passed in as something like {1,2,3}) then it is given a temporary generated name *)
	input_arrays_info						: c_kernel_variable_info list; (* type, host name, kernel name *)
    (* Return array information *)	
    return_array_info              			: c_kernel_variable_info; (* type, host name, kernel name*)    
}

(* Type for calling defg functions directly from host *)
type c_kernel_function_call = {
	(* Name of the function that is called from the host *)
	kernel_function 						: Ast.identifier; 
	(* Input array information 
		--If an array has no name (just simply passed in as something like {1,2,3}) then it is given a temporary generated name *)
	input_args_info							: c_kernel_variable_info list; (* type, host name, kernel name *)
    (* Return array information *)
    return_arg_info              			: c_kernel_variable_info; (* type, host name, kernel name*)
}

type c_expression =
    | Binop of c_expression * c_operator * c_expression
	| String_Literal of string
	| Integer_Literal of int
    | Array_Literal of c_expression list 
	| Function_Call of Ast.identifier * c_expression list
	| Identifier_Expression of Ast.identifier
    | Higher_Order_Function_Call of c_higher_order_function_call
    | Kernel_Function_Call of c_kernel_function_call

type c_variable_statement = 
    | Declaration of c_vdecl
    | Initialization of c_vdecl * c_expression
    | Assignment of Ast.identifier * c_expression

type c_statement = 
    | Variable_Statement of c_variable_statement
    | Expression of c_expression
    | Return of c_expression
    | Return_Void

type c_fdecl = {
    c_fdecl_return_type     : c_variable_type;
    c_fdecl_name        	: Ast.identifier;
    c_fdecl_params      	: c_vdecl list;    
    c_fdecl_body        	: c_statement list;
}

(* Overall Program *)
type program = c_variable_statement list * ptx_fdecl list * c_fdecl list