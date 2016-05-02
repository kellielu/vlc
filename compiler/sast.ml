open Ast
(* Contains sast type definitions for conversions during semantic analysis *)
(* -----------------------------------------PTX types -----------------------------------------*)
type ptx_data_movement = 
	| Ptx_Move | Ptx_Load | Ptx_Store

type ptx_binary_operator =
    | Ptx_Add | Ptx_Subtract | Ptx_Multiply | Ptx_Divide | Ptx_Modulo

type ptx_data_type =
	| U16 | U32 | U64 | S16 | S32 | S64

(* should use this as our information about global/param etc.*)
type ptx_variable_type = 
	| Ptx_Primitive of ptx_data_type
	| Ptx_Array of ptx_variable_type * int 					(* 'int' refers to the length of the array *)
	| Ptx_Pointer of ptx_variable_type * int 				(* 'int' refers to size of memory pointed by the pointer *)

type ptx_register_decl = 
	| Register_Declaration of ptx_data_type * string * int 	(* type, name, number of registers *)

type ptx_register = 
	| Register of string * int 								(* register name,  register number *)
(* Not sure what this is	| Typed_Register of ptx_data_type * string * int 		(* type, register name, register number *) *)
(* Implement later	| Special_Register of string 							(* register name *) *)

type ptx_parameter = 
	| Parameter_register of ptx_register
	| Parameter_constant of int 
	| Parameter_variable of Ast.identifier


type ptx_expression =
	| Ptx_reg_declaration of ptx_register_decl
	| Ptx_movement of ptx_data_movement * ptx_data_type * ptx_variable_type * ptx_parameter * ptx_parameter
	| Ptx_Binop of ptx_binary_operator * ptx_data_type * ptx_parameter * ptx_parameter * ptx_parameter
	| Ptx_Return
(*     | Ptx_Array_Literal of ptx_expression list 
	| Ptx_Function_Call of Ast.identifier * ptx_expression list
	| Ptx_Identifier_Expression of Ast.identifier
 *)

type ptx_subroutine = {
	routine_name								: Ast.identifier;
	routine_expressions							: ptx_expression list;
}

type ptx_statement = 
(*     | Ptx_Initialization of ptx_vdecl * ptx_expression *)
(*     | Ptx_Assignment of Ast.identifier * ptx_expression *)
    | Ptx_expression of ptx_expression
    | Ptx_subroutine of ptx_subroutine

type ptx_function_type = 
	| Global 
	| Device 

type ptx_constant = 
{
	ptx_constant_name 							: Ast.identifier;
	ptx_constant_variable_type					: ptx_variable_type;
}

type ptx_vdecl = 
    | Ptx_Vdecl of ptx_data_type * (* need something about global/ptrs here*) ptx_variable_type * Ast.identifier


(* ptx fdecl is the entire file
	it seems it really only needs to be composed of a few parts - a name, a variable declaration list
	and a statement list

	register_decl list should go inside body generated from semantic analyzer
*)
type ptx_fdecl = {
	(* Global or Device *)
	ptx_fdecl_type 								: ptx_function_type; (* probably not needed *)

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
}






(* -----------------------------------------C types -----------------------------------------*)

(*---------------------------------- Unnecessary?????????---------------------------------- *)
type c_binary_operator =
    | Add | Subtract | Multiply | Divide | Modulo
(*     | Plus_Equal | Subtract_Equal | Multiply_Equal | Divide_Equal  *)
(*     | Exp | Dot | Matrix_Multiplication *)
    | And | Or | Xor
    | Equal | Not_Equal | Greater_Than | Less_Than | Greater_Than_Equal | Less_Than_Equal
    | Bitshift_Right | Bitshift_Left 
type c_unary_operator = 
    | Not | Negate
    | Plus_Plus | Minus_Minus

type c_data_type = 
	| String
    | Byte
    | Unsigned_Byte
    | Integer
    | Unsigned_Integer
    | Long
    | Unsigned_Long
    | Float
    | Double
    | Boolean
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
    | Function_Call of Ast.identifier * c_expression list
    | Higher_Order_Function_Call of c_higher_order_function_call
    | Kernel_Function_Call of c_kernel_function_call
    | String_Literal of string
    | Integer_Literal of int
    | Boolean_Literal of bool
    | Floating_Point_Literal of float
    | Array_Literal of c_expression list
    | Identifier_Literal of Ast.identifier 
    | Cast of c_variable_type * c_expression
    | Binop of c_expression * c_binary_operator * c_expression
    | Unop of c_expression * c_unary_operator
    | Array_Accessor of c_expression * c_expression list (* Array, indexes *)
    | Ternary of c_expression * c_expression * c_expression (* expression if true, condition, expression if false *)

type c_variable_statement = 
    | Declaration of c_vdecl
    | Initialization of c_vdecl * c_expression
    | Assignment of Ast.identifier * c_expression

type c_statement = 
    | Variable_Statement of c_variable_statement
    | Expression of c_expression
    | Block of c_statement list (* Used for if, else, for, while blocks *)
    | If of c_expression * c_statement * c_statement (* expression-condition, statement-if block, statement-optional else block *)
    | While of c_expression * c_statement
    | For of c_statement * c_expression * c_statement * c_statement
    | Return of c_expression
    | Return_Void
    | Continue
    | Break

type c_fdecl = {
    c_fdecl_return_type     : c_variable_type;
    c_fdecl_name        	: Ast.identifier;
    c_fdecl_params      	: c_vdecl list;    
    c_fdecl_body        	: c_statement list;
}

(* Overall Program *)
type program = c_variable_statement list * ptx_fdecl list * c_fdecl list