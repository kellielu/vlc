(* Contains sast type definitions for conversions during semantic analysis *)

(* -----------------------------------------PTX types -----------------------------------------*)
type ptx_literal = 
	| Ptx_signed_int of int
	| Ptx_signed_float of float

type ptx_binary_operator =
    | Ptx_Add | Ptx_Subtract | Ptx_Multiply | Ptx_Divide | Ptx_Modulo
(*     | Plus_Equal | Subtract_Equal | Multiply_Equal | Divide_Equal  *)
(*     | Exp | Dot | Matrix_Multiplication *)
    | Ptx_And | Ptx_Or | Ptx_Xor
 	| Ptx_Bitshift_Right | Ptx_Bitshift_Left 
    | Ptx_Equal | Ptx_Not_Equal | Ptx_Greater_Than | Ptx_Less_Than | Ptx_Greater_Than_Equal 
    | Ptx_Less_Than_Equal
(*     Ptx_Greater_Than_Unsigned | Ptx_Less_Than_unsigned | Ptx_Greater_Than_Equal_Unsigned 
    | Ptx_Less_Than_Equal_Unsigned  *)

type ptx_unary_operator = 
    | Ptx_Not  | Ptx_Negate
    | Ptx_Plus_Plus | Ptx_Minus_Minus

type ptx_data_type =
	S32 | F32 | Pred
(* 	U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | F32 *)

type ptx_state_space = 
	| Register_state
	| Constant
	| Global 
	| Local 
	| Shared
	| Param
	| State_undefined

type ptx_variable = 
	| Parameterized_variable_register of Ast.identifier * int (* register name, number of registers*)
	| Variable_register of Ast.identifier * int (*register name, register number*)
	| Constant_int of int 
	| Constant_float of float
	| Variable_array of Ast.identifier * int (* array name, size of array *)
	| Variable_array_initialized of Ast.identifier * ptx_literal list
	| Ptx_Variable of Ast.identifier
	| Ptx_Variable_initialized of Ast.identifier * ptx_literal

(* type ptx_variable_option = 
	| Ptx_empty_option (* codegen will generate nothing for this*)
	| Ptx_Vector of int (* int refers to length of vector*)
	| Ptx_Alignment of int (* int refers to address alignment*)
 *)
type ptx_pdecl = {
	ptx_parameter_data_type: 		ptx_data_type;
(* 	ptx_parameter_is_pointer:		int; 	(* 1 if true, 0 if false*) *)
	ptx_parameter_state_space:		ptx_state_space;
(* 	ptx_parameter_variable_option:	ptx_variable_option; *)
	ptx_parameter_name:				Ast.identifier;
}

type ptx_vdecl = 
(* * ptx_variable_option  *)
    | Ptx_Vdecl of ptx_state_space *  ptx_data_type *  ptx_variable

type ptx_expression =
(*convert may require some options prior to first data type*)
	| Ptx_Binop of ptx_binary_operator * ptx_data_type * ptx_variable * ptx_variable * ptx_variable
	| Ptx_Unop of ptx_unary_operator * ptx_data_type * ptx_variable * ptx_variable
	| Ptx_Load of ptx_state_space * ptx_data_type * ptx_variable * ptx_variable 
	| Ptx_Store of ptx_state_space * ptx_data_type * ptx_variable * ptx_variable 
	| Ptx_vdecl of ptx_vdecl
	| Ptx_Move of ptx_data_type * ptx_variable * ptx_variable
	| Ptx_Branch of Ast.identifier
	| Predicated_statement of ptx_variable * ptx_expression
	| Ptx_Convert of ptx_data_type * ptx_data_type * ptx_variable * ptx_variable
	| Ptx_Return

type ptx_subroutine = {
	routine_name								: Ast.identifier;
	routine_expressions							: ptx_expression list;
}

type ptx_statement = 
    | Ptx_expression of ptx_expression
    | Ptx_subroutine of ptx_subroutine

type ptx_function_type = 
	| Global_func 
	| Device_func

type ptx_variable_type = 
	| Ptx_Primitive of ptx_data_type
	| Ptx_Array of ptx_variable_type * int 					(* 'int' refers to the length of the array *)
	| Ptx_Pointer of ptx_variable_type * int 				(* 'int' refers to size of memory pointed by the pointer *)
(* ptx fdecl is the entire file
	it seems it really only needs to be composed of a few parts - a name, a variable declaration list
	and a statement list

	register_decl list should go inside body generated from semantic analyzer
*)
type ptx_fdecl = {
	(* Global or Device *)
	ptx_fdecl_type 								: ptx_function_type;

	(* Name of the function *)
	ptx_fdecl_name 								: Ast.identifier;

	(* Expected parameters of the function *)
	ptx_fdecl_params 							: ptx_pdecl list;

	(* Declares the virtual registers that are needed for the function *)
	(* register_decls 								: ptx_vdecl list; *)
	(* Statements within the function *)
	ptx_fdecl_body 								: ptx_statement list;
}

type ptx_kernel_variable_info = {
	ptx_variable_type 			: ptx_variable_type;
	ptx_kernel_name 			: Ast.identifier;
}

type ptx_higher_order_fdecl = {
	(* Map or reduce *)
	ptx_higher_order_function_type 				: Ast.identifier; 
	(* Name of this function  - ex. map123, map1, map2 *)
	ptx_higher_order_function_name 				: Ast.identifier;
	(* Name of kernel function that is called from host (would be global void kernel function corresponding to map/reduce) *)
    ptx_applied_kernel_function    				: Ast.identifier;
	(* List of constants passed into map and reduce *)
	ptx_higher_order_function_constants 		: ptx_kernel_variable_info list;
	(* Size of input and return arrays *)
	ptx_array_length 							: int;
	(* Input array information 
		--If an array has no name (just simply passed in as something like {1,2,3}) then it is given a temporary generated name *)
	ptx_input_arrays_info						: ptx_kernel_variable_info list; (* type, host name, kernel name *)
    (* Return array information *)	
    ptx_return_array_info              			: ptx_kernel_variable_info; (* type, host name, kernel name*) 
    (* Dependent functions*)
    ptx_called_functions 						: Ast.identifier list;   
}
(* -----------------------------------------C types -----------------------------------------*)
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

type c_vdecl = 
    Variable_Declaration of c_variable_type * Ast.identifier

(* Our type for storing information about variables that will be copied from host and GPU *)
type c_kernel_variable_info = {
	variable_type 			: c_variable_type;
	host_name 				: Ast.identifier;
	arg_name				: Ast.identifier;
	kernel_name 			: Ast.identifier;
}

(* C function that is generate to do memory copying and to call the global void map function*)
type c_higher_order_fdecl = {
	(* Map or reduce *)
	higher_order_function_type 				: Ast.identifier; 
	(* Name of this function  - ex. map123, map1, map2 *)
	higher_order_function_name 				: Ast.identifier;
	(* Name of kernel function that is called from host (would be global void kernel function corresponding to map/reduce) *)
    applied_kernel_function    				: Ast.identifier;
	(* List of constants passed into map and reduce *)
	higher_order_function_constants 								: c_kernel_variable_info list;
	(* Size of input and return arrays *)
	array_length 							: int;
	(* Input array information 
		--If an array has no name (just simply passed in as something like {1,2,3}) then it is given a temporary generated name *)
	input_arrays_info						: c_kernel_variable_info list; (* type, host name, kernel name *)
    (* Return array information *)	
    return_array_info              			: c_kernel_variable_info; (* type, host name, kernel name*) 
    (* Dependent functions*)
    called_functions 						: Ast.identifier list;   
}

type c_expression =
    | Function_Call of Ast.identifier * c_expression list
    | String_Literal of string
    | Integer_Literal of int
    | Boolean_Literal of bool
    | Floating_Point_Literal of float
    | Array_Literal of c_expression list * int list
    | Identifier_Literal of Ast.identifier 
    | Cast of c_variable_type * c_expression
    | Binop of c_expression * c_binary_operator * c_expression
    | Unop of c_expression * c_unary_operator
    | Array_Accessor of c_expression * c_expression list (* Array, indexes *)
    | Ternary of c_expression * c_expression * c_expression (* expression if true, condition, expression if false *)

type c_variable_statement = 
    | Declaration of c_vdecl
    | Initialization of c_vdecl * c_expression
    | Assignment of c_expression * c_expression

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
type program = c_variable_statement list * ptx_fdecl list * c_higher_order_fdecl list* c_fdecl list