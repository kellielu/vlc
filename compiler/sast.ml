(* Contains sast type definitions for conversions during semantic analysis *)

(* -----------------------------------------PTX types -----------------------------------------*)
type ptx_identifier = {
	var_name 	: Ast.identifier;
	reg_name	: string; (* Register name it is stored in *)
	reg_num		: int; 	  (* Register number it is stored in *)
	write_reg 	: bool;   (* Boolean used for codegen to indicate whether we should use the variable name or the register name*)
}

type ptx_data_type =
	S32 | F32 | Pred | Ptx_Void | U64
(* 	U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | F32 *)

type ptx_variable_type = 
	| Ptx_Primitive of ptx_data_type
	| Ptx_Array of ptx_variable_type * int					(* Type array stores, Size of array*)
	| Ptx_Pointer of ptx_variable_type						(* All pointer are s64, variable_type is the type that the pointer stores*)

(* type ptx_variable_option = 
	| Ptx_empty_option (* codegen will generate nothing for this*)
	| Ptx_Vector of int (* int refers to length of vector*)
	| Ptx_Alignment of int (* int refers to address alignment*)
 *)
type ptx_literal = 
	| Ptx_Signed_Integer of int
	| Ptx_Signed_Float of float 
	| Ptx_Identifier_Literal of ptx_identifier
	| Ptx_Array_Literal of ptx_literal list
	| Ptx_Array_Access of ptx_literal * ptx_literal list

type ptx_unary_operator = 
    | Ptx_Not  | Ptx_Negate
    | Ptx_Plus_Plus | Ptx_Minus_Minus

type ptx_binary_operator =
    | Ptx_Add | Ptx_Subtract | Ptx_Multiply | Ptx_Divide | Ptx_Modulo
(*     | Plus_Equal | Subtract_Equal | Multiply_Equal | Divide_Equal  *)
(*     | Exp | Dot | Matrix_Multiplication *)
    | Ptx_And | Ptx_Or | Ptx_Xor
 	| Ptx_Bitshift_Right | Ptx_Bitshift_Left 
    | Ptx_Equal | Ptx_Not_Equal | Ptx_Greater_Than | Ptx_Less_Than | Ptx_Greater_Than_Equal 
    | Ptx_Less_Than_Equal
    | Ptx_Bitwise_Or
    | Ptx_Bitwise_And
(*     Ptx_Greater_Than_Unsigned | Ptx_Less_Than_unsigned | Ptx_Greater_Than_Equal_Unsigned 
    | Ptx_Less_Than_Equal_Unsigned  *)

type ptx_state_space = 
	| Constant
	| Global 
	| Local 
	| Shared
	| Param
	| State_Undefined

(* Storage type of variable for constants *)
type ptx_var_info = {
	ptx_variable_type 			: ptx_variable_type;
	ptx_id 						: ptx_identifier;
}

type ptx_register_declaration = {
	reg_type 			: ptx_data_type;
	reg_id				: string;
	num_registers		: int;	
}
(* Is included when we talk about parameters *)
type ptx_vdecl = 
(* * ptx_variable_option  *)
    | Ptx_Vdecl of ptx_state_space *  ptx_variable_type *  ptx_identifier

type ptx_statement =
(* Load,store,move*)
	| Ptx_Load of ptx_state_space * ptx_variable_type * ptx_literal * ptx_literal
	| Ptx_Store of ptx_state_space * ptx_variable_type * ptx_literal * ptx_literal
	| Ptx_Move of ptx_variable_type * ptx_literal * ptx_literal
(* Expressions*)
	| Ptx_Binop of ptx_binary_operator * ptx_variable_type * ptx_literal * ptx_literal * ptx_literal
	| Ptx_Unop of ptx_unary_operator * ptx_variable_type * ptx_literal * ptx_literal 
	| Ptx_Call of ptx_literal * Ast.identifier * ptx_literal list
	| Ptx_Empty_Call of Ast.identifier * ptx_literal list
(* Statements and Conditionsals*)
	| Ptx_Variable_Declaration of ptx_vdecl
	| Ptx_Branch of ptx_identifier * Ast.identifier
	| Ptx_Block of ptx_statement list
	| Ptx_Subroutine of Ast.identifier * ptx_statement list
	| Ptx_Return_Void
	| Ptx_Cast of ptx_variable_type * ptx_variable_type * ptx_identifier * ptx_identifier
	| Ptx_Empty
	
type ptx_function_type = 
	| Global_Function
	| Device_Function

type ptx_fdecl = {
	ptx_fdecl_type 								: ptx_function_type; (* Global or Device, might not be needed*)
	ptx_fdecl_name 								: Ast.identifier; (* Name of the function *)
	ptx_fdecl_input_params 						: ptx_vdecl list; (* Expected parameters of the function *)
	ptx_fdecl_return_param						: ptx_vdecl; (* Parameter that is returned *)
	register_decls 								: ptx_register_declaration list; (* Declares the virtual registers that are needed for the function *)
	ptx_fdecl_body 								: ptx_statement list; (* Expressions within the function *)
}

type ptx_higher_order_fdecl = {
	ptx_higher_order_function_type 				: Ast.identifier;  	(* Map or reduce *)
	ptx_higher_order_function_name 				: Ast.identifier;	(* Name of this function  - ex. map123, map1, map2 *)
    ptx_applied_kernel_function    				: Ast.identifier; 	(* Name of kernel function that is called by this function *)
	ptx_higher_order_function_constants 		: ptx_vdecl list; 	(* List of constants passed in that the function can use *)
	ptx_array_length 							: int; 	(* Size of input and return arrays *)
	(* Input array information 
		--If an array has no name (just simply passed in as something like {1,2,3}) then it is given a temporary generated name *)
	ptx_input_arrays_info						: ptx_vdecl list; (* type, host name, kernel name *)
    ptx_return_array_info              			: ptx_vdecl;     (* Return array information *)	
    ptx_called_functions 						: Ast.identifier list;     (* Dependent functions*)  
    ptx_register_decls 							: ptx_register_declaration list;
}
(* -----------------------------------------C types -----------------------------------------*)
type c_binary_operator =
    | Add | Subtract | Multiply | Divide | Modulo
(*     | Plus_Equal | Subtract_Equal | Multiply_Equal | Divide_Equal  *)
(*     | Exp | Dot | Matrix_Multiplication *)
    | And | Or | Xor
    | Equal | Not_Equal | Greater_Than | Less_Than | Greater_Than_Equal | Less_Than_Equal
    | Bitshift_Right | Bitshift_Left 
    | Bitwise_Or | Bitwise_And

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
	higher_order_function_constants 		: c_kernel_variable_info list;
	(* Size of input and return arrays *)
	array_length 							: int;
	(* Input array information 
		--If an array has no name (just simply passed in as something like {1,2,3}) then it is given a temporary generated name *)
	input_arrays_info						: c_kernel_variable_info list; (* type, host name, kernel name *)
    (* Return array information *)	
    return_array_info              			: c_kernel_variable_info; (* type, host name, kernel name*)    
    (* Called functions so we can load them*)
    called_functions 						: Ast.identifier list;
}

type c_expression =
    | Function_Call of Ast.identifier * c_expression list
    | String_Literal of string
    | Integer_Literal of int
    | Boolean_Literal of bool
    | Floating_Point_Literal of float
    | Array_Literal of c_expression list * int list
    | Identifier_Literal of Ast.identifier (*id, is_lvalue*)
    | Cast of c_variable_type * c_expression
    | Binop of c_expression * c_binary_operator * c_expression
    | Unop of c_expression * c_unary_operator
    | Array_Accessor of c_expression * c_expression list* bool*bool(* Array, indexes, is_lvalue, array_access *)
    | Ternary of c_expression * c_expression * c_expression (* expression if true, condition, expression if false *)

type c_variable_statement = 
    | Declaration of c_vdecl
    | Initialization of c_vdecl * c_expression
    | Assignment of c_expression * c_expression(* bool=true if left hand side is an array *)

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
type program = c_variable_statement list * ptx_fdecl list * ptx_higher_order_fdecl * c_higher_order_fdecl list * c_fdecl list