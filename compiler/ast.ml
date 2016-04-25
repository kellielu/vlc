type operator =
    | Add | Subtract | Multiply | Divide | Modulo

type identifier = 
    Identifier of string

type data_type = 
    | String
    | Integer
    | Void

type variable_type = 
    | Primitive of data_type
	| Array of variable_type * int
(* 	| Struct of variable_type list * expression list * int *)

type vdecl = 
    Variable_Declaration of variable_type * identifier

type expression =
    | Binop of expression * operator * expression
	| String_Literal of string
	| Integer_Literal of int
    | Array_Literal of expression list 
	| Function_Call of identifier * expression list
	| Identifier_Expression of identifier
    | Higher_Order_Function_Call of higher_order_function_call
and constant = 
    | Constant of identifier * expression
and higher_order_function_call = {
    higher_order_function_type                      : identifier; (* Map or reduce *)
    kernel_function_name                            : identifier;
    constants                                       : constant list;
    input_arrays                                    : expression list; (* Check in semantic analyzer that type is array*)
}

type variable_statement = 
    | Declaration of vdecl
    | Initialization of vdecl * expression
    | Assignment of identifier * expression

type statement = 
    | Variable_Statement of variable_statement
    | Expression of expression
    | Return of expression
    | Return_Void
	
type fdecl = {
    is_kernel_function                              : bool; (* Host or Kernel *)
    return_type                                     : variable_type;
    name                                            : identifier;
    params                                          : vdecl list;    
    body                                            : statement list;
}

(* Program Definition *)
type program = variable_statement list * fdecl list