type binary_operator =
    | Add | Subtract | Multiply | Divide | Modulo (* |Sqrt  *)
    | And | Or | Xor
    | Equal | Not_Equal | Greater_Than | Less_Than | Greater_Than_Equal | Less_Than_Equal
    | Bitshift_Right | Bitshift_Left 
    | Bitwise_Or | Bitwise_And
    
type unary_operator = 
    | Not  | Negate

type identifier = Identifier of string

type data_type = 
    | Unsigned_Byte
    | Byte
    | Unsigned_Integer
    | Integer
    | Long
    | Unsigned_Long
    | Float
    | Double
    | String
    | Boolean
    | Void

type variable_type = 
    | Primitive of data_type
    | Array of variable_type * int

type vdecl = Variable_Declaration of variable_type * identifier

type expression =
    | String_Literal of string 
    | Integer_Literal of int
    | Long_Literal of int64
    | Boolean_Literal of bool
    | Floating_Point_Literal of float
    | Array_Literal of expression list
    | Identifier_Literal of identifier 
    | Function_Call of identifier * expression list
    | Higher_Order_Function_Call of higher_order_function_call
    | Cast of variable_type * expression
    | Binop of expression * binary_operator * expression
    | Unop of expression * unary_operator
    | Array_Accessor of expression * expression list * bool
    | Ternary of expression * expression * expression
and constant = 
    | Constant of identifier * expression
and higher_order_function_call = {
    hof_type                                        : identifier;
    kernel_function_name                            : identifier;
    constants                                       : constant list;
    input_arrays                                    : expression list; 
}

type variable_statement = 
    | Declaration of vdecl
    | Initialization of vdecl * expression
    | Assignment of expression * expression

type statement = 
    | Variable_Statement of variable_statement
    | Expression of expression
    | Block of statement list (* Used for if, else, for, while blocks *)
    | If of expression * statement * statement (* expression-condition, statement-if block, statement-optional else block *)
    | While of expression * statement
    | For of statement * expression * statement * statement
    | Return of expression
    | Return_Void
    | Continue
    | Break
    
type fdecl = {
    is_kernel_function                              : bool; (* Host or Kernel *)
    return_type                                     : variable_type;
    name                                            : identifier;
    params                                          : vdecl list;    
    body                                            : statement list;
}

(* Program Definition *)
type program = variable_statement list * fdecl list