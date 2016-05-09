open Ast 
open Sast
open Utils 
open Exceptions

(* Maps variable name to variable type and value *)
module Variable_Map = Map.Make(String);;
(* Maps function name to return type *)
module Function_Map = Map.Make(String);;

(* For generating names for the device pointers *)
let dev_name_counter = ref 0
(* For generating names for each ptx map function *)
let map_ptx_name_counter = ref 0
(* For generating names for each c map function *)
let map_c_name_counter = ref 0
(* For generating names for each reduce function *)
let reduce_c_name_counter = ref 0
(* For generating names for ptx reduce function *)
let reduce_ptx_name_counter = ref 0
(* For generating arg names*)
let arg_counter = ref 0


(* For generating register counters for datatypes *)
let signed_int_counter = ref 0
let signed_float_counter = ref 0
let predicate_counter = ref 0
(*-----------------------------------Generates Symbols Based on Counters-----------------------------------*)
let generate_device_pointer_name () = 
    let name = "dev_ptr" ^ (string_of_int !dev_name_counter) in 
    incr dev_name_counter; 
    name

let generate_map_c_function_name () = 
    let name = "map_c" ^ (string_of_int !map_c_name_counter) in 
    incr map_c_name_counter; 
    name

let generate_map_ptx_function_name () = 
    let name = "map_ptx" ^(string_of_int !map_ptx_name_counter) in 
    incr map_ptx_name_counter; 
    name

let generate_reduce_c_function_name () = 
    let name = "red_c" ^ (string_of_int !reduce_c_name_counter) in 
    incr reduce_c_name_counter;
    name
let generate_reduce_ptx_function_name () = 
    let name = "red_ptx" ^ (string_of_int !reduce_ptx_name_counter) in 
    incr reduce_ptx_name_counter;
    name

let generate_arg_name () = 
    let name = "arg" ^ (string_of_int !arg_counter) in 
    incr arg_counter;
    name

(*-----------------------------------Types for Semantic Analysis-----------------------------------*)
(* Three types of functions *)
type cuda_function_type  = 
  | Kernel_Global
  | Kernel_Device 
  | Host 

(* Stores information about a function *)
type function_info = {
  (* Host, kernel_device, kernel_global *)
  function_type                   : cuda_function_type; 
  (* Name of function *)
  function_name                   : Ast.identifier;
  (* Function return type and arguments *)
  function_return_type            : Ast.variable_type;
  function_args                   : (Ast.identifier * Ast.variable_type) list;
  (* Functions that are called within this function - needs to be specifically noted for gpu and ptx functions *)
  dependent_functions             : Ast.identifier list;
}

type variable_info = {
  vtype : Ast.variable_type;
  register_number :int;
}
 
(* Stores information about the environment *)
type environment = {
  (* Variables that have been declared in the environment - stores variable name, variable type *)
  variable_scope_stack                              : variable_info Variable_Map.t list;
  (* List of kernel functions that have been declared in the environment  - info from function_info record *)
  kernel_function_map                               : function_info Function_Map.t;
  (* List of host functions that have been declared in the environment - info from function_info record *)
  host_function_map                                 : function_info Function_Map.t;
  (* Bool specifying whether environment is being evaluated on the gpu *)
  is_gpu_env                                        : bool;
  (*Global Void functions for map and reduce*)
  hof_function_list                                 : function_info list;
}

(*-----------------------------------Helper functions to check variables and functions in the environment -----------------------------------*)

let builtin_functions = ["print";"map";"reduce"]


(* Checks if function is a builtin function *)
(* Used to check function declarations to make sure they aren't declaring anything with the same name *)
let is_builtin_function id =
  List.exists (fun function_name -> function_name = id) builtin_functions


(* Creates a function_info record with information *)
let create_function_info ftype rtype args df name = {
  function_type                   = ftype;
  function_name                   = Identifier(name);
  function_return_type            = rtype;
  function_args                   = args;
  dependent_functions             = df;
}


(* Function for adding initializing host function map, adds builtin functions to host function map*)
let init_host_function_map = 
  let fmap = Function_Map.empty in 
  let rec add_functions fmap function_list = 
    match function_list with 
      | [] -> fmap 
      | f_info::tl -> add_functions (Function_Map.add (Utils.idtos(f_info.function_name)) f_info fmap) tl
  in
  (* For now, map/reduce is assigned a return type of Void. 
  Map is included in the function map really so that user can't declare another function with the name map *)
  let create_built_in_function = (create_function_info Host (Ast.Primitive(Ast.Void)) [] []) in 
  let builtin_function_info_structs = List.map create_built_in_function builtin_functions in
  add_functions fmap builtin_function_info_structs


(* Creates a new environment *)
let init_env = {
  variable_scope_stack        = Variable_Map.empty :: [];
  kernel_function_map         = Function_Map.empty;
  host_function_map           = init_host_function_map;
  is_gpu_env                  = false;
  (* Two lists that stores the new higher order functions we need to add*)
  hof_function_list                = []
}


(* Updates the environment *)
let update_env vscope_stack kfmap hfmap is_gpu hof_list = {
  variable_scope_stack        = vscope_stack;
  kernel_function_map         = kfmap;
  host_function_map           = hfmap;
  is_gpu_env                  = is_gpu;
  hof_function_list           = hof_list
}


(* Pushes a new scope on top of the  variable_scope_stack *)
let push_scope env = 
    update_env (Variable_Map.empty :: env.variable_scope_stack) env.kernel_function_map env.host_function_map env.is_gpu_env env.hof_function_list

(* Pops a scope from the top of the variable_scope_stack *)
let pop_scope env = 
    match env.variable_scope_stack with
      | [] -> raise Exceptions.Variable_scope_not_initialized
      | hd::[] -> raise Exceptions.Cannot_pop_original_scope
      | local_scope :: tail ->
          update_env tail env.kernel_function_map env.host_function_map env.is_gpu_env env.hof_function_list

let update_scope updated_scope env = 
    let env1 = pop_scope env in 
    update_env (updated_scope::env.variable_scope_stack) env.kernel_function_map env.host_function_map env.is_gpu_env env.hof_function_list

let update_kernel_fmap f_info env = 
    let new_kfmap = Function_Map.add (Utils.idtos(f_info.function_name)) f_info env.kernel_function_map in
    update_env env.variable_scope_stack new_kfmap env.host_function_map env.is_gpu_env env.hof_function_list

let update_host_fmap f_info env = 
    let new_hfmap = Function_Map.add (Utils.idtos(f_info.function_name)) f_info env.host_function_map in
    update_env env.variable_scope_stack env.kernel_function_map new_hfmap env.is_gpu_env env.hof_function_list


(* Checks if variable has been declared - is valid - in the scope *)
let is_variable_in_scope id env = 
    let rec check_scopes scope_stack = 
      match scope_stack with
        | [] -> false
        | [scope] ->
          if env.is_gpu_env then false 
          else (Variable_Map.mem id scope)
        | scope :: larger_scopes -> 
          if (Variable_Map.mem id scope) then true
          else check_scopes larger_scopes
      in check_scopes env.variable_scope_stack


(* Searches variable in scope for CUDA C and returns its type *)
let get_variable_type id env = 
  let rec check_scopes scope_stack = 
    match scope_stack with 
      | [] -> raise Exceptions.Variable_not_found_in_scope
      | scope::larger_scopes -> 
          if Variable_Map.mem id scope then 
            (Variable_Map.find id scope).vtype
          else
            check_scopes larger_scopes
  in check_scopes env.variable_scope_stack

(* Helper function that returns checks types are the same *)
let same_types t1 t2 = (t1 = t2)

(* Checks if function is valid in the environment *)
let is_function_in_scope id env = 
  if env.is_gpu_env = true then (Function_Map.mem id env.kernel_function_map)
  else (Function_Map.mem id env.host_function_map) || (Function_Map.mem id env.kernel_function_map)


(* Searches for function called in function call and returns information about the function *)
let get_function_info id env = 
    if env.is_gpu_env = true then
        (if(Function_Map.mem id env.kernel_function_map) then
            (Function_Map.find id env.kernel_function_map)
        else raise Exceptions.Function_not_defined)
    else
        (if (Function_Map.mem id env.host_function_map) then 
            (Function_Map.find id env.host_function_map)
        else if (Function_Map.mem id env.kernel_function_map) then 
            (Function_Map.find id env.kernel_function_map)
        else raise Exceptions.Function_not_defined)

(* ----------------------------------- Functions for Checking Ast -----------------------------------*)
(* Checks a variable declaration and initialization to ensure variable hasn't already been declared *)
let check_already_declared id env = 
  if ((is_variable_in_scope id env) = true) then false else true


(* Note: for host only! Checks that a variable in assignments and expressions have been declared*)
let check_var_is_declared id env = 
  if (is_variable_in_scope id env)= false then (raise Exceptions.Variable_not_declared)

(* Helper function that performs type inference for expressions *)
let rec infer_type expression env=
  let f type1 type2 =
    match type1 with
      | Some(t) -> (if t = type2 then Some(t)
                    else raise (Exceptions.Type_mismatch "wrong types"))
      | None -> Some(type2) in
  let match_type expression_list =
    let a = List.fold_left f None expression_list in
      match a with
        | Some(t) -> t
        | None -> raise Exceptions.Empty_array_expression_list in
  match expression with
    | Ast.String_Literal(_) -> Ast.Primitive(Ast.String)
    | Ast.Integer_Literal(_) -> Ast.Primitive(Ast.Integer)
    | Ast.Floating_Point_Literal(_) -> Ast.Primitive(Ast.Float)
    | Ast.Boolean_Literal(_) -> Ast.Primitive(Ast.Boolean)
    | Ast.Array_Literal(expr_list) ->
       let f expression = infer_type expression env in
      Ast.Array(match_type (List.map f expr_list),(List.length expr_list))
    | Ast.Identifier_Literal(id) -> 
        if(check_already_declared (Utils.idtos id) env) = true then raise Exceptions.Variable_not_found_in_scope 
        else (get_variable_type (Utils.idtos id) env)
    | Ast.Binop(e1,op,e2) -> 
        (match op with 
          | Ast.And | Ast.Or | Ast.Xor -> Ast.Primitive(Ast.Boolean)
          | _ -> if (same_types (infer_type e1 env) (infer_type e2 env)) = true then infer_type e1 env 
                  else (raise (Type_mismatch("Binop types don't match")))
        )
    | Ast.Cast(vtype,e) -> vtype
    | Ast.Unop(e,unop) -> infer_type e env
    | Ast.Array_Accessor(e1,e_list) -> 
        (* Check e1 is an array *)
        let check_array = 
          (match infer_type e1 env with 
            | Ast.Array(t,n) -> infer_type t env
            | _ -> raise Exceptions.Not_an_array_expression)
        in
        (* Check valid access *)
        let get_array_type arr dim_list = 
          (match dim_list with
            | [] -> raise Exceptions.Empty_array_access
            | [hd] -> 
              (match arr with
                | Ast.Array(t,n) ->
                | _ -> raise Exceptions.Not_an_array_expression)
              )
          )
            | hd::tl -> 

    | Ast.Ternary(e1,e2,e3) ->
        if(same_types (infer_type e1 env) (infer_type e2 env)) = true then infer_type e1 env else (raise (Type_mismatch("Ternary doesn't return same type")))
    | Ast.Higher_Order_Function_Call(hof) -> 
      let f_info = get_function_info hof.kernel_function_name in
      Ast.Array(f_info.function_return_type,(List.length(List.hd input_arrays)))
    | _ -> raise (Exceptions.Cannot_infer_expression_type)


(* Check that array has only one dimension - used for certain operations *)
let is_one_layer_array expression env = 
    match expression with 
    | Ast.Array_Literal(e_list) as array_literal -> 
        let arr = infer_type array_literal env in
          (match arr with
          | Ast.Array(vtype,size) -> if size > 1 then false else true
          | _ -> raise Exceptions.Not_an_array_expression)
    | _ -> raise Exceptions.Not_an_array_expression


(* Helper function that returns a list of dimensions for an array variable type *)
let rec get_array_dimensions vtype dimensions = 
  match vtype with
  | Ast.Array(t,n) -> 
      get_array_dimensions t (List.rev(n::dimensions))
  | Ast.Primitive(p) -> dimensions
(*   | _ -> raise Exceptions.Unknown_variable_type *)




(* ----------------------------------- Functions for converting ast to sast (Also performs advanced checking) -----------------------------------*)


(* Converts a function declaration to a PTX function declaration. This is done in five parts:
    1. Checking to see if the function exists
    1. Adding a new global (map/reduce) function to the kernel function map in environemnt
    2. Adding a new device function (defg) to the kernel function map in environment
    3. Adding a new function to the host function map in environment
    5. Converting fdecl into ptx_fedcl *)
(* TO IMPLEMENT let convert_to_ptx_fdecl fdecl env = (fdecl,env)
    let global_func_info = {
        function_type = Kernel_Global;
        function_name = fdecl.name;
        function_return_type = fdecl.return_type;
        function_args = fdecl.params;
    } :: env.kernel_function_map in
    
    let kernel_func_info = {
        function_type = Kernel_Device;
        function_name = fdecl.name;
        function_return_type = fdecl.return_type;
        function_args = fdecl.params;
    } :: env.kernel_function_map in
    
    let host_func_info = {
        function_type = Host;
        function_name = fdecl.name;
        function_return_type = fdecl.return_type;
        function_args = fdecl.params;
    } :: env.host_function_map in
    
    (fdecl, update_env env.variable_scope_stack, kernel_func_info, host_func_info env.is_gpu_env);
     *)


(* Converts a function declaration to a C function declaration
    1. Check to see if the function is already declared
    2. Update environment to contain the fdecl
    3. Convert fdecl into c_fdecl *)

(* Converts a list of something to another list *)
let rec convert_list func ast_list sast_list env = 
  match ast_list with 
    | [] -> sast_list
    | hd::tl -> 
      let sast_type,env1 = func hd env in 
        convert_list func tl (List.rev (hd::List.rev(sast_list))) env1

(* Checks statement order - nothing follows a return , break, or continue in a block*)
let good_statement_order stmt_list = 
  match stmt_list with 
    | [] ->true
    | hd ::[] -> true
    | hd :: tl ->
        match hd with 
          | Ast.Return | Ast.Continue | Ast.Break -> false
          | _ -> good_statement_order tl

let rec convert_to_c_statement stmt env = 
  match stmt with 
    | Ast.Variable_Statement(vstmt) -> 
        let c_vstmt,env1 = convert_to_c_variable_statement vstmt env in
        Sast.Variable_Statement(c_vstmt),env1
    | Ast.Expression(e) -> 
        let c_e,env1 = convert_to_c_expression e env in 
        Sast.Expression(c_e),env1
    | Ast.Block(stmt_list) -> 
        (* Check that nothing follows a return , break, or continue in a block *)
        if (good_statement_order stmt_list) = false then raise Exceptions.Have_statements_after_return_break_continue
        else
        (* Convert *)
        let c_stmt_list,env1 = convert_list convert_to_c_statement stmt_list env in
      Sast.Block(c_stmt_list),env1
    | Ast.If(e,stmt1,stmt2) ->
        (* Check that e is a boolean expression *)
        let check_bool = same_types (infer_type e env) Boolean in
        (* Convert*)
        let c_e, env1 = convert_to_c_expression e env in 
        let c_stmt1,env2 = convert_to_c_statement stmt1 env1 in 
        let c_stmt2,env3 = convert_to_c_statement stmt2, env2 in
        Sast.If(c_e,c_stmt1,c_stmt2),env3
    | Ast.While(e,stmt) ->
        let check_bool = same_types (infer_type e env) Boolean in
        (* Check that e is a boolean expression *)
        let c_e, env1 = convert_to_c_expression e env in 
        let c_stmt,env2 = convert_to_c_statement stmt env1 in
        Sast.While(c_e,c_stmt),env2
    | Ast.For(stmt1,e,stmt2,stmt3) ->
        (* Check that stmt1 is an initialization expression *)
        let valid_stmt1 = 
          match stmt1 with
            | Ast.Variable_Statement(vstmt) ->
              (match vstmt with 
                | Assignment(e1,e2) -> true
                | Initialization(vdecl,e) -> true
                | _ -> raise Exceptions.Invalid_statement_in_for
            | _ -> raise Exceptions.Invalid_statement_in_for)
        in
        (* Check that e is a boolean expression *)
        let check_bool = (same_types (infer_type e env) Boolean) in
        (* Convert *)
        let env = push_scope env in
        let c_stmt1,  env1 = convert_to_c_statement   stmt1 env   in
        let c_e,      env2 = convert_to_c_expression  e     env1  in
        let c_stmt2,  env3 = convert_to_c_statement   stmt2 env2  in
        let c_stmt3,  env4 = convert_to_c_statement   stmt3 env3  in
        let env5 = pop_scope env4 in
        Sast.For(c_stmt1,c_e,c_stmt2),env5
    | Ast.Return(e) ->
        let c_e, env1 = convert_to_c_expression e env in
        Sast.Return(c_e),env1 
    | Ast.Return_Void -> Sast.Return_Void,env
    | Ast.Continue -> Sast.Continue,env
    | Ast.Break -> Sast.Break,env

let convert_to_c_param vdecl env  = 
    match vdecl with 
      | Ast.Variable_Declaration(vtype,id) ->
          if(check_already_declared id) = true then raise Exceptions.Variable_already_declared
          else
            let updated_scope = Variable_Map.add id (List.hd env.variable_scope_stack) in
            let env1 = update_scope updated_scope env in
            let c_vtype, env2 = convert_to_c_variable_type vtype env1 in
            Sast.Variable_Declaration(c_vtype,id),env2

(* Converts from fdecl to c_fdecl *)
let convert_to_c_fdecl fdecl env =
    if (is_function_in_scope fdecl.name) = true then (raise Exceptions.Function_already_declared)
    else
      (* Add to function map*)
      (let host_func_info = {
          function_type = Host;
          function_name = fdecl.name;
          function_return_type = fdecl.return_type;
          function_args = fdecl.params;
      } 
      in
      let new_host_fmap = (Function_Map.add host_func_info.function_name host_func_info env.host_function_map) in
      let env1 = update_host_fmap new_host_fmap env in
      (* Push new scope for function *)
      let env2 = push_scope env1 in
      (* Do conversion while passing enviroment *)
      let return_type,  env3    = convert_to_c_variable_type fdecl.return_type env2 in
      let params,       env4    = convert_list convert_to_c_param         fdecl.params  [] env3 in
      let body,         env5    = convert_list convert_to_c_statement     fdecl.body    [] env4 in
      let c_fdecl = {
        c_fdecl_return_type = return_type
        c_fdecl_name = fdecl.name;
        c_fdecl_params = params;
        c_fdecl_body = body;
      }
      in
      (* Pop the variable scope for the function *)
      let env6 = pop_scope env5 in
      c_fdecl, env6)
    
    
let convert_to_c_binop binop env = 
  match binop with
    | Ast.Add -> Sast.Add,env 
    | Ast.Subtract -> Sast.Subtract,env
    | Ast.Multiply -> Sast.Multiply,env
    | Ast.Divide -> Sast.Divide,env
    | Ast.Modulo -> Sast.Modulo,env
    | Ast.And -> Sast.And,env
    | Ast.Or -> Sast.Or,env
    | Ast.Xor -> Sast.Xor,env
    | Ast.Equal -> Sast.Equal,env
    | Ast.Not_Equal -> Sast.Not_Equal,env
    | Ast.Greater_Than -> Sast.Greater_Than,env
    | Ast.Less_Than -> Sast.Less_Than,env
    | Ast.Greater_Than_Equal -> Sast.Greater_Than_Equal,env
    | Ast.Less_Than_Equal -> Sast.Less_than_Equal,env
    | Ast.Bitshift_Right -> Sast.Bitshift_Right,env
    | Ast.Bitshift_Left -> Sast.Bitshift_Left,env
    
let convert_to_ptx_binop binop env = 
  match binop with
    | Ast.Add -> Sast.Ptx_Adde,env
    | Ast.Subtract -> Sast.Ptx_Subtract,env
    | Ast.Multiply -> Sast.Ptx_Multiply,env
    | Ast.Divide -> Sast.Ptx_Divide,env
    | Ast.Modulo -> Sast.Ptx_Modulo,env
    | Ast.And -> Sast.Ptx_And,env
    | Ast.Or -> Sast.Ptx_Or,env
    | Ast.Xor -> Sast.Ptx_Xor,env
    | Ast.Equal -> Sast.Ptx_Equal,env
    | Ast.Not_Equal -> Sast.Ptx_Not_Equal,env
    | Ast.Greater_Than -> Sast.Ptx_Greater_Than,env
    | Ast.Less_Than -> Sast.Ptx_Less_Than,env
    | Ast.Greater_Than_Equal -> Sast.Ptx_Greater_Than_Equal,env
    | Ast.Less_Than_Equal -> Sast.Ptx_Less_than_Equal,env
    | Ast.Bitshift_Right -> Sast.Ptx_Bitshift_Right,env
    | Ast.Bitshift_Left -> Sast.Ptx_Bitshift_Left,env
    
let convert_to_c_unop unop env =
  match unop with
    | Ast.Not -> Sast.Not,env
    | Ast.Negate -> Sast.Negate,env
    | Ast.Plus_Plus -> Sast.Plus_Plus,env
    | Ast.Minus_Minus -> Sast.Minus_Minus,env
    
let convert_to_ptx_unop unop env =
  match unop with
    | Ast.Not -> Sast.Ptx_Not,env
    | Ast.Negate -> Sast.Ptx_Negate,env
    | Ast.Plus_Plus -> Sast.Ptx_Plus_Plus,env
    | Ast.Minus_Minus -> Sast.Ptx_Minus_Minus,env

let convert_to_c_data_type dtype env = 
  match dtype with
      | Ast.Integer -> Sast.Integer,env
      | Ast.Float -> Sast.Float,env
      | Ast.String -> Sast.String,env
      | Ast.Boolean -> Sast.Boolean,env
      | Ast.Void -> Sast.Void,env
      
let convert_to_ptx_data_type dtype env =
  match dtype with
    | Ast.Integer -> Sast.S32,env
    | Ast.Float -> Sast.F32,env
    | Ast.Boolean -> Sast.Pred,env
    | Ast.String -> raise Exceptions.NO_STRINGS_ALLOWED_IN_GDECL
    | Ast.Void -> raise Exceptions.Void_type_in_gdecl
    
let convert_to_c_variable_type vtype env = 
  match vtype with
      | Ast.Primitive(p) -> 
          let c_p,env1 = convert_to_c_data_type p env in
          Sast.Primitive(c_p),env1
      | Ast.Array(t,n) ->
          (match t with 
            | Ast.Array(t,n) -> 
                let c_t,env1 = convert_to_c_variable_type t in
                Sast.Array(c_t,n),env1
            | Ast.Primitive(p) -> 
                let c_p,env1 = convert_to_c_data_type p env in
                Sast.Primitive(c_p),env1
          )

let convert_to_c_vdecl vdecl env  = 
    match vdecl with 
      | Ast.Variable_Declaration(vtype,id) ->
          if(check_already_declared id) = true then raise Exceptions.Variable_already_declared
          else
            let updated_scope = Variable_Map.add id (List.hd env.variable_scope_stack) in
            let env1 = update_scope updated_scope env in
            let c_vtype, env2 = convert_to_c_variable_type vtype env1 in
            Sast.Variable_Declaration(c_vtype,id),env2

let same_types_list type_list = 
  let main_type = (List.hd type_list) in 
  List.map same_types main_type type_list

(* Creates list of sast structs storing information about constants for higher order function *)
let get_constants_info constant_list c_constant_list env = 
  match c_list with 
  | [] -> c_constant_list
  | hd::tl ->
      (match hd with 
        | Constant(id,e) -> 
            let vtype = infer_type e env in 
            (* Name of constant in defg gpu function*)
            let k_name = generate_device_pointer_name in 
            (* Name of constant when input as an argument *)
            let a_name = generate_arg_name in
            (* Sast.type*)
            let constant_info = {
                variable_type = (convert_to_c_variable_type vtype env);
                host_name = id;
                kernel_name = k_name;
                arg_name = k_name;
            }
            in convert_to_c_constants tl (List.rev(constant_info::List.rev(c_constant_list)))
        | _ -> raise Exceptions.Not_a_valid_constant)
    
(* Creates list of sast structs storing information about info arrays from higher order function *)
let get_input_arrays_info input_arrays var_info_list env = 
  match input_arrays with 
    | [] -> c_kernel_var_info_list
    | hd::tl ->
      (
        match infer_type hd env with 
          | Ast.Array(t,n) ->
              let h_name = generate_host_pointer_name in
              let k_name = generate_device_pointer_name in
              let a_name = generate_arg_name in
              let vtype = infer_type hd env in
              let var_info = {
                  variable_type = vtype;
                  host_name = h_name;
                  kernel_name = k_name;
                  arg_name = a_name;
              }
            in convert_input_array_list_to_c_kernel_variable_info_list tl (List.rev(var_info::List.rev(var_info_list)))
          | _ -> raise Exceptions.Nonarray_argument_passed_into_higher_order_function
      )


(* Main function for creating the C map function (when we see a map function call) *)
let make_map_c_fdecl hof_call =  
 {
    higher_order_function_type            =  "map";
    higher_order_function_name            =  generate_map_c_function_name;
    applied_kernel_function               =  generate_map_ptx_function_name;
    constants                             =  get_constants_info hof_call.constants;
    array_length                          =  List.length (hof_call.input_arrays);
    input_arrays_info                     =  get_input_arrays_info hof_call.input_arrays;
    return_type_array_info                =  get_return_array_info hof_call.kernel_function_name; 
 }  

(* Creates sast struct storing information about return array from higher order function *)
let get_return_array_info kfunc_id env = 
  let f_info        = get_function_info kfunc_id env in
  let return_vtype  = f_info.function_return_type in
  let h_name        = generate_host_pointer_name in
  let k_name        = generate_device_pointer_name in 
  let var_info      = {
      variable_type = return_vtype;
      host_name     = h_name;
      kernel_name   = k_name;
      arg_name      = a_name;
  } in
  var_info

(* TO IMPLEMENT  let convert_to_ptx_pdecl e = 
    match e with 
      |  *)
  

(* In CUDA, is just 
        int tid = blockDim.x * blockIdx.x + threadIdx.x;
        if (tid < n_el) 
          C[tid] = function(A[tid], B[tid] ...)
      *)
      (*  Second to last param is result array
          Last param is array length*)
(*  let make_map_ptx_fdecl c_map_fdecl = 
    let input_params = List.map convert_to_ptx_pdecl c_map_fdecl.input_arrays_info in
    let result_param = convert_to_ptx_pdecl c_map_fdecl return_type_array_info in
    let length = Sast.Constant_Int(c_map_fdecl.array_length) in
    let params = input_params@[result_param;length] in 
      let make_global_map_body params = 
          
      in
    let body = make_global_map_body params 
    in
    {
      ptx_fdecl_type    = Sast.Global;
      ptx_fdecl_name    = generate_map_ptx_function_name;
      ptx_fdecl_params  = params;
      ptx_fdecl_body    = body;
    } *)
let get_types args types env = 
            match args with
              | [] -> types
              | hd::tl -> get_types tl (List.rev((infer_type hd env)::List.rev types))

let rec convert_to_c_expression e env = 
    match e with 
      | Ast.Function_Call(id,e_list) ->
        (* Check that function exists in environment *)
        let check_exists = (is_function_in_scope id) in
        (* Check that function arguments match that of function declaration *)
        let f_info = (get_function_info id env) in
        let f_arg_types = f_info.function_args in 
        let check_args expected_arg_types f_args = List.map2 same_types expected_arg_types f_args in
        let good_args = check_args f_arg_types (get_types f_info.function_args env) in
        (* Convert *)
        let e_list,env1 = convert_list convert_to_c_expression e_list env in
        Sast.Function_Call(id,e_list),env1
      | Ast.String_Literal(s) -> Sast.String_Literal(s),env
      | Ast.Integer_Literal(i) -> Sast.Integer_Literal(i),env
      | Ast.Boolean_Literal(b) -> Sast.Boolean_Literal(b),env
      | Ast.Floating_Point_Literal(f) -> Sast.Floating_Point_Literal(f),env
      | Ast.Array_Literal(e_list) -> 
          (* Check all elements of the array are the same type *)
          let type_list = (convert_list infer_type e_list) in 
          let good_type_list = same_types_list type_list in
          (* Get array dimensions and pass to sast *)
          let array_vtype = 
            let f expression = infer_type expression env in
            Ast.Array(match_type (List.map f expr_list),(List.length expr_list)) 
          in
          let array_dim = get_array_dimensions array_vtype in
          (* Convert *)
          let c_e_list, env1 = convert_list convert_to_c_expression e_list in
          Sast.Array_Literal(c_e_list,array_dim),env1
      | Ast.Identifier_Literal(id) -> 
          if(check_already_declared id) = true then raise Exceptions.Variable_not_found_in_scope
          else Sast.Identifier_Literal(id),env
      | Cast(vtype, e) -> 
          let c_vtype ,env1 = convert_to_c_variable_type vtype env in
          let c_e , env2 = convert_to_c_expression e env2 in
          Sast.Cast(c_vtype,c_e),env
      | Binop(e1,op,e2) -> 
          (* Check that expressions match *)
          if(same_types (infer_type e1 env) (infer_type e2 env)) = false then raise Exceptions.Type_mismatch("Binop types don't match") 
          else
            (match op with 
              | And | Or | Not | Xor -> 
                (* Check that type is boolean if using boolean operator *)
                if(same_types (infer_type e1 env) Boolean) = false then raise Exceptions.Type_mismatch("Boolean operator not having a boolean type")
              | _ -> 
                (* Check if type is string*)
                if(same_types (infer_type e1 env) String ) = true then raise Exceptions.String_not_accepted_in_binop
                else 
                  let c_e1,env1 = convert_to_c_expression e1 env in
                  let c_op,env2 = convert_to_c_binop op env1 in
                  let c_e2,env3 = convert_to_c_expression env2 in
                  Sast.Binop(c_e1,c_op,c_e2),env
            )
      | Ast.Higher_Order_Function_Call(hof) -> 
       (* Check that function exists in environment *)
        let check_exists = (is_function_in_scope hof.kernel_function_name) in
        (* Check that arrays are valid arrays *)
        let good_arrays = (List.map same_types_list hof.input_arrays) in
        (* Check that function arguments match that of function declaration *)
        let f_info = (get_function_info hof.kernel_function_name env) in
        let expected_arg_types = f_info.function_args in 
        let get_array_types arr = 
            match arr with 
                | Ast.Array(t,n) -> t
                | _ -> raise Exceptions.Invalid_input_argument_to_map
        in
        let f_arg_types = get_array_types (get_types hof.input_arrays env) in
        let good_args = same_types_list f_arg_types expected_arg_types in
        (*Check that constants match those unknown variables in the defg*)
        let hof_call_constants_names = List.map (fun x -> fst(x)) hof.constants in
        let hof_constants_names = List.map (fun x -> fst(x)) f_info.constants in 
          let check_constants hof_call_c hof_fdecl_c = 
            match hof_decl_c with
            | [] -> true
            | hd::tl -> if (List.exists (fun s -> s = hd) hof_call_c) = false then raise Exceptions.Constants_missing_in_defg
              else check_constants hof_call_c tl 
          in
        let good_constants = check_constants hof_call_constants_names hof_constants_names in
        match hof.higher_order_function_type with 
            | "map" ->
                (*Add the c map function to the environment*)
                let map_fdecl = make_map_c_fdecl in
                let env1 = update_env env.variable_scope_stack env.kernel_function_map env.host_function_map env.is_gpu_env (List.rev(map_fdecl::List.rev(env.hof_function_list))) in
                (* Convert *)
                Sast.FunctionCall(c_map_fdecl.higher_order_function_name,(List.map (fun x -> x.host_name) c_map_fdecl.input_arrays_info)),env1
            (* | "reduce" ->
                in Sast.FunctionCall(c_ma) *)
            | _ -> raise Exceptions.Unknown_higher_order_function_call

(* TO IMPLEMENT let rec convert_to_ptx_expression e = 
  match e with 
    | Ast.Binop(e1,o,e2) ->
      convert_expression_to_ptx(e1) ^ 
      convert_expression_To_ptx(e2) ^ 
      convert_operator(o)
    | Integer_Literal -> Sast.Constant_Int(int)
    |  *)

let convert_to_c_variable_statement vstmt env = 
    match vstmt with 
      | Ast.Declaration(vdecl) -> (* Check that it isn't already declared in convert_to_c_vdecl *) 
            let c_vdecl, new_env = convert_to_c_vdecl vdecl env in
            Sast.Declaration(c_vdecl),new_env
      | Ast.Initialization(vdecl,e) ->
            (*Check same types*)
            let check_types = same_types (fst(vdecl)) (infer_type e) in
            (* Convert  - note vdecl also checks if declared *)
            let c_vdecl, env1 = convert_to_c_vdecl vdecl env in
            let c_e, env2 = convert_to_c_expression e env1 in
            Sast.Initialization(c_vdecl,c_e),env2
      | Ast.Assignment(e1,e2) -> 
            (* Check that identifiers are declared *)
            let id = match e1 with 
              | Ast.Identifier(id) -> if check_already_declared = true then id else (raise Exceptions.Name_not_found(id))
              | Ast.Array_Accessor(e,e_list)->
                    (match e with 
                      | Ast.Identifier -> if check_already_declared = true then id else (raise Exceptions.Name_not_found(id))
                      | _ -> (raise Exceptions.Cannot_assign_expression)
                    )
              | _ -> raise Exceptions.Cannot_assign_expression
            in 
            (* Checks that both sides have the same type *)
            let check_types = if same_types((get_variable_type id env) (infer_type e2)) = false then (raise Exceptions.Type_mismatch("Incompatible types for assignment")) else () in
            (* Convert to sast representations *)
            let c_e1, env1 = convert_to_c_expression e env in 
            let c_e2, env2 = convert_to_c_expression e env1 in 
            Sast.Assignment(c_e1, c_e2), env2

(* TO IMPLEMENT
let convert_to_ptx_variable_statement vstmt env =
    match vstmt with
        | Ast.Declaration(vdecl) -> *)
            
        
(* Converts global vstmt list into c vstmt list *)
let convert_to_c_variable_statement_list vstmt_list c_vstmt_list env = 
     match vstmt_list with 
      | [] -> (c_vstmt_list,env)
      | hd::tl -> 
          let c_vstmt, env1 = convert_to_c_variable_statement hd env in
          convert_to_c_variable_statement_list tl (List.rev(c_vstmt::List.rev(c_vstmt_list))) env1


(* Converts a list of function declarations to ptx and c functions *)
let convert_fdecl_list fdecl_list ptx_fdecl_list c_fdecl_list env = 
     match fdecl_list with 
      | [] -> (ptx_fdecl_list,c_fdecl_list,env)
      | hd::tl -> 
       ( match hd.is_kernel_function with
          | false ->
              let c_fdecl, env1 = convert_to_c_fdecl hd env in 
              convert_fdecl_list tl ptx_fdecl_list List.rev(c_fdecl::List.rev(c_fdecl_list)) env1
(* TEMP          | true -> 
              let ptx_fdecl, new_env = convert_to_ptx_fdecl hd env in
              convert_fdecl_list tl List.rev(ptx_fdecl::List.rev(ptx_fdecl_list)) c_fdecl_list new_env *)
          | _  -> raise Exceptions.NO_STRINGS_ALLOWED_IN_GDECL
        )

(* Main function for converting ast to sast *)
let convert ast env =
    let vstmt_list,env1                     = convert_list convert_to_c_variable_statement (fst(checked_ast)) env in
    let ptx_fdecl_list,c_fdecl_list, env2   = convert_fdecl_list (snd(checked_ast)) [] [] env1 in
    let sast                                = (vstmt_list,ptx_fdecl_list,(env2.hof_function_list),c_fdecl_list) in 
    sast

(* Main function for Sast *)
let analyze ast =  
  let env = init_env in
  let sast = convert ast env in
  sast

