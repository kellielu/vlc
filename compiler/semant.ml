open Ast 
open Sast
(* open Utils 
open Exceptions *)

(* Maps variable name to variable type and value *)
module Variable_Map = Map.Make(String);;
(* Maps function name to return type *)
module Function_Map = Map.Make(String);;

(* For generating names for the device pointers *)
let dev_name_counter = ref 0
(* For generating names for each map function *)
let map_name_counter = ref 0
(* For generating names for each reduce function *)
let reduce_name_counter = ref 0

(* For generating register counters for datatypes *)
let register_counter = ref 0
(*-----------------------------------Generates Symbols Based on Counters-----------------------------------*)
let generate_device_pointer_name () = 
    let name = (string_of_int !dev_name_counter) in 
    incr dev_name_counter; 
    name

let generate_map_function_name () = 
    let name = (string_of_int !map_name_counter) in 
    incr map_name_counter; 
    name

let generate_reduce_function_name () = 
    let name = (string_of_int !reduce_name_counter) in 
    incr reduce_name_counter;
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
 
(* Stores information about the environment *)
type environment = {
  (* Variables that have been declared in the environment - stores variable name, variable type *)
  variable_scope_stack                              : Ast.variable_type Variable_Map.t list;
  (* List of kernel functions that have been declared in the environment  - info from function_info record *)
  kernel_function_map                               : function_info Function_Map.t;
  (* List of host functions that have been declared in the environment - info from function_info record *)
  host_function_map                                 : function_info Function_Map.t;
  (* Bool specifying whether environment is being evaluated on the gpu *)
  is_gpu_env                                        : bool;
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
}


(* Updates the environment *)
let update_env vscope_stack kfmap hfmap is_gpu = {
  variable_scope_stack        = vscope_stack;
  kernel_function_map         = kfmap;
  host_function_map           = hfmap;
  is_gpu_env                  = is_gpu;
}


(* Pushes a new scope on top of the  variable_scope_stack *)
let push_scope scope env = 
    let new_scope_stack = scope :: env.variable_scope_stack in
    update_env new_scope_stack env.kernel_function_map env.host_function_map env.is_gpu_env


(* Pops a scope from the top of the variable_scope_stack *)
let pop_scope scope env = 
    let new_scope_stack = 
      match env.variable_scope_stack with 
        | [] -> raise Exceptions.Cannot_pop_empty_variable_scope_stack
        | hd :: tl -> tl
    in update_env new_scope_stack env.kernel_function_map env.host_function_map env.is_gpu_env


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
            Variable_Map.find id scope
          else
            check_scopes larger_scopes
  in check_scopes env.variable_scope_stack



(* Checks if function is valid in the environment *)
let is_function_in_scope id env = 
  (Function_Map.mem id env.host_function_map) || (Function_Map.mem id env.kernel_function_map)



(* Searches for function called in function call and returns information about the function *)
let get_function_info id env = 
  if (Function_Map.mem id env.host_function_map) then 
    (Function_Map.find id env.host_function_map)
  else if (Function_Map.mem id env.kernel_function_map) then 
    (Function_Map.find id env.kernel_function_map)
  else raise Exceptions.Function_not_defined

(* ----------------------------------- Functions for Checking Ast -----------------------------------*)

(* Helper function that returns a list of dimensions for an array variable type *)
let rec get_array_dimensions vtype dimensions = 
  match vtype with
  | Ast.Array(t,n) -> 
      get_array_dimensions t (List.rev(n::dimensions))
  | Ast.Primitive(p) -> dimensions
(*   | _ -> raise Exceptions.Unknown_variable_type *)





(* Helper function that performs type inference for expressions *)
let rec infer_type expression =
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
       let f expression = infer_type expression in
       Ast.Array(match_type (List.map f expr_list),(List.length expr_list))
    | _ -> raise (Exceptions.Cannot_infer_expression_type)




(* Checks a variable declaration and initialization to ensure variable hasn't already been declared *)
let check_unique_id id env = 
  if (is_variable_in_scope id env)= true then raise Exceptions.Variable_already_declared


(* Note: for host only! Checks that a variable in assignments and expressions have been declared*)
let check_var_is_declared id env = 
  if (is_variable_in_scope id env)= false then (raise Exceptions.Variable_not_declared)


(* Check that array has only one dimension - used for certain operations *)
let is_one_layer_array expression = 
    match expression with 
    | Ast.Array_Literal(e_list) as array_literal -> 
        let arr = infer_type array_literal in
          (match arr with
          | Ast.Array(vtype,size) -> if size > 1 then false else true
          | _ -> raise Exceptions.Not_an_array_expression)
    | _ -> raise Exceptions.Not_an_array_expression




(* Checks binary operators *)
(* let check_binop binop e1 e2 host_env = 
  match binop with 
    | Ast.Add
    | Ast.Subtract 
    | Ast.Multiply 
    | Ast.Divide 
    | Ast.Modulo *)


(*-----------------------------------------------------*)
(*---------------Scope and variable checking-----------*)
(*-----------------------------------------------------*)
(* let var_in_scope identifier env =
  let rec check_scope scopes =
    match scopes with
      | [] -> false
      | scope :: tail ->
         if Variable_Map.mem identifier scope then
              true
         else check_scope tail
  in check_scope env.scope_stack

let set_var_type identifier variable_type env =
  let scope, tail = (match env.scope_stack with
                | scope :: tail-> scope, tail
                | [] -> raise Exceptions.Invalid_environment) in
  let new_scope = Variable_Map.add identifier variable_type scope in
  update_env (new_scope :: tail)

let update_scope identifier variable_type (str, env) =
  if is_var_declared identifier env then
    raise Exceptions.Already_declared
  else
    (str, set_var_type identifier variable_type env) *)

(* ----------------------------------- Functions for converting ast to sast (Also performs advanced checking) -----------------------------------*)

(*-----------------------------------Main functions for semantic analysis-----------------------------------*)

(* Main function for walking through ast and doing semantic checking *)
(* let check_ast ast env = 
  analyze_variable_statement(fst(ast));
  analyze_fdecl(snd(ast));
  ast *)


(* (* Main function for checking the sast after conversion (really just have to check for the map/reduce constant types) *)
let check_sast sast = sast

 Main function for converting ast to sast 
  Vlc -> c is simple, just convert it normally. We have to do some more sophisticated
  stuff for vlc ast -> ptx sast. 
 *)

 

let convert_to_ptx_fdecl fdecl env = (fdecl,env)

let convert_to_c_fdecl fdecl env = (fdecl,env)
  (* env.push *)
  (* env.pop *)

let convert_to_c_variable_statement vstmt env = (vstmt env)
(*     match vstmt with 
      | Ast.Declaration(vdecl) -> 
        let c_vdecl, new_env = 
        in Sast.Declaration(c_vdecl),new_env *)

(* Converts global vstmt list into c vstmt list *)
let convert_to_c_variable_statement_list vstmt_list c_vstmt_list env = 
    (vstmt_list c_vstmt_list env)
(*     match vstmt_list with 
      | [] -> (c_vstmt_list,env)
      | hd::tl -> 
          let c_vstmt, new_env = convert_to_c_variable_statement hd env in
          convert_to_c_variable_statement_list tl (List.rev(hd::List.rev(c_vstmt_list))) *)

(* Converts a list of function declarations to ptx and c functions *)
let convert_fdecl_list fdecl_list ptx_fdecl_list c_fdecl_list env = 
    (fdecl_list ptx_fdecl_list c_fdecl_list env)
(*     match fdecl_list with 
      | [] -> (ptx_fdecl_list,c_fdecl_list,env)
      | hd::tl -> 
        (match hd.is_kernel_function with
          | true -> 
              let ptx_fdecl, new_env = convert_to_ptx_fdecl hd env in
              convert_fdecl_list tl List.rev(ptx_fdecl::List.rev(ptx_fdecl_list)) c_fdecl_list new_env
          | false ->
              let c_fdecl, new_env = convert_to_c_fdecl hd env in 
              convert_fdecl_list tl ptx_fdecl_list List.rev(c_fdecl::List.rev(c_fdecl_list)) new_env
        ) *)

(* Main function for converting ast to sast *)
let convert ast env = (ast env)
  let vstmt_list,env1                     = convert_to_c_variable_statement_list (fst(checked_ast)) [] env in
  let ptx_fdecl_list,c_fdecl_list, env2   = convert_fdecl_list (snd(checked_ast)) [] [] env1 in
  let sast                                = (vstmt_list,ptx_fdecl_list,c_fdecl_list) in 
  sast

(* Main function for Sast *)
let analyze ast =  
  let env = init_env in
  let sast = convert ast env in
  sast


