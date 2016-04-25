open Ast 
open Sast
open Exceptions

(* module StringMap = Map.Make(String);;
module Variable_Map = Map.Make(struct type t = identifier let compare = compare end);;
 *)

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

(* Three types of functions *)
type cuda_function_type  = 
  | Kernel_Global
  | Kernel_Device 
  | Host 

(* Stores information about a function *)
type function_info = {
  function_type            : cuda_function_type; (* host, kernel_device, kernel_global *)
  function_name            : identifier;
  function_return_type     : variable_type;
  function_arg_types       : variable_type list;
  function_arg_names       : identifier list;
}
 
(* Stores information about the current environment *)
type environment = {
  (* Variables that have been declared in the environment - stores variable name, variable type and value *)
  variable_scope_stack                        : variable_type Variable_Map.t list;
  (* List of kernel functions that have been declared in the environment *)
  kernel_function_map                         : function_info Function_Map.t;
  (* Functions that have been declared in the environment - notably tracks function name and return type *)
  function_map                                : function_info Function_Map.t;
  (* Bool specifying whether environment is being evaluated on the gpu *)
  is_gpu_env                                  : bool;

}

(* Checks if variable has been declared in the scope *)
let is_var_in_scope id env = 
    let rec check_scopes scope_stack = 
      match scope_stack with
        | [] -> false
        | [scope] ->
          if env.is_gpu_env then false 
          else Variable_Map.mem id scope
        | scope :: larger_scopes -> 
          if Variable_Map.mem id scope then true
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

  (* Searches for declared function in the environment *)
  let valid_function_call id env = 
    if Function_Map.mem id env.function_map then true
    else if Function_Map.mem id env.kernel_function_map then true 
    else false

  (* Searches for function called in function call and returns it as host, kernel_device, or kernel_global*)
  let get_function_type id env = 
    if Function_Map.mem id env.function_map then 
      (Function_Map.find id env.function_map).function_type
    else if Function_Map.mem id env.kernel_function_map then 
      (Function_Map.find id env.kernel_function_map).function_type
    else raise Exceptions.Function_not_defined


type env_checker = 
  | NewScope of (environment -> (string * environment))
  | Env of (environment -> environment)



(*-----------------------------------------------------*)
(*----------------------Initialize---------------------*)
(*-----------------------------------------------------*)
(* type 'a env = {
  scope_stack: variable_type Variable_Map.t list;
}
  
type 'a sourcecomponent =
  | Verbatim of string
  | Generator of ('a -> (string * 'a))
  | NewScopeGenerator of ('a -> (string * 'a))

let create = {
  scope_stack = Variable_Map.empty :: [];
}

let update_env var_map = {
  scope_stack = var_map;
}

(*-----------------------------------------------------*)
(*---------------Scope and variable checking-----------*)
(*-----------------------------------------------------*)
let var_in_scope identifier env =
  let rec check_scope scopes =
    match scopes with
      | [] -> false
      | scope :: tail ->
         if Variable_Map.mem identifier scope then
              true
         else check_scope tail
  in check_scope env.scope_stack

let get_var_type identifier env =
  let rec check_scope scopes =
    match scopes with
      | [] -> let Identifier(sym) = identifier in raise(Exceptions.Name_not_found sym)
      | scope :: tail ->
	  if Variable_Map.mem identifier scope then
            Variable_Map.find identifier scope
          else
            check_scope tail in
  let scope_stack = env.scope_stack in
  check_scope scope_stack

let is_var_declared identifier env =
  match env.scope_stack with
    | [] -> false
    | scope :: tail -> Variable_Map.mem identifier scope

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
    (str, set_var_type identifier variable_type env)

let push_scope env = update_env (Variable_Map.empty :: env.scope_stack)
let pop_scope env = 
  match env.scope_stack with
    | local_scope :: tail -> update_env tail
    | [] -> raise Invalid_environment

(*-----------------------------------------------------*)
(*------------------Build program----------------------*)
(*-----------------------------------------------------*)
let combine initial_env components =
  let f (str, env) component =
    match component with
      | Verbatim(verbatim) -> str ^ verbatim, env
      | Generator(gen) ->
        let new_str, new_env = gen env in
          str ^ new_str, new_env (* in
        List.fold_left f ("", initial_env) components *)
      | NewScopeGenerator(gen) ->
        let new_str, new_env = gen (push_scope env) in
          str ^ new_str, (pop_scope new_env) in
        List.fold_left f ("", initial_env) components *)

(*------------------------------------------------------------*)
(*---------------------Type inference-------------------------*)
(*------------------------------------------------------------*)
(* let rec infer_type expression env =
  let f type1 type2 =
    match type1 with
      | Some(t) -> (if t = type2 then Some(t)
                    else raise (Type_mismatch "wrong types"))
      | None -> Some(type2) in
  let match_type expression_list =
    let a = List.fold_left f None expression_list in
      match a with
        | Some(t) -> t
        | None -> raise Empty_list in
  match expression with
    | String_Literal(_) -> String
    | Array_Literal(expr_list) ->
       let f expression = infer_type expression env in
       Array(match_type (List.map f expr_list),(List.length expr_list))
    | _ -> raise (Exceptions.Cannot_infer_expression_type)
 *)

(* Converts ast to sast *)
let convert_sast ast = ast


(* Main function for Sast *)
let analyze ast =  convert_sast ast

