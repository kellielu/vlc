open Ast

module Variable_Map = Map.Make(struct type t = identifier let compare = compare end);;

exception Already_declared
exception Name_not_found of string
exception Invalid_environment

(*-----------------------------------------------------*)
(*----------------------Initialize---------------------*)
(*-----------------------------------------------------*)
type 'a env = {
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
      | [] -> let Identifier(sym) = identifier in raise(Name_not_found sym)
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
                | [] -> raise Invalid_environment) in
  let new_scope = Variable_Map.add identifier variable_type scope in
  update_env (new_scope :: tail)

let update_scope identifier variable_type (str, env) =
  if is_var_declared identifier env then
    raise Already_declared
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
        List.fold_left f ("", initial_env) components

