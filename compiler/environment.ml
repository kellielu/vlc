open Ast

module Variable_Map = Map.Make(String);;
module Function_Map = Map.Make(String);;

exception Already_declared
exception Not_found
exception Invalid_environment

type 'a sourcecomponent =
  | Verbatim of string
  | Generator of ('a -> (string * 'a))

let create = (Function_Map.empty, Variable_Map.empty :: []);;

(* TO DO: need identifier type*)

(*
let get_var_type identifier env =
  let rec check_scope scopes =
    match scopes with
      | [] -> raise Not_found
      | scope :: tail -> Variable_Map.find identifier scope in
  let _, scope_stack = env in
  check_scope scope_stack

let is_var_declared identifier env =
  let rec check_scope scopes =
    match scopes with
      | [] -> false
      | scope :: tail -> Variable_Map.mem identifier scope in
  let _, scope_stack = env in
  check_scope scope_stack

let set_var_type identifier variable_type env =
  let func_map, scope_stack = env in
  let scope, tail = (match scope_stack with
                | scope :: tail-> scope, tail
                | [] -> raise Invalid_environment) in
  let new_scope = Variable_Map.add identifier variable_type scope in
  func_map, new_scope :: tail

let update string variable_type (str, env) =
  if is_var_declared identifier env then
    raise Already_declared
  else
    (str, set_var_type identifier variable_type env)
*)

let combine initial_env components =
  let f (str, env) component =
    match component with
      | Verbatim(verbatim) -> str ^ verbatim, env
      | Generator(gen) ->
        let new_str, new_env = gen env in
          str ^ new_str, new_env in
  List.fold_left f ("", initial_env) components

