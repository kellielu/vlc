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
(* For generating names for each ptx map function *)
let map_ptx_name_counter = ref 0
(* For generating names for each c map function *)
let map_c_name_counter = ref 0
(* For generating names for each reduce function *)
let reduce_name_counter = ref 0

(* For generating register counters for datatypes *)
let signed_int_counter = ref 0
let signed_float_counter = ref 0
let predicate_counter = ref 0
(*-----------------------------------Generates Symbols Based on Counters-----------------------------------*)
let generate_device_pointer_name () = 
    let name = (string_of_int !dev_name_counter) in 
    incr dev_name_counter; 
    name

let generate_map_c_function_name () = 
    let name = (string_of_int map_c_name_counter) in 
    incr map_c_name_counter; 
    name

let generate_map_ptx_function_name () = 
    let name = (string_of_int map_ptx_name_counter) in 
    incr map_ptx_name_counter; 
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

type variable_info = {
  variable_type = Ast.variable_type;
  register_number = int;
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
  hof_ptx_global                                    : function_info list;
  hof_c                                             : function_info list;
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
let push_scope env = 
    update_env (VariableMap.empty :: env.variable_scope_stack) env.host_function_map env.is_gpu_env


(* Pops a scope from the top of the variable_scope_stack *)
let pop_scope env = 
    match env.variable_scope_stack with
      | local_scope :: tail ->
          update_env tail env.host_function_map env.is_gpu_env


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
            (Variable_Map.find id scope).variable_type
          else
            check_scopes larger_scopes
  in check_scopes env.variable_scope_stack

(* Helper function that returns checks types are the same *)
let same_types t1 t2 = (t1 = t2)

(* Checks if function is valid in the environment *)
let is_function_in_scope id env = 
  (Function_Map.mem id env.host_function_map) || (Function_Map.mem id env.kernel_function_map)



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

(* Helper function that returns a list of dimensions for an array variable type *)
let rec get_array_dimensions vtype dimensions = 
  match vtype with
  | Ast.Array(t,n) -> 
      get_array_dimensions t (List.rev(n::dimensions))
  | Ast.Primitive(p) -> dimensions
(*   | _ -> raise Exceptions.Unknown_variable_type *)


(* *)



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
    | Ast.Identifier_Literal(id) -> 
        if(check_already_declared id) = true 
            then raise Exceptions.Variable_not_found_in_scope 
        else (get_variable_type id)
    | Ast.Binop(e1,op,e2) -> 
        (match op with 
          |Ast.And | Ast.Or | Ast.Not | Ast.Xor -> Ast.Bool
          | _ -> if (same_types (infer_type e1) (infer_type e2)) = true then infer_type e1 else (raise Type_mismatch("Binop types don't match"))
    | Ast.Cast(vtype,e) -> vtype
    | Ast.Unop(e,unop) -> infer_type e
    | Ast.Array_Accessor(e1,e_list) -> 
        (match e1 with 
          | Ast.Array(t,n) -> infer_type t
          | _ -> raise Exceptions.Not_an_array_expression)
    | Ast.Ternary(e1,e2,e3) ->
        if(same_types (infer_type e1) (infer_type e2)) = true then infer_type e1 else (raise Type_mismatch("Ternary doesn't return same type"))
    | Ast.Higher_Order_Function_Call(hof) -> 
      let f_info = get_function_info hof.kernel_function_name in
      Ast.Array(f_info.function_return_type,(List.length(List.hd input_arrays))
    | _ -> raise (Exceptions.Cannot_infer_expression_type)




(* Checks a variable declaration and initialization to ensure variable hasn't already been declared *)
let check_already_declared id env = 
  if (is_variable_in_scope id env)= true then false


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
let check_binop binop e1 e2 env = 
  match binop with 
    | Ast.Add
    | Ast.Subtract 
    | Ast.Multiply 
    | Ast.Divide 
    | Ast.Modulo

(* ----------------------------------- Functions for converting ast to sast (Also performs advanced checking) -----------------------------------*)

(*-----------------------------------Main functions for semantic analysis-----------------------------------*)

(* Main function for walking through ast and doing semantic checking *)
(* let check_ast ast env = 
  analyze_variable_statement(fst(ast));
  analyze_fdecl(snd(ast));
  ast *)


(* (* Main function for checking the sast after c_expression (really just have to check for the map/reduce constant types) *)
let check_sast sast = sast

 Main function for converting ast to sast 
  Vlc -> c is simple, just convert it normally. We have to do some more sophisticated
  stuff for vlc ast -> ptx sast. 
 *)

 let make_map_ptx_fdecl hof_call = {
    ptx_fdecl_type = Sast.Global;
    ptx_fdecl_name = generate_map_ptx_function_name
 }

 let make_map_c_fdecl hof_call ptx_fdecl = {
    c_fdecl_return_type = Array()
 }

(* Converts a function declaration to a PTX function declaration. This is done in five parts:
    1. Checking to see if the function exists
    1. Adding a new global (map/reduce) function to the kernel function map in environemnt
    2. Adding a new device function (defg) to the kernel function map in environment
    3. Adding a new function to the host function map in environment
    5. Converting fdecl into ptx_fedcl *)
let convert_to_ptx_fdecl fdecl env = (fdecl,env)
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
    


(* Converts a function declaration to a C function declaration
    1. Check to see if the function is already declared
    2. Update environment to contain the fdecl
    3. Convert fdecl into c_fdecl *)
let convert_to_c_fdecl fdecl env = (fdecl,env)
    let host_func_info = {
        function_type = Host;
        function_name = fdecl.name;
        function_return_type = fdecl.return_type;
        function_args = fdecl.params;
    } :: env.host_function_map in
    
    (fdecl, update_env env.variable_scope_stack, env.kernel_function_map, host_func_info env.is_gpu_env);
    


(* Converts a list of something to another list *)
let rec convert_list func ast_list sast_list= 
  match ast_list with 
    | [] -> sast_list
    | hd:tl -> 
      let sast_type = func hd in 
        convert_list func tl (List.reverse(hd::List.reverse(sast_list)))
    
let convert_to_c_binop binop env = 
    | Ast.Add -> Sast.Add
    | Ast.Subtract -> Sast.Subtract
    | Ast.Multiply -> Sast.Multiply
    | Ast.Divide -> Sast.Divide
    | Ast.Modulo -> Sast.Modulo
    | Ast.And -> Sast.And
    | Ast.Or -> Sast.Or
    | Ast.Xor -> Sast.Xor
    | Ast.Equal -> Sast.Equal
    | Ast.Not_Equal -> Sast.Not_Equal
    | Ast.Greater_Than -> Sast.Greater_Than
    | Ast.Less_Than -> Sast.Less_Than
    | Ast.Greater_Than_Equal -> Sast.Greater_Than_Equal
    | Ast.Less_Than_Equal -> Sast.Less_than_Equal
    | Ast.Bitshift_Right -> Sast.Bitshift_Right
    | Ast.Bitshift_Left -> Sast.Bitshift_Left
    
let convert_to_ptx_binop binop env = 
    | Ast.Add -> Sast.Ptx_Add
    | Ast.Subtract -> Sast.Ptx_Subtract
    | Ast.Multiply -> Sast.Ptx_Multiply
    | Ast.Divide -> Sast.Ptx_Divide
    | Ast.Modulo -> Sast.Ptx_Modulo
    | Ast.And -> Sast.Ptx_And
    | Ast.Or -> Sast.Ptx_Or
    | Ast.Xor -> Sast.Ptx_Xor
    | Ast.Equal -> Sast.Ptx_Equal
    | Ast.Not_Equal -> Sast.Ptx_Not_Equal
    | Ast.Greater_Than -> Sast.Ptx_Greater_Than
    | Ast.Less_Than -> Sast.Ptx_Less_Than
    | Ast.Greater_Than_Equal -> Sast.Ptx_Greater_Than_Equal
    | Ast.Less_Than_Equal -> Sast.Ptx_Less_than_Equal
    | Ast.Bitshift_Right -> Sast.Ptx_Bitshift_Right
    | Ast.Bitshift_Left -> Sast.Ptx_Bitshift_Left
    
let convert_to_c_unop unop env =
    | Ast.Not -> Sast.Not
    | Ast.Negate -> Sast.Negate
    | Ast.Plus_Plus -> Sast.Plus_Plus
    | Ast.Minus_Minus -> Sast.Minus_Minus
    
let convert_to_ptx_unop unop env =
    | Ast.Not -> Sast.Ptx_Not
    | Ast.Negate -> Sast.Ptx_Negate
    | Ast.Plus_Plus -> Sast.Ptx_Plus_Plus
    | Ast.Minus_Minus -> Sast.Ptx_Minus_Minus

let convert_to_c_data_type dtype env = 
      | Ast.Integer -> Sast.Integer
      | Ast.Float -> Sast.Float
      | Ast.String -> Sast.String
      | Ast.Boolean -> Sast.Boolean
      | Ast.Void -> Sast.Void
      
let convert_to_ptx_data_type dtype env =
    | Ast.Integer -> Sast.S32
    | Ast.Float -> Sast.F32
    | Ast.Boolean -> Sast.Pred
    | Ast.String -> raise Exceptions.NO_STRINGS_ALLOWED_IN_GDECL
    | Ast.Void -> raise Exceptions.Void_type_in_gdecl
    
let convert_to_c_variable_type vtype env = 
      | Ast.Primitive(p) -> Sast.Primitive((convert_to_c_data_type p))
      | Ast.Array(t,n) ->
          (match t with 
            | Ast.Array(t,n) -> Sast.Array((convert_to_c_variable_type t),n)
            | Ast.Primitive(p) -> Sast.Primitive((convert_to_c_data_type p))
          )

let convert_to_c_vdecl vdecl env  = 
    match vdecl with 
      | Ast.Variable_Declaration(vtype,id) ->
          if(check_already_declared id) = true then raise Exceptions.Variable_already_declared
          else
            let new_vscope_stack = Variable_Map.add id env.
            let new_env = update_env new_vscope_stack env.kernel_function_map env.host_function_map env.is_gpu env in
            let c_vtype, new_env = convert_to_c_variable_type vtype new_env in
            Sast.Variable_Declaration(c_vtype,id),new_env

let same_types_list type_list = 
  let main_type = (List.hd type_list) in 
  List.map same_types main_type type_list

let rec convert_to_c_expression e env = 
    match e with 
      | Ast.Function_Call(id,e_list) ->
        (* Check that function exists in environment *)
        let check_exists = (is_function_in_scope id) in
        (* Check that function arguments match that of function declaration *)
        let f_info = (get_function_info id env) in
        let f_arg_types = f_info.function_args in 
          let check_args expected_arg_types f_args = List.map2 same_types expected_arg_types f_args in
          check_args f_arg_types (List.map infer_type f_info.function_args)
        in
        in Sast.Function_Call(id,(convert_list convert_to_c_expression e_list)),env
      | Ast.String_Literal(s) -> Sast.String_Literal(s),env
      | Ast.Integer_Literal(i) -> Sast.Integer_Literal(i),env
      | Ast.Boolean_Literal(b) -> Sast.Boolean_Literal(b),env
      | Ast.Floating_Point_Literal(f) -> Sast.Floating_Point_Literal(f),env
      | Ast.Array_Literal(e_list) -> 
          (* Check all elements of the array are the same type *)
          let type_list = (convert_list infer_type e_list) in 
          in same_types_list type_list in
          (* Get array dimensions and pass to sast *)
          let array_vtype = 
            let f expression = infer_type expression in
            Ast.Array(match_type (List.map f expr_list),(List.length expr_list)) 
          in
          let array_dim = get_array_dimensions array_vtype in
          Sast.Array_Literal((convert_list convert_to_c_expression e_list),array_dim),env
      | Ast.Identifier_Literal(id) -> 
          if(check_already_declared id) = true then raise Exceptions.Variable_not_found_in_scope
          else Sast.Identifier_Literal(id),env
      | Cast(vtype, e) -> Sast.Cast((convert_to_c_variable_type vtype),(convert_to_c_expression e)),env
      | Binop(e1,op,e2) -> 
          if(same_types (infer_type e1) (infer_type e2)) = false then raise Exceptions.Type_mismatch("Binop types don't match") 
          else
            (match op with 
              | And | Or | Not | Xor | -> 
                if(same_types (infer_type e1) Boolean) = false then raise Exceptions.Type_mismatch("Boolean operator not having a boolean type")
              | _ -> 
                if(same_types (infer_type e1) String ) = true then raise Exceptions.String_not_accepted_in_binop
                else Sast.Binop(convert_to_c_expression e1,convert_to_c_binop op, convert_to_c_expression e2),env
            )
      | Ast.Higher_Order_Function_Call(hof) -> 
       (* Check that function exists in environment *)
        let check_exists = (is_function_in_scope hof.kernel_function_name) in
        (* Check that function arguments match that of function declaration *)
        let f_info = (get_function_info hof.kernel_function_name env) in
        let expected_arg_types = f_info.function_args in 
        let get_array_types arr = 
            match arr with 
                | Ast.Array(t,n) -> t
                | _ -> raise Exceptions.Invalid_input_argument_to_map
        in
        let f_arg_types = get_array_types (List.map infer_type hof.input_arrays) in
        same_types_list f_arg_types expected_arg_types in
        (*Check that constants match those unknown variables in the defg*)
        let hof_call_constants_names = List.map (fun x -> fst(x)) hof.constants in
        let hof_constants_names = List.map (fun x -> fst(x)) f_info.constants in 
        List.map2 (fun x,y -> x = y) hof_call_constants_names hof_constants_names in
        (*Add the c map function to the environment*)
        let c_map_name = "map" ^ string_of_int generate_map_c_function_name() in
        
        (*Add the global void map function to the environment*)
        (*Transform into Sast function call*) 
        in Sast.FunctionCall(c_map_name

let rec convert_to_ptx_expression e = 
  match e with 
    | Ast.Binop(e1,o,e2) ->
      convert_expression_to_ptx(e1) ^ 
      convert_expression_To_ptx(e2) ^ 
      convert_operator(o)
    | Integer_Literal -> Sast.Constant_Int(int)
    | 

let convert_to_c_variable_statement vstmt env = 
    match vstmt with 
      | Ast.Declaration(vdecl) -> (* Check that it isn't already declared *) 
            let c_vdecl, new_env = convert_to_c_vdecl vdecl env in
            Sast.Declaration(c_vdecl),new_env
      | Ast.Initialization(vdecl,e) ->
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
            let check_types = if same_types((get_variable_type id env) (infer_type e2) = false then (raise Exceptions.Type_mismatch("Incompatible types for assignment")) else () in
            (* Convert to sast representations *)
            let c_e1, env1 = convert_to_c_expression e env in 
            let c_e2, env2 = convert_to_c_expression e env1 in 
            Sast.Assignment(c_e1, c_e2), env2

let convert_to_ptx_variable_statement vstmt env =
    match vstmt with
        | Ast.Declaration(vdecl) ->
            
        
(* Converts global vstmt list into c vstmt list *)
let convert_to_c_variable_statement_list vstmt_list c_vstmt_list env = 
    (vstmt_list c_vstmt_list env)
     match vstmt_list with 
      | [] -> (c_vstmt_list,env)
      | hd::tl -> 
          let c_vstmt, new_env = convert_to_c_variable_statement hd env in
          convert_to_c_variable_statement_list tl (List.rev(hd::List.rev(c_vstmt_list)))

(* Converts a list of function declarations to ptx and c functions *)
let convert_fdecl_list fdecl_list ptx_fdecl_list c_fdecl_list env = 
    (fdecl_list ptx_fdecl_list c_fdecl_list env)
     match fdecl_list with 
      | [] -> (ptx_fdecl_list,c_fdecl_list,env)
      | hd::tl -> 
        (match hd.is_kernel_function with
          | true -> 
              let ptx_fdecl, new_env = convert_to_ptx_fdecl hd env in
              convert_fdecl_list tl List.rev(ptx_fdecl::List.rev(ptx_fdecl_list)) c_fdecl_list new_env
          | false ->
              let c_fdecl, new_env = convert_to_c_fdecl hd env in 
              convert_fdecl_list tl ptx_fdecl_list List.rev(c_fdecl::List.rev(c_fdecl_list)) new_env
        )

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

