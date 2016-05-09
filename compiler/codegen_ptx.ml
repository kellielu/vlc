open Sast
(* For sprintf *)
open Printf
(*------------------------------------------------------------ KERNEL CODE GENERATION ------------------------------------------------------------*)
(* 
let generate_kernel_fdecl kernel_f  =
  Environment.combine  [
    Generator(generate_variable_type kernel_f.kernel_r_type);
    Verbatim(" ");
    Generator(generate_id kernel_f.kernel_name);
    Verbatim("(");
    Generator(generate_parameter_list kernel_f.kernel_params);
    Verbatim("){\n");
    Generator(generate_statement_list kernel_f.kernel_body);
    Verbatim("}\n");
  ]

let rec generate_nonempty_kernel_fdecl_list kernel_fdecl_list  =
  match kernel_fdecl_list with
    | kernel_fdecl :: [] -> Environment.combine  [Generator(generate_kernel_fdecl kernel_fdecl)]
    | kernel_fdecl :: tail ->
      Environment.combine  [
        Generator(generate_kernel_fdecl kernel_fdecl);
        Verbatim("\n\n");
        Generator(generate_nonempty_kernel_fdecl_list tail)
      ]
    | [] -> raise (Empty_kernel_fdecl_list)
and generate_kernel_fdecl_list kernel_fdecl_list  =
  match kernel_fdecl_list with
    | [] -> Environment.combine  [Verbatim("")]
    | decl :: tail -> Environment.combine  [Generator(generate_nonempty_kernel_fdecl_list kernel_fdecl_list)]

 *)



(*-------------------------------------Duplicated in codegen_c-------------------------------------*)

(* Generate id *)
let generate_id id  = 
  sprintf "%s" (Utils.idtos(id))
(* Calls generate_func for every element of the list and concatenates results with specified concat symbol
   Used if you need to generate a list of something - e.x. list of statements, list of params *)
let generate_list generate_func concat mylist = 
  let list_string = String.concat concat (List.map generate_func mylist) in
  sprintf "%s" list_string

(*--------------------------------------------------------------------------*)
let generate_ptx_literal literal = 
  let l = match literal with
    | Ptx_signed_int(i) -> string_of_int(i)
    | Ptx_signed_float(f) -> string_of_float(f)
  in 
  sprintf "%s" l

let generate_ptx_binary_operator operator = 
  let op = match operator with
    | Ptx_Add -> "add"
    | Ptx_Subtract -> "sub"
    | Ptx_Multiply -> "mul"
    | Ptx_Divide -> "div"
    | Ptx_Modulo -> "rem"
    | Ptx_Equal -> "setp.eq"
    | Ptx_Not_Equal -> "setp.ne"
    | Ptx_Greater_Than -> "setp.gt"
    | Ptx_Less_Than -> "setp.lt"
    | Ptx_Greater_Than_Equal -> "setp.ge"
    | Ptx_Less_Than_Equal -> "setp.le"
    | Ptx_And -> "and"
    | Ptx_Or -> "or"
    | Ptx_Xor -> "xor"
    | Ptx_Bitshift_Right -> "shr"
    | Ptx_Bitshift_Left -> "shl"
  in
  sprintf "%s" op

let generate_ptx_unary_operator operator =
  let op = match operator with
    | Ptx_Not -> "not"
    | Ptx_Negate -> "neg"
    | Ptx_Plus_Plus -> raise Exceptions.PTXCREMENT_GENERATED_ERROR
    | Ptx_Minus_Minus -> raise Exceptions.PTXCREMENT_GENERATED_ERROR
  in
  sprintf "%s" op

let generate_ptx_data_type data_type = 
  let t = match data_type with
  (*
    | U8 -> ".u8"
    | U16 -> ".u16"
    | U32 -> ".u32"
    | U64 -> ".u64"
    | S8 -> ".s8"
    | S16 -> ".s16"
  *)
    | S32 -> ".s32"
    | Pred -> ".red"
(*     | S64 -> ".s64" *)
    | F32 -> ".f32"
  in
  sprintf "%s" t

let generate_ptx_state_space space =
  let s = match space with
    | Global -> ".global"
    | Local -> ".local"
    | Shared -> ".shared"
    | State_undefined -> ""
    | Register_state -> ".reg"
    | Constant -> ".const"
    | Param -> ".param"
  in
  sprintf "%s" s

let generate_ptx_variable variable = 
  let v = match variable with
    | Parameterized_variable_register(id, i) -> "%" ^ (generate_id(id)) ^ "<" ^
      string_of_int(i) ^ ">"
    | Variable_register(id, i) -> "%" ^ (generate_id(id)) ^ string_of_int(i)
    | Constant_int(i) -> string_of_int(i)
    | Constant_float(f) -> string_of_float(f)
    | Variable_array(id, i) -> (generate_id(id)) ^ "[" ^ string_of_int(i) ^ "]"
    | Variable_array_initialized(id, l_list) -> (generate_id(id)) ^ "[] = { " ^
      (generate_list generate_ptx_literal ", " l_list) ^ "}"
    | Ptx_Variable(id) -> generate_id(id)
    | Ptx_Variable_initialized(id, l) -> generate_id(id) ^ " = " ^ generate_ptx_literal(l)
  in
  sprintf "%s" v

let generate_ptx_vdecl declaration =
  let v = match declaration with
    | Ptx_Vdecl (space, dtype, v) -> generate_ptx_state_space(space) ^ " " ^
      (generate_ptx_data_type(dtype)) ^ " " ^ (generate_ptx_variable(v)) ^ ";"
  in
  sprintf "%s" v

let rec generate_ptx_expression expression =
  let e = match expression with
    | Ptx_Binop(o, t, v1, v2, v3) -> generate_ptx_binary_operator(o) ^ generate_ptx_data_type(t) 
        ^ "     " ^ generate_ptx_variable(v1) ^ ", " ^ generate_ptx_variable(v2) ^ ", " 
        ^ generate_ptx_variable(v3) ^ ";"
    | Ptx_Unop(o, t, v1, v2) -> 
        let unop = match o with 
            | Ptx_Not -> generate_ptx_unary_operator(o) ^ 
                generate_ptx_data_type(t) ^ "     " ^ generate_ptx_variable(v1) 
                ^ ", " ^ generate_ptx_variable(v2) ^ ";"
            | Ptx_Negate -> generate_ptx_unary_operator(o) ^ 
                generate_ptx_data_type(t) ^ "     " ^ generate_ptx_variable(v1) 
                ^ ", " ^ generate_ptx_variable(v2) ^ ";"
            | Ptx_Plus_Plus -> "add" ^ generate_ptx_data_type(t) ^ "     " ^
                generate_ptx_variable(v1) ^ ", " ^ generate_ptx_variable(v2) ^
                ", 1;"
            | Ptx_Minus_Minus -> "sub" ^ generate_ptx_data_type(t) ^ "     " ^
                generate_ptx_variable(v1) ^ ", " ^ generate_ptx_variable(v2) ^
                ", 1;"
        in unop
    | Ptx_vdecl(v) -> generate_ptx_vdecl(v)
    | Ptx_Move(d, v1, v2) -> "mov" ^ generate_ptx_data_type(d) ^ "     " ^
      generate_ptx_variable(v1) ^ generate_ptx_variable(v2) ^ ";"
    | Ptx_Load(ss, d, v1, v2) -> "ld" ^ generate_ptx_state_space(ss) ^ generate_ptx_data_type(d)
      ^ "     " ^ generate_ptx_variable(v1) ^ ",[" ^ generate_ptx_variable(v2) ^ "];"
    | Ptx_Store(ss, d, v1, v2) -> "st" ^ generate_ptx_state_space(ss) ^ generate_ptx_data_type(d)
      ^ "     " ^ "[" ^ generate_ptx_variable(v1) ^ "]," ^ generate_ptx_variable(v2) ^ ";"
    | Ptx_Branch(sub) -> "bra " ^ generate_id(sub) ^ ";"
    | Predicated_statement(v, s) -> "@" ^ generate_ptx_variable(v) ^ " " ^
      generate_ptx_expression(s)
    | Ptx_Convert (d1, d2, v1, v2) -> "cvt" ^ generate_ptx_data_type(d1) ^
      generate_ptx_data_type(d2) ^ " " ^ generate_ptx_variable(v1) ^ ", " ^
      generate_ptx_variable(v2) ^ ";"
    | Ptx_Call(v1, id, vlist) -> "call " ^ generate_ptx_variable(v1) ^ " " ^
      generate_id(id) ^ " " ^ (generate_list generate_ptx_variable " " vlist) ^ ";"
    | Ptx_Empty_Call(id, vlist) -> "call " ^ generate_id(id) 
      ^ (generate_list generate_ptx_variable " " vlist) ^ ";"
    | Ptx_Return -> "ret;"
  in
  sprintf "%s" e

let generate_ptx_subroutine subroutine = 
  let s =
  generate_id subroutine.routine_name ^ ": \n" ^
  generate_list generate_ptx_expression "\n" subroutine.routine_expressions
  in
  sprintf "%s" s

let generate_ptx_statement statement =
  let s = match statement with
    | Ptx_expression(e) -> generate_ptx_expression(e)
    | Ptx_subroutine(s) -> generate_ptx_subroutine(s)
  in 
  sprintf "%s" s

let generate_ptx_function_type fun_type =
  let t = match fun_type with
    | Global_func -> ".entry"
    | Device_func -> ".func"
  in
  sprintf "%s" t

let generate_ptx_pdecl p = 
  let pdecl = 
    ".param " ^ (generate_ptx_data_type(p.ptx_parameter_data_type)) ^ " " ^
    (generate_ptx_state_space(p.ptx_parameter_state_space)) ^ " " ^
    (generate_id(p.ptx_parameter_name))
  in
  sprintf "%s" pdecl

(* Writing out to PTX file *)
let write_ptx filename ptx_string = 
  let file = open_out (filename ^ ".ptx") in 
  fprintf file "%s" ptx_string

(* Generates the ptx function string *)
let generate_ptx_function f =
  let ptx_function_body = 
    ".visible " ^ generate_ptx_function_type(f.ptx_fdecl_type) ^ (generate_id(f.ptx_fdecl_name)) ^ "(" 
    ^ (generate_list generate_ptx_pdecl "," f.ptx_fdecl_params) ^ ")\n" ^ 
    "{" ^ 
(*     (generate_list generate_ptx_vdecl "\n" f.register_decls ) ^ "\n" ^  *)
    (generate_list generate_ptx_statement "\n" f.ptx_fdecl_body) ^ 
    "}"
  in
  let ptx_function_string = sprintf " \
  //Generated by VLC\n\
  .version 4.1\n\
  .target sm_20\n\
  .address_size 64\n\
  %s" ptx_function_body 
  in 
  sprintf "%s" ptx_function_string

(* Generates global ptx functions *)
let generate_ptx_hof_function hof = 
  match Utils.idtos(hof.higher_order_function_type) with 
    | "map" -> 
    let ptx_function_string = ".visible .entry " ^ Utils.idtos(hof.higher_order_function_name)^ " ( " ^ 
    ") "
    in sprintf "%s" ptx_function_string
(*     | "reduce" -> *)
    | _ -> ""
(* Main function for generating all ptx files*)
let generate_ptx_function_files program = 
  (let ptx_hof_function_list = Utils.quad_trd(program) in
  (* Generates global ptx functions*)
  let rec generate_ptx_hof_files ptx_hof_func_list = 
    match ptx_hof_func_list with 
      | [] -> ()
      | hd::tl -> 
        write_ptx (Utils.idtos(hd.higher_order_function_name)) (generate_ptx_hof_function hd);
        generate_ptx_hof_files tl
  in
  generate_ptx_hof_files ptx_hof_function_list);
  (* Generates device ptx functions*)
  let ptx_function_list = Utils.quad_snd program in
  let rec generate_ptx_files ptx_func_list =
  	match ptx_func_list with
  		| [] -> ()
  		| hd::tl ->
  			write_ptx (Utils.idtos(hd.ptx_fdecl_name)) (generate_ptx_function hd);
  			generate_ptx_files tl
  in generate_ptx_files ptx_function_list


