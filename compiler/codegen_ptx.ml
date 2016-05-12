open Sast
(* For sprintf *)
open Printf
(*------------------------------------------------------------ KERNEL CODE GENERATION ------------------------------------------------------------*)

(* Calls generate_func for every element of the list and concatenates results with specified concat symbol
   Used if you need to generate a list of something - e.x. list of statements, list of params *)
let generate_list generate_func concat mylist = 
  let list_string = String.concat concat (List.map generate_func mylist) in
  sprintf "%s" list_string

(* Generates ptx id*)
let generate_id id = 
    let id_string = 
      if(id.write_reg = true) then id.reg_name ^ string_of_int id.reg_num
      else Utils.idtos(id.var_name)
    in sprintf "%s" id_string

let generate_ptx_data_type data_type = 
  let t = match data_type with
    | U64 -> ".u64"
 (*    | U8 -> ".u8"
    | U16 -> ".u16"
    | U32 -> ".u32" *)
(*     | S8 -> ".s8"
    | S16 -> ".s16" *)
  
    | S32 -> ".s32"
    | Pred -> ".pred"
(*     | S64 -> ".s64" *)
    | F32 -> ".f32"
    | Ptx_Void -> ""
  in
  sprintf "%s" t

let rec generate_ptx_variable_type vtype = 
  let t = match vtype with
    | Ptx_Primitive(p) -> generate_ptx_data_type p
    | Ptx_Array(d, i) -> generate_ptx_variable_type d 
    | _ -> ""
  in
  sprintf "%s" t


let rec generate_ptx_literal literal = 
  let l = match literal with
    | Ptx_Signed_Integer(i) -> string_of_int(i)
    | Ptx_Signed_Float(f) -> string_of_float(f)
    | Ptx_Predicate(p) -> string_of_int p
    | Ptx_Identifier_Literal(id) -> generate_id id
    | Ptx_Array_Literal(l_list) -> "{" ^ generate_list generate_ptx_literal "," l_list ^ "}"
    | Ptx_Array_Access(l,l_list) -> generate_ptx_literal l ^ "[" ^ generate_list generate_ptx_literal "][" l_list ^ "]"
  in 
  sprintf "%s" l

let generate_ptx_unary_operator operator =
  let op = match operator with
    | Ptx_Not -> "not"
    | Ptx_Negate -> "neg"
    | Ptx_Plus_Plus -> ""
    | Ptx_Minus_Minus -> ""
  in
  sprintf "%s" op

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
    | Ptx_Bitwise_Or -> "|"
    | Ptx_Bitwise_And -> "&"
  in
  sprintf "%s" op


let generate_ptx_state_space space =
  let s = match space with
    | Global -> ".global"
    | Local -> ".local"
    | Shared -> ".shared"
    | State_Undefined -> ""
    | Constant -> ".const"
    | Param -> ".param"
  in
  sprintf "%s" s

let generate_ptx_register_declaration rdecl = 
  let reg_string = 
    ".reg " ^ generate_ptx_data_type rdecl.reg_type ^ " %" ^ rdecl.reg_id ^ "<" ^ string_of_int rdecl.num_registers ^">;"
  in sprintf "%s" reg_string

let generate_ptx_param vdecl = 
  let param_string = 
    match vdecl with 
      | Ptx_Vdecl(ss,vtype,id)-> 
          (match vtype with
            | Ptx_Primitive(p) -> ".param" ^ generate_ptx_variable_type vtype ^ " " ^
                                  generate_ptx_state_space ss ^ " " ^
                                  generate_id id
            | Ptx_Array(vtype,size) -> ".param .u64 " ^ generate_id id  
          )
    in sprintf "%s" param_string

let rec get_array_dimensions vtype dimensions = 
  match vtype with
  | Sast.Ptx_Array(t,n) -> 
      get_array_dimensions t (List.rev(n::List.rev(dimensions)))
  | Sast.Ptx_Primitive(p) -> dimensions
  | _ -> []

let generate_ptx_vdecl vdecl = 
  let vdecl_string = 
    match vdecl with 
      | Ptx_Vdecl (ss, vtype, id) -> 
        (match vtype with 
            | Ptx_Primitive(p) -> generate_ptx_state_space ss ^ " " ^ generate_ptx_data_type p ^ generate_id id 
            | Ptx_Array(dtype,size) -> 
                    generate_ptx_state_space ss ^ " " ^ 
                    generate_ptx_variable_type dtype ^ " " ^ 
                    "[" ^ generate_list string_of_int "][" size ^ "] " ^  
                    generate_id id 
        )
      in sprintf "%s" vdecl_string



let rec generate_ptx_statement statement =
  let s = match statement with
    | Ptx_Load(ss, d, id1, id2) -> "ld" ^ generate_ptx_state_space(ss) ^ generate_ptx_variable_type(d)
      ^ "     " ^ generate_ptx_literal id1 ^ ",[" ^ generate_ptx_literal id2 ^ "];\n"
    | Ptx_Store(ss, d, id1, id2) -> "st" ^ generate_ptx_state_space(ss) ^ generate_ptx_variable_type(d)
      ^ "     " ^ "[" ^ generate_ptx_literal id1 ^ "]," ^ generate_ptx_literal id2 ^ ";\n"
    | Ptx_Move(d, id1, id2) -> "mov" ^ generate_ptx_variable_type(d) ^ "     " ^
      generate_ptx_literal id1 ^ ", " ^ generate_ptx_literal id2 ^ ";\n"
    | Ptx_Binop(o, t, id1, id2, id3) -> generate_ptx_binary_operator(o) ^ generate_ptx_variable_type(t) 
        ^ "     " ^ generate_ptx_literal id1 ^ ", " ^ generate_ptx_literal id2 ^ ", " 
        ^ generate_ptx_literal id3 ^ ";\n"
    | Ptx_Unop(o, t, id1, id2) -> 
        let unop = match o with 
            | Ptx_Not -> generate_ptx_unary_operator(o) ^ 
                generate_ptx_variable_type(t) ^ "     " ^ generate_ptx_literal(id1) 
                ^ ", " ^ generate_ptx_literal id2 ^ ";\n"
            | Ptx_Negate -> generate_ptx_unary_operator(o) ^ 
                generate_ptx_variable_type(t) ^ "     " ^ generate_ptx_literal(id1)  
                ^ ", " ^ generate_ptx_literal id2  ^ ";\n"
            (* | Ptx_Plus_Plus -> "add" ^ generate_ptx_variable_type(t) ^ "     " ^
                generate_id(id1)  ^ ", " ^ generate_id id2  ^
                ", 1;\n"
            | Ptx_Minus_Minus -> "sub" ^ generate_ptx_variable_type(t) ^ "     " ^
                generate_id(id1)  ^ ", " ^ generate_id id2  ^
                ", 1;\n" *)
        in unop
    | Ptx_Call(return_val, fname, arglist) -> "call " ^ generate_ptx_literal return_val ^ " " ^
        Utils.idtos(fname) ^ " (" ^ (generate_list generate_ptx_literal "," arglist)^")" ^ ";\n"
    | Ptx_Empty_Call(id, vlist) -> "call " ^ Utils.idtos(id) ^"(" ^(generate_list generate_ptx_literal "," vlist)^")" ^ ";\n"
    | Ptx_Variable_Declaration(vdecl) -> generate_ptx_vdecl vdecl ^ ";\n"
    | Ptx_Branch(id,sub) -> "@" ^ generate_ptx_literal id ^" bra " ^ Utils.idtos(sub) ^ ";\n"
    | Ptx_Block(stmt_list) -> generate_list generate_ptx_statement "" stmt_list
    | Ptx_Subroutine(id,stmt_list)-> Utils.idtos id ^ ": \n" ^ generate_list generate_ptx_statement "\n" stmt_list
    | Ptx_Return_Void -> "ret;\n"
  (*   | Ptx_Cast (d1, d2, v1, v2) -> "cvt" ^ generate_ptx_variable_type(d1) ^
        generate_ptx_variable_type(d2) ^ " " ^ generate_ptx_(v1) ^ ", " ^
        generate_ptx_expression(v2) ^ ";\n" *)
    | Ptx_Empty -> ""
    in sprintf "%s" s


let generate_ptx_function_type fun_type =
  let t = match fun_type with
    | Global_Function -> ".entry"
    | Device_Function -> ".func"
  in
  sprintf "%s" t

let generate_ld_statement vdecl =
  let ld_string =  
    match vdecl with 
      | Ptx_Vdecl(ss,vtype,id) -> "ld" ^ (generate_ptx_state_space ss) ^ (generate_ptx_variable_type vtype) ^ 
                                  " %" ^ id.reg_name ^  string_of_int id.reg_num^ "," ^ 
                                  "[" ^ Utils.idtos id.var_name ^ "];"
  in sprintf "%s" ld_string                            


(* Writing out to PTX file *)
let write_ptx filename ptx_string = 
  let file = open_out (filename ^ ".ptx") in 
  fprintf file "%s" ptx_string

let generate_load_statement vdecl = 
    (match vdecl with 
      | Sast.Ptx_Vdecl(ss,vtype,id) -> 
        (match vtype with 
          | Sast.Ptx_Primitive(p) -> "ld" ^ (generate_ptx_state_space ss) ^ (generate_ptx_data_type p ) ^ 
                                  " " ^ id.reg_name ^ string_of_int id.reg_num ^ "," ^ Utils.idtos id.var_name 
          | Sast.Ptx_Array(t,n) -> "ld" ^ (generate_ptx_state_space ss) ^ (generate_ptx_variable_type vtype) ^ 
                                  " %" ^ id.reg_name ^  string_of_int id.reg_num^ "," ^ 
                                  "[" ^ Utils.idtos id.var_name ^ "];"
        )
    )

(* Generates the ptx function string *)
let generate_ptx_function f =
  let ptx_function_body = 
    ".visible " ^ generate_ptx_function_type(f.ptx_fdecl_type) ^ 
    " (" ^ generate_ptx_param f.ptx_fdecl_return_param ^ ") " ^ (Utils.idtos f.ptx_fdecl_name) ^ 
    "(" ^ (generate_list generate_ptx_param "," f.ptx_fdecl_input_params) ^ ")\n\n" ^ 
    "{\n" ^ 
      (generate_list generate_ptx_register_declaration "\n" f.register_decls ) ^ "\n\n\n" ^ 
      (generate_list generate_load_statement           "\n" (f.ptx_fdecl_return_param::f.ptx_fdecl_input_params)) ^ "\n\n" ^
      (generate_list generate_ptx_statement "" f.ptx_fdecl_body) ^ 
    "}\n\n"
  in
  let ptx_function_string = sprintf " \
  //Generated by VLC\n\
  .version 4.1\n\
  .target sm_20\n\
  .address_size 64\n\
  %s" ptx_function_body 
  in 
  sprintf "%s" ptx_function_string


let rec range i j = if i > j then [] else i :: (range (i+1) j)

let generate_array_load vdecl a_start nr_start type_start num_arr_param = 
      let cvt_string = 
        match vdecl with 
        | Ptx_Vdecl(ss,vtype,id) -> 
            "cvta.to.global.64    %ptr" ^ string_of_int nr_start ^  ",%ptr" ^ string_of_int a_start ^ ";\n" ^ 
            "add.s64              %ptr" ^ string_of_int (nr_start +1) ^ ",%ptr" ^ string_of_int nr_start ^ ", %ptr" ^ string_of_int (num_arr_param+1) ^ ";\n" ^ 
            "ld.global" ^ (generate_ptx_variable_type vtype) ^ "        %vlc" ^ string_of_int type_start ^ ",[%ptr"^ string_of_int nr_start^ "];\n\n"
      in sprintf "%s" cvt_string

let generate_array_rtype vdecl a_start nr_start type_start num_arr_param = 
  let cvt_string = 
        match vdecl with 
        | Ptx_Vdecl(ss,vtype,id) -> 
            "cvta.to.global.64    %ptr" ^ string_of_int nr_start ^  ",%ptr" ^ string_of_int a_start ^ ";\n" ^ 
            "add.s64              %ptr" ^ string_of_int (nr_start +1) ^ ",%ptr" ^ string_of_int nr_start ^ ", %ptr" ^ string_of_int (num_arr_param+1) ^ ";\n" ^ 
            "ld.global" ^ (generate_ptx_variable_type vtype) ^ "        %rtype" ^ string_of_int type_start ^ ",[%ptr"^ string_of_int nr_start^ "];\n\n"
      in sprintf "%s" cvt_string

let rec generate_load_and_access_elements vdecl_list array_start nex_reg_start type_start num_arr_param final_string= 
   let s =  match vdecl_list with 
      | [] -> final_string 
      | hd::tl -> 
        let statement = generate_array_load hd array_start nex_reg_start type_start num_arr_param in
        generate_load_and_access_elements tl (array_start+1) (nex_reg_start+3) (type_start + 1) num_arr_param (final_string ^ statement)
    in sprintf "%s" s

let rec generate_call_params num_arr_param final_string= 
    let generate_call_param num = ("%vlc" ^ string_of_int num) in 
    (match num_arr_param with
      | 1 -> final_string ^ (generate_call_param num_arr_param)
      | _ -> generate_call_params (num_arr_param -1) (final_string ^ (generate_call_param num_arr_param ^ ",")))

(* Generates global ptx functions *)
let generate_ptx_hof_function hof = 
  let num_arr_param = (List.length hof.ptx_input_arrays_info) + 1 in 
  let type_start = 1 in
  let return_vtype = match hof.ptx_return_array_info with Ptx_Vdecl(ss,vtype,id) -> vtype in
  (match Utils.idtos hof.ptx_higher_order_function_type with 
      | "map" -> 
          (let ptx_function_string = 
              ".visible .entry " ^ Utils.idtos(hof.ptx_higher_order_function_name)^ " ( \n" ^ 
                  generate_list generate_ptx_param ",\n" hof.ptx_input_arrays_info ^"," ^"\n\n" ^  
                  generate_ptx_param hof.ptx_return_array_info ^ "," ^ "\n\n" ^ 
                  ".param .u32 ARRAY_LENGTH" ^ 
                  ") {\n" ^ 
                  (* Load register declarations *)
                  generate_list generate_ptx_register_declaration "\n" hof.ptx_register_decls ^ "\n\n" ^ 
                  generate_list generate_ld_statement "\n" (List.rev (hof.ptx_return_array_info::(List.rev hof.ptx_input_arrays_info))) ^ "\n\n" ^ 
                  "ld.param.u32 %asize1, [ARRAY_LENGTH];\n" ^ 
                  (* Move tid *)
                  "mov.u32 %mytid1, %tid.x;\n" ^ 
                  "setp.ge.s32 %pred1, %mytid1, %asize1\n" ^ 
                  "@%pred1 bra RETURN;\n" ^ 
                  "mul.wide.s32 %ptr" ^ string_of_int (num_arr_param+1) ^ ",%mytid1, 4;\n\n" ^ 
                  (* Load input arrays *)
                  generate_array_rtype hof.ptx_return_array_info 1 (num_arr_param +2) 1 num_arr_param ^"\n"^
                  generate_load_and_access_elements hof.ptx_input_arrays_info (2)  (num_arr_param+5) type_start num_arr_param "" ^ "\n" ^ 
                  "call " ^ "(%rtype1)," ^ (Utils.idtos hof.ptx_applied_kernel_function) ^ ",(" ^ generate_call_params (num_arr_param-1) "" ^ ");\n" ^ 
                  "st.global" ^ (generate_ptx_variable_type return_vtype) ^ "  [%ptr"^ string_of_int (num_arr_param+3) ^"],  %rtype1" ^ ";\n\n" ^ 
                  "RETURN:\n" ^ 
                  "\t ret;\n" ^  
                  "}\n\n\n"    
          in sprintf " \
          //Generated by VLC\n\
          .version 4.1\n\
          .target sm_20\n\
          .address_size 64\n\
          %s\n\n" ptx_function_string)
  (*     | "reduce" -> *)
      | _ -> "")

(* Main function for generating all ptx files*)
let generate_ptx_function_files program = 
  (let ptx_hof_function_list = Utils.quint_trd(program) in
  (* Generates global ptx functions*)
  let rec generate_ptx_hof_files ptx_hof_func_list = 
    match ptx_hof_func_list with 
      | [] -> ()
      | hd::tl -> 
        let ptx_hof_function_string = generate_ptx_hof_function hd in
        write_ptx (Utils.idtos(hd.ptx_higher_order_function_name)) ptx_hof_function_string;
        print_endline ptx_hof_function_string;
        generate_ptx_hof_files tl
  in
  generate_ptx_hof_files ptx_hof_function_list);
  (* Generates device ptx functions*)
  let ptx_function_list = Utils.quint_snd program in
  let rec generate_ptx_files ptx_func_list =
  	match ptx_func_list with
  		| [] -> ()
  		| hd::tl ->
        let ptx_function_string = (generate_ptx_function hd) in
  			write_ptx (Utils.idtos(hd.ptx_fdecl_name)) ptx_function_string;
        print_endline ptx_function_string;
  			generate_ptx_files tl
  in generate_ptx_files ptx_function_list


