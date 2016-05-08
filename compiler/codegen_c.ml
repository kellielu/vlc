open Sast
(* open Exceptions *)
(* open Codegen_ptx *) 

(* For sprintf *)
open Printf

(*-------------------------------------Generating Functions-------------------------------------*)

(* Calls generate_func for every element of the list and concatenates results with specified concat symbol
   Used if you need to generate a list of something - e.x. list of statements, list of params *)
let generate_list generate_func concat mylist = 
  let list_string = String.concat concat (List.map generate_func mylist) in
  sprintf "%s" list_string

(* Generate operators *)
let generate_binary_operator operator  =
  let op = match operator with
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Modulo -> "%"
    | And -> "&&"
    | Or -> "||"
    | Xor -> "^"
    | Equal -> "=="
    | Not_Equal -> "!="
    | Greater_Than -> ">"
    | Less_Than -> "<"
    | Greater_Than_Equal -> ">=" 
    | Less_Than_Equal -> "<="
    | Bitshift_Right -> ">>"
    | Bitshift_Left -> "<<"
  in
  sprintf "%s" op

let generate_unary_operator operator = 
  let op = match operator with 
    | Not -> "!"
    | Negate -> "-"
    | Plus_Plus -> "++"
    | Minus_Minus -> "--"
  in sprintf "%s" op

(* Generate data type*)
let generate_data_type dtype = 
    let data_type = match dtype with 
        | String -> "char *"
        | Unsigned_Byte -> "unsigned char"
        | Byte -> "signed char"
        | Unsigned_Integer -> "unsigned int"
        | Integer -> "int"
        | Unsigned_Long -> "unsigned long"
        | Long -> "long"
        | Float -> "float"
        | Double -> "double"
        | Boolean -> "bool"
        | Void -> "void"
    in sprintf "%s" data_type

(* Generate variable type *)
let rec generate_variable_type variable_type  =
  let vtype = match variable_type with
    | Primitive(p) -> generate_data_type p
    | Array(t,n) -> 
      (match t with
        | Array(t1,n1) -> generate_variable_type t1 
        | Primitive(p) -> generate_data_type p)
  in sprintf "%s" vtype

(* Generate id *)
let generate_id id  = 
  let id_string = Utils.idtos(id) in 
  sprintf "%s" id_string

let generate_function_id id = 
let id_string = Utils.idtos(id)
  match id_string with
    | "print" -> sprintf "printf"
    | _ as identifier -> sprintf identifier

 (* Generates CUDA device pointer *)
 let generate_device_ptr ktype = 
    sprintf "CUdeviceptr " ^ ktype.kernel_name ^ ";"

 (* Generates CUDA memory allocation from host to device *)
 (* Fill in with VLC_Array*) 
 let generate_mem_alloc_statement_host_to_device ktype arr_length= 
    sprintf "checkCudaErrors(cuMemAlloc(&" ^ Utils.idtos(ktype.kernel_name) ^ ", sizeof(" ^ (generate_variable_type ktype.variable_type) ^ ")*" ^ string_of_int arr_length ^ "));"

 let generate_mem_alloc_host_to_device fcall = 
    let rec create_list mylist length element = if length > 0 then create_list (element::mylist) (length-1) element else mylist in
    let mem_alloc_string = 
    String.concat "\n" (List.map2 generate_mem_alloc_statement_host_to_device fcall.input_arrays_info (create_list [] (List.length fcall.input_arrays_info) fcall.array_length)) in
    sprintf "%s" mem_alloc_string

(* Generates CUDA copying from host to device*)
 let generate_mem_cpy_statement_host_to_device ktype = 
    let vtype = ( match ktype.variable_type with 
                    | Array(t,n) -> 

                    | Primitive(t) -> 
                ) 
    in 
    let mem_cpy_string  = 
      "checkCudaErrors(cuMemcpyHtoD("^ Utils.idtos(arr_info.kernel_name) ^", " ^ Utils.idtos(arr_info.host_name) ^ ", sizeof(" ^ (generate_variable_type arr_info.variable_type) ^ ")*" ^ string_of_int arr_length ^ "));\n" in
    sprintf "%s" mem_cpy_string

 let generate_mem_cpy_host_to_device fcall = 
    let rec create_list mylist length element = if length > 0 then create_list (element::mylist) (length-1) element else mylist in
    let mem_cpy_string = String.concat "\n" (List.map2 generate_mem_cpy_statement_host_to_device fcall.input_arrays_info (create_list [] (List.length fcall.input_arrays_info) fcall.array_length)) in
    sprintf "%s" mem_cpy_string

(* Generates CUDA statement for kernel params *)
 let generate_kernel_params arr_info = 
    let rec get_kernel_names a_info_list name_list = 
      match a_info_list with 
        | [] -> name_list 
        | hd::tl -> get_kernel_names tl (hd.kernel_name::name_list) 
    in
    let kernel_names = (get_kernel_names arr_info []) in 
    let kernel_param_string = generate_list generate_id ", &" kernel_names in 
    sprintf "void *KernelParams[] = { &" ^ kernel_param_string ^ "};"

(* Generate CUDA memory cleanup *)
let generate_mem_cleanup arr_info = 
    sprintf "checkCudaErrors(cuMemFree("^ Utils.idtos(arr_info.kernel_name) ^ "));"

(* Generates variable declaration statements *)
let generate_vdecl d  = 
   match d with 
  | Variable_Declaration(vtype,id) ->
      match vtype with
        | Array(t,n) ->
          let param_string = "VLC_Array<" ^ (generate_data_type t) ^"," ^ n ^ ">" ^ (generate_id id)  in 
          sprintf "%s" param_string
        | Primitive(p) ->
            let param_string = (generate_data_type p) ^ " " ^ (generate_id id) in 
            sprintf "%s" param_string
(*         | _ -> raise Exceptions.Unknown_variable_type
  | _ -> raise Exceptions.Unknown_type_of_vdecl *)

let generate_param d =
  match d with 
    | Variable_Declaration(vtype,id) ->
      match vtype with
        | Array(t,n) ->
          let param_string = "VLC_Array<" ^ (generate_data_type t) ^"," ^ n ^ ">" ^ (generate_id id) input_arrays_info in 
          sprintf "%s" param_string
        | Primitive(p) ->
          let param_string = (generate_data_type p) ^ " " ^ (generate_id id) in 
          sprintf "%s" param_string
(*         | _ -> raise Exceptions.Unknown_variable_type
    | _ -> raise Exceptions.Unknown_type_of_param *)

 (* Generate expressions - including higher order function calls - and constants *)
let rec generate_expression expression  =
  let expr = match expression with
    | Function_Call(id, expr_list) ->
        (generate_function_id id) ^ "(" ^ generate_list generate_expression "," expr_list ^ ")"
    | Higher_Order_Function_Call(fcall) -> generate_higher_order_function_call fcall
    | String_Literal(s) -> 
        "\"" ^ s ^ "\""
    | Integer_Literal(i) -> 
        string_of_int i
    | Boolean_Literal(b) -> 
        string_of_bool b
    | Floating_Point_Literal(f) ->
        string_of_float f
    | Array_Literal(e_list,int_list) -> "VLC_Array(int" ^ string_of_int (List.length int_list) ^ "," ^ (generate_list string_of_int "," int_list) ^ "," ^ (generate_list generate_expression "," e_list) ^ ")" 
    | Identifier_Literal(id) -> 
        (generate_id id)
    | Cast(vtype,e) ->
        "(" ^ (generate_variable_type vtype) ^ ")" ^ (generate_expression e)
    | Binop(e1, o, e2) -> 
        (generate_expression e1) ^ " " ^ (generate_binary_operator o) ^ " " ^ (generate_expression e2)
    | Unop(e,o) ->
        (match o with 
        | Not | Negate  -> (generate_unary_operator o) ^ (generate_expression e)
        | Plus_Plus | Minus_Minus -> (generate_expression e) ^ (generate_unary_operator o))
    | Array_Accessor(e,e_list) -> (generate_expression e) ^ "[" ^ (generate_list generate_expression "][" e_list) ^ "]"
    | Ternary(e1,e2,e3) -> "(" ^ (generate_expression e2) ^ ") ? " ^ (generate_expression e1) ^ ":" ^ (generate_expression e3)
  in sprintf "%s" expr
(* Generates CUDA statements that copy constants from host to gpu *)
(* and generate_constant_on_gpu const  = 
  let mem_alloc_constant_string = match const.variable_type with 
    | Primitive(vtype) ->
          generate_device_ptr (Utils.idtos(const.kernel_name)) ^ 
          generate_mem_alloc_statement_host_to_device const 1 ^ 
          generate_mem_cpy_statement_host_to_device const 1 
    | Array(vtype,length) ->
        "vlcarray fill"
(*     | _ -> raise Exceptions.Unknown_variable_type *)
  in 
  sprintf "%s" mem_alloc_constant_string *)

(* Generates host arguments from a c_kernel_variable_info *)
let generate_args ktype = 
  (generate_variable_type ktype.variable_type) ^ " " ^ (generate_id ktype.arg_name)

let generate_host_ptr ktype = 
  (generate_variable_type ktype.variable_type) ^ " " ^ ktype.host_name ^ ";"

(* (* Generates if statement for every constant. Necessary because constants maybe different types *)
let generate_if_statements_for_constants ktype_list length = 
    let constant_index = List.length ktype_list in
    match constant_index with 
      | length ->
        let ktype = List.nth ktype_list (length - constant_index) in
        "if(i=" ^ string_of_int index ^ "){" ^ 
            (generate_id ktype.host_name) ^  "=" ^ "va_args(constants," ^ (generate_variable_type ktype.variable_type) ^ ");" ^ 
            generate_mem_alloc_statement_host_to_device ktype ^ (* Cuda mem alloc *)
            generate_mem_cpy_statement_host_to_device ktype ^ (* Cuda mem copy *)
        "}"
      | 1 -> 
        let ktype = List.nth ktype_list (length - constant_index) in
        "else{" ^ 
            (generate_id ktype.host_name) ^  "=" ^ "va_args(constants," ^ (generate_variable_type ktype.variable_type) ^ ");" ^ 
            generate_mem_alloc_statement_host_to_device ktype ^ (* Cuda mem alloc *)
            generate_mem_cpy_statement_host_to_device ktype ^ (* Cuda mem copy *)
        "}"
      | _ ->
        let ktype = List.nth ktype_list (length - constant_index) in 
        "else if{" ^ 
            (generate_id ktype.host_name) ^  "=" ^ "va_args(constants," ^ (generate_variable_type ktype.variable_type) ^ ");" ^ 
            generate_mem_alloc_statement_host_to_device ktype ^ (* Cuda mem alloc *)
            generate_mem_cpy_statement_host_to_device ktype ^ (* Cuda mem copy *)
        "}" *)


(* Generates c function declaration for map  *)
let generate_higher_order_function_decl hofcall = 
    let higher_order_function_decl_string = 
      match Utils.idtos(fcall.higher_order_function_type) with
      | "map" -> "VLC_Array <" ^ (generate_variable_type hofcall.return_array_info.variable_type) ^ ">" ^ 
                hofcall.higher_order_function_name ^ "(...)" ^ "{\n" ^ 
                      "checkCudaErrors(cuCtxCreate(&context, 0, device));\n" ^ 
                      "std::ifstream t(\"" ^ Utils.idtos hofcall.applied_kernel_function ^ ".ptx\");\n" ^ 
                      "if (!t.is_open()) {\n" ^
                              " std::cerr << \"" ^ Utils.idtos hofcall.applied_kernel_function ^ ".ptx not found\n\";\n" ^
                              "return 1;\n" ^
                      "}\n" ^
                      "std::string " ^ Utils.idtos hofcall.applied_kernel_function ^ "_str" ^ "((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());\n" ^ 
                      "checkCudaErrors(cuModuleLoadDataEx(&cudaModule," ^ (Utils.idtos hofcall.applied_kernel_function) ^ "_str" ^ ", 0, 0, 0));\n" ^ 
                      "checkCudaErrors(cuModuleGetFunction(&function, cudaModule, \"" ^ (Utils.idtos hofcall.applied_kernel_function) ^ "_str" ^ "\"));\n" ^ 
                      "int num_constants = " ^ (List.length hofcall.constants) ^ ";\n" ^
                      "int num_input_arrays = " ^ (hofcall.array_length) ^ ";\n" ^ 
                      generate_list generate_host_ptr "\n" hofcall.constants ^ "\n" ^ 
                      generate_list generate_host_ptr "\n" hofcall.input_arrays_info ^ "\n" ^
                      generate_list generate_device_ptr "\n" hofcall.constants ^ "\n" ^  
                      generate_list generate_device_ptr "\n" hofcall.input_arrays_info ^ "\n" ^ 
                      generate_device_ptr hofcall.return_array_info ^ 
                      generate_list generate_mem_alloc_statement_host_to_device hof.constants ^
                      generate_list generate_mem_cpy_statement_host_to_device hof.constants ^
                      (* " va_list constants;\n" ^ 
                      " va_start(constants,num_constants)\n" ^
                      "for(int i = 0; i < num_constants; i++){\n" ^ 
                          (generate_if_statements_for_constants hofcall.constants (List.length hofcall.constants)) ^
                      "}\n" ^ 
                      " va_end(constants);\n" ^
                      "for(int j = 0; j < num_input_arrays; j++){\n" ^
                          generate_list generate_mem_alloc_host_to_device hofcall ^ 
                          generate_list generate_mem_cpy_host_to_device hofcall ^
                      "}\n" ^  *)
                      (* Sets Kernel params and other information needed to call cuLaunchKernel *)
                      generate_kernel_params hofcall.input_arrays_info ^ "\n" ^
                      "unsigned int blockSizeX = 16;\n" ^ 
                      "unsigned int blockSizeY = 1;\n" ^
                      "unsigned int blockSizeZ = 1;\n" ^
                      "unsigned int gridSizeX = 1;\n" ^
                      "unsigned int gridSizeY = 1;\n" ^
                      "unsigned int gridSizeZ = 1;\n" ^
                      (* Launches kernel *)
                      "checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ, blockSizeX, blockSizeY, blockSizeZ,0, NULL, KernelParams, NULL));\n" ^
                      (* Copies result array back to host *)
                      "checkCudaErrors(cuMemcpyDtoH(c," ^ Utils.idtos((hofcall.return_array_info).host_name) ^ ", sizeof(" ^ generate_variable_type ((hofcall.return_array_info).variable_type) ^ ")*" ^ string_of_int hofcall.array_length ^ "));\n" ^ 
                      (* Cleanup *)
                      generate_list generate_mem_cleanup "\n" hofcall.input_arrays_info ^ "\n" ^ 
                      generate_mem_cleanup fcall.return_array_info ^ "\n" ^ 
                      generate_list generate_mem_cleanup "\n" hofcall.constants ^ "\n" ^
                      "checkCudaErrors(cuModuleUnload(cudaModule));\n" ^ 
                      "checkCudaErrors(cuCtxDestroy(context));\n" ^  
                  "}\n" 
    | "reduce" -> raise Exceptions.Unknown_higher_order_function_call
   (* | _ -> raise Exceptions.Unknown_higher_order_function_call *)
   in sprintf "%s" higher_order_function_call_string



let generate_variable_statement vstatement = 
  let vstatement_string = match vstatement with
    | Declaration(d)  -> 
        (generate_vdecl d) ^ ";\n"
    | Assignment (id, e) -> 
        (generate_id id) ^ "=" ^ (generate_expression e) ^ ";\n"
    | Initialization(d,e) ->
        (generate_vdecl d) ^ "=" ^ (generate_expression e) ^ ";\n"
(*     | _ -> raise Exceptions.Unknown_variable_statement *)
  in sprintf "%s" vstatement_string



(* Generates statements *)
let rec generate_statement statement  =
  let statement_string = match statement with
    | Variable_Statement(vsmtm) -> 
        generate_variable_statement vsmtm  
    | Expression(e) -> 
        (generate_expression e) ^ ";\n"
    | Block(stmt_list) -> generate_list generate_statement "" stmt_list
    | If(e,stmt1,stmt2) -> 
        (match stmt2 with 
        | Block([]) -> "if(" ^ (generate_expression e) ^ "){\n" ^ (generate_statement stmt1) ^ "}\n"
        | _ -> "if(" ^ (generate_expression e) ^ "){\n" ^ (generate_statement stmt1) ^ "}\n" ^ "else{\n" ^ (generate_statement stmt2) ^ "}\n")
    | While(e,stmt) -> "while(" ^ (generate_expression e) ^ "){\n" ^ (generate_statement stmt) ^ "}\n"
    | For(stmt1,e,stmt2,stmt3) -> "for(" ^ (generate_statement stmt1) ^ (generate_expression e) ^ ";" ^ (generate_statement stmt2) ^ "){\n" ^ (generate_statement stmt3) ^ "}\n"
    | Return(e) ->
        "return" ^ (generate_expression e) ^ ";\n"
    | Return_Void ->  
        "return;\n"
    | Continue ->
        "continue;\n"
    | Break ->
        "break;\n"
(*     | _ -> raise Exceptions.Unknown_type_of_statement *)
  in sprintf "%s" statement_string


(* Generates function declarations *)
let generate_fdecl f  =
  let fdecl_string = 
    (generate_variable_type f.c_fdecl_return_type) ^ " " ^ 
    (generate_id f.c_fdecl_name) ^ "(" ^ 
    (generate_list generate_param "," f.c_fdecl_params) ^ "){\n" ^ 
    (generate_list generate_statement "\n" f.c_fdecl_body) ^ "}\n" 
  in
  sprintf "%s" fdecl_string

(* Writing out to CUDA file *)
let write_cuda filename cuda_program_string = 
  let file = open_out (filename ^ ".cu") in 
  fprintf file "%s" cuda_program_string

(* Generates the full CUDA file *)
let generate_cuda_file filename program = 
  let cuda_program_body = 
    (generate_list generate_variable_statement "" (Utils.triple_fst(program))) ^ 
    (generate_list generate_fdecl "" (Utils.triple_trd(program))) 
  in 
  let cuda_program_string = sprintf "\n\
  #include <stdio.h>\n\
  #include <stdlib.h>\n\
  #include \"cuda.h\"\n\
  #include <iostream>\n\
  #include <vlc.hpp>\n\
  #include <stdargs.h>
  CUdevice    device;\n\
  CUmodule    cudaModule;\n\
  CUcontext   context;\n\
  CUfunction  function;\n\
  %s" cuda_program_body in
  write_cuda filename cuda_program_string

(* Generate program *)
let generate_program cuda_filename program = 
  generate_cuda_file cuda_filename program;
  Codegen_ptx.generate_ptx_function_files program
  

