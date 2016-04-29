open Sast
(* open Exceptions *)
open Codegen_ptx 

(* For sprintf *)
open Printf

(*-------------------------------------Generating Functions-------------------------------------*)

(* Calls generate_func for every element of the list and concatenates results with specified concat symbol
   Used if you need to generate a list of something - e.x. list of statements, list of params *)
let generate_list generate_func concat mylist = 
  let list_string = String.concat concat (List.map generate_func mylist) in
  sprintf "%s" list_string

(* Generate operators *)
let generate_operator operator  =
  let op = match operator with
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Modulo -> "%"
(*     | _ -> raise Exceptions.Unknown_operator *)
  in
  sprintf "%s" op

(* Generate data type*)
let generate_data_type dtype = 
    let data_type = match dtype with 
      | String -> "char *"
      | Integer -> "int"
      | Void -> "void"
(*       | _ -> raise Exceptions.Unknown_data_type *)
    in sprintf "%s" data_type

(* Generate variable type *)
let rec generate_variable_type variable_type  =
  let vtype = match variable_type with
    | Primitive(p) -> generate_data_type p
    | Array(t,n) -> 
      match t with
        | Array(t1,n1) -> generate_variable_type t1 
        | Primitive(p) -> generate_data_type p
(*         | _ -> raise Exceptions.Unknown_variable_type
    | _ -> raise Exceptions.Unknown_variable_type *)
  in sprintf "%s" vtype

(* Generate id *)
let generate_id id  = 
  let id_string = Utils.idtos(id) in sprintf "%s" id_string
(*   match id_string with
    | "print" -> sprintf "printf"
    | _ as identifier -> sprintf identifier *)

 (* Generates CUDA device pointer *)
 let generate_device_ptr ptr_name = 
    sprintf "CUdeviceptr " ^ ptr_name ^ ";"

 (* Generates CUDA memory allocation from host to device *)
 (* Fill in with VLC_Array*) 
 let generate_mem_alloc_statement_host_to_device arr_info arr_length= 
    sprintf "checkCudaErrors(cuMemAlloc(&" ^ Utils.idtos(arr_info.kernel_name) ^ ", sizeof(" ^ (generate_variable_type arr_info.variable_type) ^ ")*" ^ string_of_int arr_length ^ "));"

 let generate_mem_alloc_host_to_device fcall = 
    let rec create_list mylist length element = if length > 0 then create_list (element::mylist) (length-1) element else mylist in
    let mem_alloc_string = 
    String.concat "\n" (List.map2 generate_mem_alloc_statement_host_to_device fcall.input_arrays_info (create_list [] (List.length fcall.input_arrays_info) fcall.array_length)) in
    sprintf "%s" mem_alloc_string

(* Generates CUDA copying from host to device*)
 let generate_mem_cpy_statement_host_to_device arr_info arr_length = 
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
        | Array(t,n) -> sprintf "vlcarray fill"
            (* Fill in with VLC_Array*)
            (* let array_dimensions= (get_array_dimensions t [n]) in
            Environment.combine  [
                Generator(generate_variable_type t);
                Verbatim(" ");
                Generator(generate_id d.name);
                (* Get the array dimensions *)
                Verbatim("[");
                Verbatim(String.concat "][" (List.map string_of_int array_dimensions));
                Verbatim("]")
            ] *)
        | Primitive(p) ->
            let param_string = (generate_data_type p) ^ " " ^ (generate_id id) in 
            sprintf "%s" param_string
(*         | _ -> raise Exceptions.Unknown_variable_type
  | _ -> raise Exceptions.Unknown_type_of_vdecl *)

let generate_param d =
  match d with 
    | Variable_Declaration(vtype,id) ->
      match vtype with
        | Array(t,n) -> sprintf "vlcarray fill"
            (* Fill in with VLC_Array*)
            (* let array_dimensions= (get_array_dimensions t [n]) in
            Environment.combine  [
                Generator(generate_variable_type t);
                Verbatim(" ");
                Generator(generate_id d.name);
                (* Get the array dimensions *)
                Verbatim("[");
                Verbatim(String.concat "][" (List.map string_of_int array_dimensions));
                Verbatim("]")
            ] *)
        | Primitive(p) ->
            let param_string = (generate_data_type p) ^ " " ^ (generate_id id) in 
            sprintf "%s" param_string
(*         | _ -> raise Exceptions.Unknown_variable_type
    | _ -> raise Exceptions.Unknown_type_of_param *)

 (* Generate expressions - including higher order function calls - and constants *)
let rec generate_expression expression  =
  let expr = match expression with
    | Binop(e1, o, e2) -> 
        (generate_expression e1) ^ " " ^ (generate_operator o) ^ " " ^ (generate_expression e2)
    | Function_Call(id, expr_list) ->
        (generate_id id) ^ "(" ^ generate_list generate_expression "," expr_list ^ ")"
    | String_Literal(s) -> 
        "\"" ^ s ^ "\""
    | Integer_Literal(i) -> 
        string_of_int i
    | Array_Literal(s) -> 
        "vlcarray fill"
        (* Fill in with VLC_Array*)
        (* sprintf "{" ^ (generate_expression_list s) ^ "}" *)
    | Identifier_Expression(id) -> 
        (generate_id id)
    | Kernel_Function_Call(kfcall) -> generate_kernel_function_call kfcall
    | Higher_Order_Function_Call(fcall) -> 
      match Utils.idtos(fcall.higher_order_function_type) with
        | "map" | "reduce" ->  generate_higher_order_function_call fcall
        | _ -> raise Exceptions.Unknown_higher_order_function_call
(*     | _ -> raise Exceptions.Unknown_type_of_expression *)
    
  in sprintf "%s" expr
(* Generates CUDA statements that copy constants from host to gpu *)
and generate_constant_on_gpu const  = 
  let mem_alloc_constant_string = match const.variable_type with 
    | Primitive(vtype) ->
          generate_device_ptr (Utils.idtos(const.kernel_name)) ^ 
          generate_mem_alloc_statement_host_to_device const 1 ^ 
          generate_mem_cpy_statement_host_to_device const 1 
    | Array(vtype,length) ->
        "vlcarray fill"
(*     | _ -> raise Exceptions.Unknown_variable_type *)
  in 
  sprintf "%s" mem_alloc_constant_string
and generate_kernel_function_call kfcall = sprintf "hi" (* Why do we need semicolon??????*)
    (* Fill in with VLC_Array *)
(* Generates statements for higher order map or reduce calls *)
and generate_higher_order_function_call fcall = 
    let higher_order_function_call_string = 
    (* Fill in with VLC_Array *)
      "{0};\n" ^ 
    (* Initializes CUDA driver and loads needed function *)
      "checkCudaErrors(cuCtxCreate(&context, 0, device));\n" ^ 
      "std::ifstream t(\"" ^ Utils.idtos fcall.applied_kernel_function ^ ".ptx\");\n" ^ 
      "if (!t.is_open()) {\n" ^
          " std::cerr << \"" ^ Utils.idtos fcall.applied_kernel_function ^ ".ptx not found\n\";\n" ^
          "return 1;\n" ^
      "}\n" ^
      "std::string " ^ Utils.idtos fcall.applied_kernel_function ^ "_str" ^ "((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());\n" ^ 
      "checkCudaErrors(cuModuleLoadDataEx(&cudaModule," ^ (Utils.idtos fcall.applied_kernel_function) ^ "_str" ^ ", 0, 0, 0));\n" ^ 
      "checkCudaErrors(cuModuleGetFunction(&function, cudaModule, \"" ^ (Utils.idtos fcall.applied_kernel_function) ^ "_str" ^ "\"));\n" ^ 
    (* Copies over constants *)
      generate_list generate_constant_on_gpu "\n" fcall.constants ^ "\n" ^
    (* Allocates GPU pointers for input and result array *)
    let rec get_kernel_names a_info_list name_list = 
      match a_info_list with 
        | [] -> name_list 
        | hd::tl -> get_kernel_names tl (Utils.idtos(hd.kernel_name)::name_list) 
    in
    let kernel_names = (get_kernel_names fcall.input_arrays_info []) in
      generate_list generate_device_ptr "\n" kernel_names ^ "\n" ^ 
      generate_device_ptr (Utils.idtos((fcall.return_array_info).kernel_name))  ^ "\n" ^
    (* Allocations memory and copies input arrays over to GPU memory *)
      generate_mem_alloc_host_to_device fcall ^ "\n" ^
      generate_mem_cpy_host_to_device fcall ^

    (* Sets Kernel params and other information needed to call cuLaunchKernel *)
      generate_kernel_params fcall.input_arrays_info ^ "\n" ^
      "unsigned int blockSizeX = 16;\n" ^ 
      "unsigned int blockSizeY = 1;\n" ^
      "unsigned int blockSizeZ = 1;\n" ^
      "unsigned int gridSizeX = 1;\n" ^
      "unsigned int gridSizeY = 1;\n" ^
      "unsigned int gridSizeZ = 1;\n" ^
    (* Launches kernel *)
      "checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ, blockSizeX, blockSizeY, blockSizeZ,0, NULL, KernelParams, NULL));\n" ^
    (* Copies result array back to host *)
      "checkCudaErrors(cuMemcpyDtoH(c," ^ Utils.idtos((fcall.return_array_info).host_name) ^ ", sizeof(" ^ generate_variable_type ((fcall.return_array_info).variable_type) ^ ")*" ^ string_of_int fcall.array_length ^ "));\n" ^ 
    (* Cleanup *)
    generate_list generate_mem_cleanup "\n" fcall.input_arrays_info ^ "\n" ^ 
    generate_mem_cleanup fcall.return_array_info ^ "\n" ^ 
    "checkCudaErrors(cuModuleUnload(cudaModule));\n" ^ 
    "checkCudaErrors(cuCtxDestroy(context));\n"
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
let generate_statement statement  =
  let statement_string = match statement with
    | Variable_Statement(vsmtm) -> 
        generate_variable_statement vsmtm  
    | Expression(e) -> 
        (generate_expression e) ^ ";\n"
    | Return(e) ->
        "return" ^ (generate_expression e) ^ ";\n"
    | Return_Void ->  
        "return;\n"
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
  

