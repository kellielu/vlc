open Sast
open Utils
open Exceptions
open Codegen_ptx 

(* For sprintf *)
open Printf
open String

let builtin_functions = ["print"]

let is_builtin_function name =
  List.exists (fun function_name -> function_name = name) builtin_functions

let rec get_array_dimensions vtype dimensions = 
  match vtype with
  | Array(t,n) -> 
      get_array_dimensions t (List.rev(n::dimensions))
  | Primitive(p) -> dimensions
  | _ -> raise Exceptions.Unknown_variable_type

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
  in
  sprintf "%s" op

(* Generate data type*)
let generate_data_type dtype = 
    let data_type = match dtype with 
      | String -> "char *"
      | Integer -> "int"
      | Void -> "void"
      | _ -> Exceptions.Unknown_data_type
    in sprintf data_type

(* Generate variable type *)
let rec generate_variable_type variable_type  =
  let vtype = match variable_type with
    | Primitive(p) -> generate_data_type p
    | Array(t,n) -> 
      match t with
        | Array(t1,n1) -> generate_variable_type t1 
        | Primtive(p) -> generate_data_type p
        | _ -> raise Exceptions.Unknown_variable_type
  in sprintf "%s" vtype

(* Generate id *)
let generate_id id  = 
  let id_string = Utils.idtos(id) in
  match id_string with
    | "print" -> sprintf "printf"
    | _ as identifier -> sprintf identifier

 (* Generates CUDA device pointer *)
 let generate_device_ptr name = 
    sprintf "CUdeviceptr " ^ name ^ ";"

 (* Generates CUDA memory allocation from host to device *)
 (* Fill in with VLC_Array*) 
 let generate_mem_alloc_statement_host_to_device arr_name arr_datatype arr_length= 
    sprintf "checkCudaErrors(cuMemAlloc(&" ^ arr_name ^ ", sizeof(" ^ arr_datatype ^ ")*" ^ arr_length ^ "));"

 let generate_mem_alloc_host_to_device fcall = 
    let create_list mylist length element = if length > 0 then element::mylist; create_list mylist (length-1) element else mylist in
    let mem_alloc_string = String.concat "\n" List.map2 generate_mem_alloc_statement_host_to_device fcall.kernel_input_array_names fcall.input_array_types (create_list [] (List.length fcall.kernel_input_array_names) fcall.array_length) in
    sprintf "%s" mem_alloc_string

(* Generates CUDA copying from host to device*)
 let generate_mem_cpy_statement_host_to_device kernel_arr_name host_arr_name vtype arr_length = 
    let mem_cpy_string  = 
      "checkCudaErrors(cuMemcpyHtoD("^ kernel_arr_name ^", " ^ host_arr_name ^ ", sizeof(" ^ vtype ^ ")*" ^ arr_length ^ "));\n"

 let generate_mem_cpy_host_to_device fcall = 
    let create_list mylist length element = if length > 0 then element::mylist; create_list mylist (length-1) element else mylist in
    let mem_cpy_string = String.concat "\n" List.map2 generate_mem_cpy_statement_host_to_device fcall.kernel_input_array_names fcall.host_input_array_names fcall.input_array_types (create_list [] (List.length fcall.kernel_input_array_names) fcall.array_length) in
    sprintf "%s" mem_cpy_string

(* Generates CUDA statement for kernel params *)
 let generate_kernel_params name_array = 
    let kernel_param_string = generate_list generate_id ", &" name_array in 
    sprintf ("void *KernelParams[] = { &" ^ kernel_param_string ^ "};")

(* Generate CUDA memory cleanup *)
let generate_mem_cleanup name = 
    sprintf "checkCudaErrors(cuMemFree("^ name ^ "));"

 (* Generate expressions - including higher order function calls - and constants *)
let rec generate_expression expression  =
  let expr = match expression with
    | Binop(e1, o, e2) -> 
        sprintf (generate_expression e1) ^ " " (generate_operator o) ^ " " ^ (generate_expression e2)
    | Function_Call(id, exp) ->
        sprintf (generate_id id) ^ "(" ^ (generate_expression_list exp) ^ ")"
    | String_Literal(s) -> 
        sprintf "\"" ^ s ^ "\""
    | Integer_Literal(i) -> 
        sprintf string_of_int i
    | Array_Literal(s) -> 
        (* Fill in with VLC_Array*)
        (* sprintf "{" ^ (generate_expression_list s) ^ "}" *)
    | Identifier_Expression(id) -> 
        sprintf (generate_id id)
    | Higher_Order_Function_Call(fcall) -> 
      match Utils.idtos(fcall.f_type) with
        | "map" | "reduce" -> 
            generate_higher_order_function_call fcall
        | _ -> raise Exceptions.Unknown_higher_order_function_call
    | Kernel_Function_Call(kfcall) ->
        generate_kernel_function_call (kfcall)

(* Generates CUDA statements that copy constants from host to gpu*)
and generate_constant_on_gpu const  = 
  match const.variable_type with 
  | Primitive(vtype) ->
      let mem_alloc_constant_string = 
        generate_device_ptr const.kernel_const_name ^ 
        generate_mem_alloc_statement_host_to_device const.kernel_const_name vtype 1 ^ 
        generate_mem_cpy_statement_host_to_device const.kernel_const_name host_const_name vtype 1 ^ 
      in 
      sprintf mem_alloc_constant_string
  | Array(vtype,length) ->
    (* Fill in with VLC_Array *)

(* Generates statements for higher order map or reduce calls *)
and generate_higher_order_function_call fcall = 
    let higher_order_function_call_string = 
    (* Fill in with VLC_Array *)
      "{0};\n" ^ 
    (* Initializes CUDA driver and loads needed function *)
      "checkCudaErrors(cuCtxCreate(&context, 0, device));\n" ^ 
      "std::ifstream t(\"" ^ Utils.idtos fcall.kernel_function ^ ".ptx\");\n" ^ 
      "if (!t.is_open()) {\n" ^
          " std::cerr << \"" ^ Utils.idtos fcall.kernel_function ^ ".ptx not found\n\";\n" ^
          "return 1;\n" ^
      "}\n" ^
      "std::string " ^ Utils.idtos fcall.kernel_function ^ "_str" ^ "((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());\n" ^ 
      "checkCudaErrors(cuModuleLoadDataEx(&cudaModule," ^ (Utils.idtos fcall.kernel_function) ^ "_str" ^ ", 0, 0, 0));\n" ^ 
      "checkCudaErrors(cuModuleGetFunction(&function, cudaModule, \"" ^ (Utils.idtos fcall.kernel_function) ^ "_str" ^ "\"));\n" ^ 
    (* Copies over constants *)
      generate_list "\n" generate_constant_on_gpu fcall.constants ^ "\n" ^
    (* Allocates GPU pointers for input and result array *)
      generate_list generate_device_ptr "\n" fcall.kernel_input_array_names ^ "\n" ^ 
      generate_device_ptr fcall.kernel_return_array_name  ^ "\n" ^
    (* Allocations memory and copies input arrays over to GPU memory *)
      generate_mem_alloc_host_to_device fcall ^ "\n" ^
      generate_mem_cpy_host_to_device 

    (* Sets Kernel params and other information needed to call cuLaunchKernel *)
      generate_kernel_params fcall.kernel_array_name_list ^ "\n" ^
      "unsigned int blockSizeX = 16;\n" ^ 
      "unsigned int blockSizeY = 1;\n" ^
      "unsigned int blockSizeZ = 1;\n" ^
      "unsigned int gridSizeX = 1;\n" ^
      "unsigned int gridSizeY = 1;\n" ^
      "unsigned int gridSizeZ = 1;\n" ^
    (* Launches kernel *)
      "checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ, blockSizeX, blockSizeY, blockSizeZ,0, NULL, KernelParams, NULL));\n" ^
    (* Copies result array back to host *)
      "checkCudaErrors(cuMemcpyDtoH(c," ^ fcall.host_return_array_name ^ ", sizeof(" ^ fcall.return_array_type ^ ")*" ^ fcall.array_length ^ "));\n" ^ 
    (* Cleanup *)
    generate_list generate_mem_cleanup "\n" fcall.kernel_input_array_names ^ "\n" ^ 
    generate_mem_cleanup fcall.kernel_return_array_name ^ "\n" ^ 
    "checkCudaErrors(cuModuleUnload(cudaModule));\n" ^ 
    "checkCudaErrors(cuCtxDestroy(context));\n"
  in sprintf higher_order_function_call_string
  
and generate_kernel_function_call kfcall =
  (* Fill in for kernel function_call *)
  sprintf kfcall.kernel_function





(* Generates parameters for function declarations*)
let generate_param d  =
  match d.v_type with 
  | Array(t,n) ->
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
      let param_string = (generate_data_type p) ^ " " ^ (generate_id d.name) in 
      sprintf param_string
  | _ -> raise Exceptions.Unknown_type_of_param

(* Generates variable declaration statements *)
let generate_vdecl d  = 
  match d.v_type with 
  | Array(t,n) -> 
    (* Fill in with VLC_Array *)
    (* let array_dimensions= (get_array_dimensions t [n]) in
    Environment.update_scope d.name d.v_type (
      Environment.combine  [
        Generator(generate_variable_type t);
        Verbatim(" ");
        Generator(generate_id d.name);
        Verbatim("[");
        Verbatim(String.concat "][" (List.map string_of_int array_dimensions));
        Verbatim("]");
        Verbatim(";\n")
      ]
    ) *)
  | Primitive(p) -> 
      let vdecl_string = (generate_data_type p) ^ " " ^ (generate_id d.name) in 
      sprintf param_string
  | _ -> raise Exceptions.Unknown_type_of_vdecl

(* Generates variable statements *)
let generate_variable_statement vstatement = 
  let vstatement_string = match vstatement with
    | Declaration(d)  -> 
        (generate_vdecl d) ^ ";\n"
    | Assignment (id, e) -> 
        (generate_id id) ^ "=" ^ (generate_expression e) ^ ";\n"
    | Initialization(d,e) ->
        (generate_vdecl) ^ "=" ^ (generate_expression e) ^ ";\n"
  in sprintf vstatement_string

(* Generates statements *)
let rec generate_statement statement  =
  let statement_string = match statement with
    | Variable_Statement(vsmtm) -> 
        generate_variable_statement vsmtm  
    | Expression(e) -> 
        (generate_expression e) ^ ";\n"
    | Return(e) ->
        "return" ^ (generate_expression e) ^ ";\n"
    | Return_Void ->  
        "return;\n"
  in 
  sprintf statement_string


(* Generates function declarations *)
let generate_fdecl f  =
  let fdecl_string = 
    (generate_variable_type f.r_type) ^ " " ^ 
    (generate_id f.name) ^ "(" ^ 
    (generate_parameter_list f.params) ^ "){\n" ^ 
    (generate_statement_list f.body) ^ "}\n" 
  in
  sprintf fdecl_string

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
  let cuda_program_string = sprintf "
  #include <stdio.h>
  #include <stdlib.h>
  #include \"cuda.h\"
  #include <iostream>

  CUdevice    device;
  CUmodule    cudaModule;
  CUcontext   context;
  CUfunction  function;

  %s" cuda_program_body in
  write_cuda filename cuda_program_string

