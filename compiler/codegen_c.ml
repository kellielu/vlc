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
    | Bitwise_And -> "&"
    | Bitwise_Or -> "|"
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

(* Generates pure variable type from array *)
let rec generate_pure_data_type arr = 
  match arr with 
  | Primitive(p) -> generate_data_type p 
  | Array(t,n) -> generate_pure_data_type t

(* Generate variable type *)
let generate_variable_type variable_type  =
  let vtype = match variable_type with
    | Primitive(p) -> generate_data_type p
    | Array(t,n) ->  
      let vtype = generate_pure_data_type t in
      "VLC_Array<" ^ vtype ^ ">"
  in sprintf "%s" vtype

(* Generate id *)
let generate_id id  = 
  let id_string = Utils.idtos(id) in 
  sprintf "%s" id_string

let generate_function_id id = 
let id_string = Utils.idtos(id) in
  match id_string with
    | "print" -> sprintf "printf"
    | "random" -> sprintf "rand"
    | _ -> sprintf "%s" id_string

 (* Generates CUDA device pointer *)
 let generate_device_ptr ktype = 
    sprintf "CUdeviceptr " ^ Utils.idtos ktype.kernel_name ^ ";"

 (* Generates CUDA memory allocation from host to device *)
 (* Fill in with VLC_Array*) 
 let generate_mem_alloc_statement_host_to_device ktype= 
    let arr_length = 
        match ktype.variable_type with 
          | Sast.Array(t,i_list) -> (List.fold_left (fun x y -> x * y) 1 i_list)
          | _ -> 1
      in
    sprintf "checkCudaErrors(cuMemAlloc(&" ^ Utils.idtos(ktype.kernel_name) ^ ", sizeof(" ^ (generate_pure_data_type ktype.variable_type) ^ ")*" ^ string_of_int arr_length ^ "));"

 let generate_mem_alloc_host_to_device fcall = 
    (* let rec create_list mylist length element = if length > 0 then create_list (element::mylist) (length-1) element else mylist in
     *)let mem_alloc_string = 
    String.concat "\n" (List.map generate_mem_alloc_statement_host_to_device fcall.input_arrays_info) in
    sprintf "%s" mem_alloc_string

(* Generates CUDA copying from host to device*)
 let generate_mem_cpy_statement_host_to_device ktype= 
    let mem_cpy_string = 
      match ktype.variable_type with 
        | Array(t,n) -> 
              let arr_length = 
                (match ktype.variable_type with 
                  | Sast.Array(t,i_list) -> (List.fold_left (fun x y -> x * y) 1 i_list)
                  | _ -> 1)
              in
              "checkCudaErrors(cuMemcpyHtoD("^ Utils.idtos(ktype.kernel_name) ^", " ^ Utils.idtos(ktype.host_name) ^ ", sizeof(" ^ (generate_pure_data_type ktype.variable_type) ^ ")*" ^ string_of_int arr_length ^ "));\n"
        | Primitive(t) -> 
              "checkCudaErrors(cuMemcpyHtoD("^ Utils.idtos(ktype.kernel_name) ^", " ^ "&" ^ Utils.idtos(ktype.host_name) ^ ", sizeof(" ^ (generate_variable_type ktype.variable_type) ^ ")*1" ^ "));\n"
    in sprintf "%s" mem_cpy_string

 let generate_mem_cpy_host_to_device fcall = 
    (* let rec create_list mylist length element = if length > 0 then create_list (element::mylist) (length-1) element else mylist in
     *)let mem_cpy_string = String.concat "\n" (List.map generate_mem_cpy_statement_host_to_device fcall.input_arrays_info ) in
    sprintf "%s" mem_cpy_string

(* Generates CUDA statement for kernel params *)
 let generate_kernel_params arr_info array_length= 
    let rec get_kernel_names a_info_list name_list = 
      match a_info_list with 
        | [] -> name_list 
        | hd::tl -> get_kernel_names tl (List.rev (hd.kernel_name::(List.rev name_list) ))
    in
    let kernel_names = (get_kernel_names arr_info []) in 
    let kernel_param_string = generate_list generate_id ", &" kernel_names in 
    sprintf "void *KernelParams[] = { &" ^ kernel_param_string ^"," ^ string_of_int array_length ^ "};"

(* Generate CUDA memory cleanup *)
let generate_mem_cleanup arr_info = 
    sprintf "checkCudaErrors(cuMemFree("^ Utils.idtos(arr_info.kernel_name) ^ "));"

(* Generates variable declaration statements *)
let generate_vdecl d  = 
   match d with 
  | Variable_Declaration(vtype,id) -> (generate_variable_type vtype) ^ " " ^ (generate_id id)

let generate_param d =
  match d with 
    | Variable_Declaration(vtype,id) ->
      match vtype with
        | Array(t,n) ->
          let param_string = "VLC_Array<" ^ (generate_variable_type t) ^ ">" ^ generate_id id  in 
          sprintf "%s" param_string
        | Primitive(p) ->
          let param_string = (generate_data_type p) ^ " " ^ (generate_id id) in 
          sprintf "%s" param_string
(*         | _ -> raise Exceptions.Unknown_variable_type
    | _ -> raise Exceptions.Unknown_type_of_param *)

 (* Generate expressions - including higher order function calls - and constants *)
let rec generate_expression expression  =
  let expr = match expression with
    | Sast.Function_Call(id, expr_list) ->
        (generate_function_id id) ^ "(" ^ generate_list generate_expression "," expr_list ^ ")"
(*     | Higher_Order_Function_Call(fcall) -> generate_higher_order_function_call fcall *)
    | Sast.String_Literal(s) -> 
        "\"" ^ s ^ "\""
    | Sast.Integer_Literal(i) -> 
        string_of_int i
    | Sast.Boolean_Literal(b) -> 
        string_of_bool b
    | Sast.Floating_Point_Literal(f) ->
        string_of_float f
    | Sast.Array_Literal(e_list,int_list) -> 
        "VLC_Array(" ^ string_of_int (List.length e_list) ^ "," ^ string_of_int (List.length int_list) ^"," ^ string_of_int ((List.length e_list) + (List.length int_list)) ^ "," ^ 
                      (generate_list string_of_int "," int_list) ^ "," ^ 
                      (generate_list generate_expression "," e_list) ^ ")" 
    | Sast.Identifier_Literal(id) -> 
        (generate_id id)
    | Sast.Cast(vtype,e) ->
        "(" ^ (generate_variable_type vtype) ^ ")" ^ (generate_expression e)
    | Sast.Binop(e1, o, e2) ->
        (match o with 
        | Sast.Equal | Sast.Not_Equal | Sast.Greater_Than | Sast.Less_Than | Sast.Greater_Than_Equal | Sast.Less_Than_Equal -> (generate_expression e1) ^ " " ^ (generate_binary_operator o) ^ " " ^ (generate_expression e2)
        | _ -> "("  ^ (generate_expression e1) ^ " " ^ (generate_binary_operator o) ^ " " ^ (generate_expression e2) ^ ")"
      )
    | Sast.Unop(e,o) ->
        (match o with 
        | Sast.Not | Sast.Negate  -> "("  ^(generate_unary_operator o) ^ (generate_expression e)^ ")"
        | Sast.Plus_Plus | Sast.Minus_Minus -> "(" ^ (generate_expression e) ^ (generate_unary_operator o))^ ")"
    | Sast.Array_Accessor(e,e_list,is_lvalue,access_array) -> 
        if is_lvalue = false then
          if access_array = true then
           "("  ^ (generate_expression e) ^ ".get_array_value_host(" ^ string_of_int (List.length e_list) ^ "," ^ generate_list generate_expression "," e_list ^ ")" ^ ")"
          else
           "("  ^ (generate_expression e) ^ ".get_element_value(" ^ string_of_int (List.length e_list) ^ "," ^ generate_list generate_expression "," e_list ^ ")"^ ")"
        else (generate_expression e)
    | Sast.Ternary(e1,e2,e3) -> "(" ^ (generate_expression e2) ^ ") ? " ^ (generate_expression e1) ^ ":" ^ (generate_expression e3)
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
  match ktype.variable_type with
    | Sast.Array(t,n) -> (generate_pure_data_type ktype.variable_type) ^ "* " ^ Utils.idtos ktype.host_name ^ ";"
    | Sast.Primitive(p) -> (generate_variable_type ktype.variable_type) ^ " " ^ Utils.idtos ktype.host_name ^ ";"

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

let generate_function_string_assignment id = 
  "std::ifstream " ^ Utils.idtos id ^ "(\"" ^ Utils.idtos id ^ ".ptx\");"

let generate_function_string_read id = 
  "std::string " ^ Utils.idtos id ^ "_str" ^ "((std::istreambuf_iterator<char>(" ^ Utils.idtos id ^ ")), std::istreambuf_iterator<char>());"

let rec generate_if_statements_constants constant_list str index = 
  match constant_list with 
    | [] -> ""
    | hd::[] -> 
      let statement_string = 
        if index = 0 then
          "if(i ==" ^ string_of_int index ^ "){\n" ^ 
            "\t" ^ Utils.idtos hd.host_name  ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^ 
            "\t" ^ generate_mem_alloc_statement_host_to_device hd ^ "\n" ^
            "\t" ^ generate_mem_cpy_statement_host_to_device hd ^ "\n" ^ 
          "}\n"
        else
          "else{\n" ^ 
            "\t" ^ Utils.idtos hd.host_name ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^ 
            "\t" ^ generate_mem_alloc_statement_host_to_device hd ^ "\n" ^
            "\t" ^ generate_mem_cpy_statement_host_to_device hd ^ "\n" ^ 
          "}\n"
      in str ^ statement_string
    | hd::tl ->
      let statement_string = 
        if index = 0 then
          "if(i ==" ^ string_of_int index ^ "){\n" ^ 
            "\t" ^ Utils.idtos hd.host_name  ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^ 
            "\t" ^ generate_mem_alloc_statement_host_to_device hd ^ "\n" ^
            "\t" ^ generate_mem_cpy_statement_host_to_device hd ^ "\n" ^ 
          "}\n"
        else
          "else if(i ==" ^ string_of_int index ^ "){\n" ^ 
            "\t" ^ Utils.idtos hd.host_name  ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^ 
            "\t" ^ generate_mem_alloc_statement_host_to_device hd ^ "\n" ^
            "\t" ^ generate_mem_cpy_statement_host_to_device hd ^ "\n" ^ 
          "}\n"
      in generate_if_statements_constants tl (str ^ statement_string) (index+1)

let rec generate_if_statements_input_arrays input_arrays str index = 
    match input_arrays with
    | [] -> ""
    | hd::[] ->
    let statement_string = 
         if index = 0 then
        "if(i ==" ^ string_of_int index ^ "){\n" ^ 
         "\t" ^ generate_variable_type hd.variable_type ^ " tmp"^ string_of_int index ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^
          "\t" ^ Utils.idtos hd.host_name  ^ " = tmp" ^ string_of_int index ^ ".get_values();\n" ^  
        "}\n"
        else
       "else{\n" ^ 
          "\t" ^ generate_variable_type hd.variable_type ^ " tmp" ^ string_of_int index ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^
          "\t" ^ Utils.idtos hd.host_name  ^ " = tmp" ^ string_of_int index ^ ".get_values();\n" ^ 
        "}\n"
      in str ^ statement_string
    | hd::tl ->
    let statement_string = 
      if index = 0 then
        "if(i ==" ^ string_of_int index ^ "){\n" ^ 
         "\t" ^ generate_variable_type hd.variable_type ^ " tmp"^ string_of_int index ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^
          "\t" ^ Utils.idtos hd.host_name  ^ " = tmp" ^ string_of_int index ^ ".get_values();\n" ^ 
        "}\n"
      else
        "else if(i ==" ^ string_of_int index ^ "){\n" ^ 
          "\t" ^ generate_variable_type hd.variable_type ^ " tmp" ^ string_of_int index ^ " = va_args(constants," ^ generate_variable_type hd.variable_type ^ ");\n" ^
          "\t" ^ Utils.idtos hd.host_name  ^ " = tmp" ^ string_of_int index ^ ".get_values();\n" ^ 
        "}\n"
    in generate_if_statements_input_arrays tl (str ^ statement_string) (index+1)

(* Generates c function declaration for map  *)
let generate_higher_order_function_decl hof = 
    let higher_order_function_decl_string = 
      match Utils.idtos(hof.higher_order_function_type) with
      | "map" ->  (generate_variable_type hof.return_array_info.variable_type) ^ 
                Utils.idtos hof.higher_order_function_name ^ "(...)" ^ "{\n\n" ^ 
                      "checkCudaErrors(cuCtxCreate(&context, 0, device));\n\n" ^ 
                      (* Generates strings for all the functions that are called*)
                      generate_list generate_function_string_assignment "\n" hof.called_functions ^ "\n" ^ 
                      generate_function_string_assignment hof.applied_kernel_function ^ "\n\n" ^  

                      (* Generates statements that read the string in from the file*)
                      generate_list generate_function_string_read "\n" hof.called_functions ^ "\n" ^ 
                      generate_function_string_read hof.applied_kernel_function ^ "\n\n" ^ 
                      (* Concatenates them all in one string *)
                      (Utils.idtos hof.applied_kernel_function) ^ "_str" ^ " = " ^ generate_list (fun x-> (Utils.idtos x) ^ "_str") " +\"\\n\" + " (List.rev(hof.applied_kernel_function::List.rev(hof.called_functions))) ^ ";\n\n" ^ 
                      
                      "checkCudaErrors(cuModuleLoadDataEx(&cudaModule," ^ (Utils.idtos hof.applied_kernel_function) ^ "_str.c_str()" ^ ", 0, 0, 0));\n" ^ 
                      "checkCudaErrors(cuModuleGetFunction(&function, cudaModule, \"" ^ (Utils.idtos hof.applied_kernel_function) ^ "\"));\n\n" ^ 
                      "size_t num_constants = " ^ string_of_int (List.length hof.higher_order_function_constants) ^ ";\n" ^
                      "size_t num_input_arrays = " ^ string_of_int (List.length hof.input_arrays_info) ^ ";\n\n" ^ 
                      (* Generates host pointers we are assigning to *)
                      generate_list generate_host_ptr "\n" hof.higher_order_function_constants ^ "\n" ^ 
                      generate_list generate_host_ptr "\n" hof.input_arrays_info ^ "\n" ^
                      generate_host_ptr hof.return_array_info ^ "\n\n" ^ 
                      (* Generates device pointers we are assigning to *)
                      generate_list generate_device_ptr "\n" hof.higher_order_function_constants ^ "\n" ^  
                      generate_list generate_device_ptr "\n" hof.input_arrays_info ^ "\n" ^ 
                      generate_device_ptr hof.return_array_info ^ "\n\n" ^ 
                      (* Performs constant copying *)
                      "va_list constants;\n" ^ 
                      "va_start(constants,num_constants);\n" ^ 
                      "for(int i = 0; i < num_constants; i++){\n\n" ^
                          generate_if_statements_constants hof.higher_order_function_constants "" 0 ^ "\n" ^ 
                      "}\n" ^ 
                      "va_end(constants);\n\n" ^ 
                      (* Matches arguments with host pointers*)
                      "for(int i =0;i < num_input_arrays; i++){\n\n" ^ 
                          generate_if_statements_input_arrays hof.input_arrays_info "" 0 ^ "\n" ^ 
                      "}\n" ^ 
                      (* Performs mem alloc and mem copy for input arrays*)
                      generate_list generate_mem_alloc_statement_host_to_device "\n" hof.input_arrays_info ^ "\n" ^ 
                      generate_list generate_mem_cpy_statement_host_to_device "" hof.input_arrays_info ^ "\n\n\n" ^ 
                      (* Performs mem alloc and mem copy for result array*)
                      generate_mem_alloc_statement_host_to_device hof.return_array_info ^ "\n" ^ 
                      generate_mem_cpy_statement_host_to_device hof.return_array_info ^ "\n\n" ^ 
                      (* " va_list constants;\n" 
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
                      generate_kernel_params (List.rev (hof.return_array_info::(List.rev hof.input_arrays_info))) hof.array_length ^ "\n\n" ^
                      "unsigned int blockSizeX = 16;\n" ^ 
                      "unsigned int blockSizeY = 1;\n" ^
                      "unsigned int blockSizeZ = 1;\n" ^
                      "unsigned int gridSizeX = 1;\n" ^
                      "unsigned int gridSizeY = 1;\n" ^
                      "unsigned int gridSizeZ = 1;\n\n" ^
                      (* Launches kernel *)
                      "checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ, blockSizeX, blockSizeY, blockSizeZ,0, NULL, KernelParams, NULL));\n\n" ^
                      (* Copies result array back to host *)
                      "checkCudaErrors(cuMemcpyDtoH(" ^ Utils.idtos((hof.return_array_info).host_name) ^ "," ^ Utils.idtos ((hof.return_array_info).kernel_name) ^ ", sizeof(" ^ generate_pure_data_type ((hof.return_array_info).variable_type) ^ ")*" ^ string_of_int hof.array_length ^ "));\n\n" ^ 
                      (* Cleanup *)
                      generate_list generate_mem_cleanup "\n" hof.input_arrays_info ^ "\n" ^ 
                      generate_mem_cleanup hof.return_array_info ^ "\n" ^ 
                      generate_list generate_mem_cleanup "\n" hof.higher_order_function_constants ^ "\n\n" ^
                      "checkCudaErrors(cuModuleUnload(cudaModule));\n" ^ 
                      "checkCudaErrors(cuCtxDestroy(context));\n" ^  
                  "}\n\n\n" 
    | _ -> raise Exceptions.Not_implemented_yet
   in sprintf "%s" higher_order_function_decl_string



let generate_variable_statement vstatement = 
  let vstatement_string = match vstatement with
    | Sast.Declaration(d)  -> 
        (match d with 
          | Variable_Declaration(vtype,id) ->
            (match vtype with
              | Sast.Array(t,i_list) -> 
                  (generate_vdecl d) ^ "= VLC_Array<"^ generate_variable_type t^ ">(" ^ 
                  string_of_int (List.fold_left (fun x y -> x * y) 1 i_list) ^ "," ^ 
                  string_of_int (List.length i_list) ^ "," ^ generate_list string_of_int "," i_list ^ ");\n"
              | Sast.Primitive(p) -> (generate_vdecl d) ^ ";\n"
            )
        )
        
    | Sast.Assignment (e1, e2) -> 
        (match e1 with
          | Sast.Array_Accessor(e,e_list,is_lvalue,array_access) ->
              if array_access = true then (generate_expression e1) ^ ".set_array_value(" ^ (generate_expression e2) ^ "," ^ string_of_int (List.length e_list) ^ ","^(generate_list generate_expression "," e_list )^");\n"
              else (generate_expression e1) ^ ".set_element_value(" ^ (generate_expression e2) ^ "," ^ string_of_int (List.length e_list)^ ");\n"
          | _ -> (generate_expression e1) ^ "=" ^ (generate_expression e2) ^ ";\n")
    | Sast.Initialization(d,e) ->
        (generate_vdecl d) ^ "=" ^ (generate_expression e) ^ ";\n"
(*     | _ -> raise Exceptions.Unknown_variable_statement *)
  in sprintf "%s" vstatement_string

let generate_for_statement for_stmt = 
  let statement_string = match for_stmt with
    | Sast.Variable_Statement(vstmt) ->
      (match vstmt with 
        | Sast.Declaration(d) ->
          (generate_vdecl d)
        | Sast.Assignment(e1,e2) ->
          (match e1 with
          | Sast.Array_Accessor(e,e_list,array_access,is_lvalue) ->
              if array_access = true then (generate_expression e1) ^ ".set_array_value(" ^ (generate_expression e2) ^ "," ^ string_of_int (List.length e_list) ^ ","^(generate_list generate_expression "," e_list )
              else (generate_expression e1) ^ ".set_element_value(" ^ (generate_expression e2) ^ "," ^ string_of_int (List.length e_list)
          | _ -> (generate_expression e1) ^ "=" ^ (generate_expression e2))
        | Sast.Initialization(d,e) ->
          (generate_vdecl d) ^ "=" ^ (generate_expression e)
      )
    | _ -> ""
(*     | _ -> raise Exceptions.Unknown_variable_statement *)
  in sprintf "%s" statement_string

(* Generates statements *)
let rec generate_statement statement  =
  let statement_string = match statement with
    | Sast.Variable_Statement(vsmtm) -> 
        generate_variable_statement vsmtm  
    | Sast.Expression(e) -> 
        (generate_expression e) ^ ";\n"
    | Sast.Block(stmt_list) -> generate_list generate_statement "" stmt_list
    | Sast.If(e,stmt1,stmt2) -> 
        (match stmt2 with 
        | Block([]) -> "if(" ^ (generate_expression e) ^ "){\n" ^ (generate_statement stmt1) ^ "}\n"
        | _ -> "if(" ^ (generate_expression e) ^ "){\n" ^ (generate_statement stmt1) ^ "}\n" ^ "else{\n" ^ (generate_statement stmt2) ^ "}\n")
    | Sast.While(e,stmt) -> "while(" ^ (generate_expression e) ^ "){\n" ^ (generate_statement stmt) ^ "}\n"
    | Sast.For(stmt1,e,stmt2,stmt3) -> "for(" ^ (generate_for_statement stmt1) ^";"^ (generate_expression e) ^ ";" ^ (generate_for_statement stmt2) ^ "){\n" ^ (generate_statement stmt3) ^ "}\n"
    | Sast.Return(e) ->
        "return " ^ (generate_expression e) ^ ";\n"
    | Sast.Return_Void ->  
        "return;\n"
    | Sast.Continue ->
        "continue;\n"
    | Sast.Break ->
        "break;\n"
(*     | _ -> raise Exceptions.Unknown_type_of_statement *)
  in sprintf "%s" statement_string

(* Generates function declarations *)
let generate_fdecl f  =
  let fdecl_string = 
    (generate_variable_type f.c_fdecl_return_type) ^ " " ^ 
    (generate_id f.c_fdecl_name) ^ "(" ^ 
    (generate_list generate_param "," f.c_fdecl_params) ^ "){\n" ^ 
    (generate_list generate_statement "" f.c_fdecl_body) ^ "}\n\n\n" 
  in
  sprintf "%s" fdecl_string

(* Writing out to CUDA file *)
let write_cuda filename cuda_program_string = 
  let file = open_out ((String.sub filename 0 ((String.length filename) - 4)) ^ ".cu") in 
  fprintf file "%s" cuda_program_string

(* Generates the full CUDA file *)
let generate_cuda_file filename program = 
  let cuda_program_body = 
    (generate_list generate_variable_statement "" (Utils.quint_fst(program))) ^ 
    (generate_list generate_higher_order_function_decl "" (Utils.quint_four(program))) ^
    (generate_list generate_fdecl "\n\n" (Utils.quint_five(program))) 
  in 
  let cuda_program_string = sprintf "\n\
  #include <stdio.h>\n\
  #include <stdlib.h>\n\
  #include \"cuda.h\"\n\
  #include <iostream>\n\
  #include \"vlc.hpp\"\n\
  #include <stdarg.h>\n\
  CUdevice    device;\n\
  CUmodule    cudaModule;\n\
  CUcontext   context;\n\
  CUfunction  function;\n\
  %s" cuda_program_body ^
  "int main(void) { return vlc(); }" in
  write_cuda filename cuda_program_string;
  print_endline cuda_program_string

(* Generate program *)
let generate_program cuda_filename program = 
  generate_cuda_file cuda_filename program;
  Codegen_ptx.generate_ptx_function_files program
  

