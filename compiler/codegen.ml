open Ast
open Environment
open Utils
open Semant

exception Empty_expression_list
exception Empty_parameter_list
exception Empty_fdecl_list
exception Empty_kernel_fdecl_list
exception Empty_vdecl_list
exception Empty_list
exception Unknown_type_of_var
exception Unknown_type_of_vdecl
exception Unknown_type_of_param
exception Type_mismatch of string
exception Invalid_operation
exception Not_implemented
exception Unknown_type


(*------------------------------------------------------------*)
(*---------------------Type inference-------------------------*)
(*------------------------------------------------------------*)
let rec infer_type expression env =
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
    | _ -> raise (Not_implemented)


let generate_id id env = 
  let id_string = Utils.idtos(id) in
  match id_string with
    | "print" -> Environment.combine env [Verbatim("printf")]
    | _ as identifier -> Environment.combine env [Verbatim(identifier)]

(* ------------------------------------------------------------HOST CODE GENERATION ------------------------------------------------------------*)

(*------------------------------------------------------------*)
(*---------------------Expressions----------------------------*)
(*------------------------------------------------------------*)
let generate_operator operator env =
  match operator with
    | Add -> Environment.combine env [Verbatim("+")]
    | Subtract -> Environment.combine env [Verbatim("-")]
    | Multiply -> Environment.combine env [Verbatim("*")]
    | Divide -> Environment.combine env [Verbatim("/")]
    | Modulo -> Environment.combine env [Verbatim("%")]

let rec generate_expression expression env =
  match expression with
    | Function_Call(id, exp) ->
      Environment.combine env [
        Generator(generate_id id);
        Verbatim("(");
        Generator(generate_expression_list exp);
        Verbatim(")")
      ]
    | Binop(e1, op, e2) ->
      Environment.combine env [
        Generator(generate_expression e1);
        Generator(generate_operator) o);
        Generator(generate_expression e2)
      ]
    | String_Literal(s) -> Environment.combine env [Verbatim("\"" ^ s ^ "\"")]
    | Integer_Literal(i) -> Environment.combine env [Verbatim(string_of_int i)]
    | Array_Literal(s) -> 
    Environment.combine env [
          Verbatim("{");
          Generator(generate_expression_list s);
          Verbatim("}")]
    | Identifier_Expression(id) -> Environment.combine env [ Generator(generate_id id)]
and generate_expression_list expression_list env =
  match expression_list with
    | [] -> Environment.combine env []
    | lst -> Environment.combine env [Generator(generate_nonempty_expression_list lst)]
and generate_nonempty_expression_list expression_list env =
  match expression_list with
    | expression :: [] -> Environment.combine env [Generator(generate_expression expression)]
    | expression :: tail -> Environment.combine env [
        Generator(generate_expression expression);
        Verbatim(", ");
        Generator(generate_nonempty_expression_list tail)
      ]
    | [] -> (raise Empty_expression_list)


let rec generate_variable_type variable_type env =
  match variable_type with
    | String -> Environment.combine env [Verbatim("char *")]
    | Integer -> Environment.combine env [Verbatim("int")]
    | Array(t,n) -> 
      match t with
        | Array(t1,n1) -> generate_variable_type t1 env
        | String | Integer -> Environment.combine env [Generator(generate_variable_type t)]
        | _ -> raise Unknown_type_of_var

let rec get_array_dimensions vtype dimensions = 
  match vtype with
  | Array(t,n) -> 
      get_array_dimensions t (List.rev(n::dimensions))
  | String | Integer -> dimensions
  | _ -> raise Unknown_type_of_var


let generate_param d env =
  match d.v_type with 
  | Array(t,n) ->
      let array_dimensions= (get_array_dimensions t [n]) in
      Environment.combine env [
          Generator(generate_variable_type t);
          Verbatim(" ");
          Generator(generate_id d.name);
          (* Get the array dimensions *)
          Verbatim("[");
          Verbatim(String.concat "][" (List.map string_of_int array_dimensions));
          Verbatim("]")
      ]
  | String | Integer ->
      Environment.combine env [
        Generator(generate_variable_type d.v_type);
        Verbatim(" ");
        Generator(generate_id d.name)
      ]
  | _ -> raise Unknown_type_of_param

let generate_vdecl d env = 
  match d.v_type with 
  | Array(t,n) -> 
    let array_dimensions= (get_array_dimensions t [n]) in
    Environment.update_scope d.name d.v_type (
      Environment.combine env [
        Generator(generate_variable_type t);
        Verbatim(" ");
        Generator(generate_id d.name);
        Verbatim("[");
        Verbatim(String.concat "][" (List.map string_of_int array_dimensions));
        Verbatim("]");
        Verbatim(";\n")
      ]
    )
  | String| Integer -> 
    Environment.update_scope d.name d.v_type (
      Environment.combine env [
        Generator(generate_variable_type d.v_type);
        Verbatim(" ");
        Generator(generate_id d.name);
        Verbatim(";\n");
      ]
    )
  | _ -> raise Unknown_type_of_vdecl



(*-----------------------------------------------------------*)
(*---------------------Parameters----------------------------*)
(*-----------------------------------------------------------*)
let rec generate_nonempty_parameter_list param_list env =
  match param_list with
    | param :: [] -> Environment.combine env [Generator(generate_param param)]
    | param :: tail ->
      Environment.combine env [
        Generator(generate_param param);
        Verbatim(", ");
        Generator(generate_nonempty_parameter_list tail)
      ]
    | [] -> raise (Empty_parameter_list)
and generate_parameter_list param_list env =
  match param_list with
    | [] -> Environment.combine env [Verbatim("")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_parameter_list param_list)]

(*-----------------------------------------------------------*)
(*------------------Parallel ops-----------------------------*)
(*-----------------------------------------------------------*)
let generate_copy_from_host id1 id2 id3 env ->

let generate_copy_to_host id1 id2 id3 env ->

let generate_parallel_binop id1 id2 op id3 env ->
  



(*-----------------------------------------------------------*)
(*---------------------Statements----------------------------*)
(*-----------------------------------------------------------*)
let rec generate_statement statement env =
  match statement with
    | Declaration(d) -> Environment.combine env [
	Generator(generate_param d);
	Verbatim(";")
      ]
    | Expression(e) -> Environment.combine env [
        Generator(generate_expression e);
        Verbatim(";")
      ]
    | Assignment (id, e) ->
        let datatype = (infer_type id env) in
        (match datatype with
	  | Array -> Environment.combine env [
	      let var_type = Environment.get_var_type id env in
	      (
(* generate device initialization *) 
(* allocate memory for device *)
(* copy host arrays to device *)
(* declare blocksize *)
(* declare gridsize *)
(* execute kernel *)
(* copy array back to host *)
(* release memory ? *)
(* Will not be able to do A = B+C+D... *)
(* "  

  // CUDA initialization
  cuInit(0);
  cuDeviceGetCount(&devCount);
  cuDeviceGet(&device, 0);

  int devMajor, devMinor;
  cuDeviceComputeCapability(&devMajor, &devMinor, device);
  std::cout << \"Device Compute Capability: \"
            << devMajor << \".\" << devMinor << \"\n\";
  if (devMajor < 2) {
    std::cerr << \"ERROR: Device 0 is not SM 2.0 or greater\n\";
    return 1;
  }

  std::string str((std::istreambuf_iterator<char>(t)),
                    std::istreambuf_iterator<char>());

  // Create driver context
  checkCudaErrors(cuCtxCreate(&context, 0, device));

  // Create module for object
  checkCudaErrors(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

  // Get kernel function
  checkCudaErrors(cuModuleGetFunction(&function, cudaModule, \"kernel\"));

  // Device data
  CUdeviceptr devBufferA;
  CUdeviceptr devBufferB;
  CUdeviceptr devBufferC;

  checkCudaErrors(cuMemAlloc(&devBufferA, sizeof(float)*16));
  checkCudaErrors(cuMemAlloc(&devBufferB, sizeof(float)*16));
  checkCudaErrors(cuMemAlloc(&devBufferC, sizeof(float)*16));

  float* hostA = new float[16];
  float* hostB = new float[16];
  float* hostC = new float[16];

  // Populate input
  for (unsigned i = 0; i != 16; ++i) {
    hostA[i] = (float)i;
    hostB[i] = (float)(2*i);
    hostC[i] = 0.0f;
  }

  checkCudaErrors(cuMemcpyHtoD(devBufferA, &hostA[0], sizeof(float)*16));
  checkCudaErrors(cuMemcpyHtoD(devBufferB, &hostB[0], sizeof(float)*16));


  unsigned blockSizeX = 16;
  unsigned blockSizeY = 1;
  unsigned blockSizeZ = 1;
  unsigned gridSizeX  = 1;
  unsigned gridSizeY  = 1;
  unsigned gridSizeZ  = 1;

  // Kernel parameters
  void *KernelParams[] = { &devBufferA, &devBufferB, &devBufferC };

  std::cout << \"Launching kernel\n\";

  // Kernel launch
  checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ,
                                 blockSizeX, blockSizeY, blockSizeZ,
                                 0, NULL, KernelParams, NULL));

  // Retrieve device data
  checkCudaErrors(cuMemcpyDtoH(&hostC[0], devBufferC, sizeof(float)*16));


  std::cout << \"Results:\n\";
  for (unsigned i = 0; i != 16; ++i) {
    std::cout << hostA[i] << " + " << hostB[i] << " = " << hostC[i] << \"\n\";
  }


  // Clean up after ourselves
  delete [] hostA;
  delete [] hostB;
  delete [] hostC;

  // Clean-up
  checkCudaErrors(cuMemFree(devBufferA));
  checkCudaErrors(cuMemFree(devBufferB));
  checkCudaErrors(cuMemFree(devBufferC));
  checkCudaErrors(cuModuleUnload(cudaModule));
  checkCudaErrors(cuCtxDestroy(context));

  return 0;
"*)
          | Integer -> Environment.combine env [
              Generator(generate_id id);
              Verbatim(" = ");
              Generator(generate_expression e);
	      Verbatim(";")
            ]
          | String -> raise (Invalid_operation)
        )
        Environment.combine env [
          Generator(generate_id id);
          Verbatim(" = ");
          Generator(generate_expression e);
	  Verbatim(";")
        ]
    | Return(e) ->
      Environment.combine env [
        Verbatim("return ");
        Generator(generate_expression e);
        Verbatim(";")
      ]
    | Initialization(d, e) -> Environment.combine env [
	Generator(generate_param d);
	Verbatim(" = ");
	Generator(generate_expression e);
	Verbatim(";");
      ]
and generate_statement_list statement_list env =
  match statement_list with
    | [] -> Environment.combine env []
    | statement :: tail ->
        Environment.combine env [
          Generator(generate_statement statement);
          Verbatim("\n");
          Generator(generate_statement_list tail)
        ]




(*-------------------------------------------------------*)
(*---------------------fdecls----------------------------*)
(*-------------------------------------------------------*)
let generate_fdecl f env =
  Environment.combine env [
    Generator(generate_variable_type f.r_type);
    Verbatim(" ");
    Generator(generate_id f.name);
    Verbatim("(");
    Generator(generate_parameter_list f.params);
    Verbatim("){\n");
    Generator(generate_statement_list f.body);
    Verbatim("}\n");
  ]


let rec generate_nonempty_fdecl_list fdecl_list env =
  match fdecl_list with
    | fdecl :: [] -> Environment.combine env [Generator(generate_fdecl fdecl)]
    | fdecl :: tail ->
      Environment.combine env [
        Generator(generate_fdecl fdecl);
        Verbatim("\n\n");
        Generator(generate_nonempty_fdecl_list tail)
      ]
    | [] -> raise (Empty_fdecl_list)
and generate_fdecl_list fdecl_list env =
  match fdecl_list with
    | [] -> Environment.combine env [Verbatim("")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_fdecl_list fdecl_list)]




(*-----------------------------------------------------------*)
(*---------------------vdecl list----------------------------*)
(*-----------------------------------------------------------*)
let rec generate_nonempty_vdecl_list vdecl_list env =
  match vdecl_list with
    | vdecl :: [] -> Environment.combine env [Generator(generate_vdecl vdecl)]
    | vdecl :: tail ->
      Environment.combine env [
        Generator(generate_vdecl vdecl);
        Verbatim(", ");
        Generator(generate_nonempty_vdecl_list tail)
      ]
    | [] -> raise (Empty_vdecl_list)
and generate_vdecl_list vdecl_list env =
  match vdecl_list with
    | [] -> Environment.combine env [Verbatim("")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_vdecl_list vdecl_list)]

(*------------------------------------------------------------ KERNEL CODE GENERATION ------------------------------------------------------------*)

let generate_kernel_fdecl kernel_f env =
  Environment.combine env [
    Generator(generate_variable_type kernel_f.kernel_r_type);
    Verbatim(" ");
    Generator(generate_id kernel_f.kernel_name);
    Verbatim("(");
    Generator(generate_parameter_list kernel_f.kernel_params);
    Verbatim("){\n");
    Generator(generate_statement_list kernel_f.kernel_body);
    Verbatim("}\n");
  ]


let rec generate_nonempty_kernel_fdecl_list kernel_fdecl_list env =
  match kernel_fdecl_list with
    | kernel_fdecl :: [] -> Environment.combine env [Generator(generate_kernel_fdecl kernel_fdecl)]
    | kernel_fdecl :: tail ->
      Environment.combine env [
        Generator(generate_kernel_fdecl kernel_fdecl);
        Verbatim("\n\n");
        Generator(generate_nonempty_kernel_fdecl_list tail)
      ]
    | [] -> raise (Empty_kernel_fdecl_list)
and generate_kernel_fdecl_list kernel_fdecl_list env =
  match kernel_fdecl_list with
    | [] -> Environment.combine env [Verbatim("")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_kernel_fdecl_list kernel_fdecl_list)]


(*--------------------------------------------------------*)
(*---------------------program----------------------------*)
(*--------------------------------------------------------*)
let generate_program program =
  let v_list = Utils.triple_fst(program) in 
  let kernel_f_list = Utils.triple_snd(program) in
  let f_list = Utils.triple_trd(program) in 
  let env = Environment.create in
  let program, env = 
  Environment.combine env [
    Verbatim("#include <stdio.h>
#include <stdlib.h>
#include \"cuda.h\"
#include<iostream>


char** add_kernel = [\".version 3.1\",
\".target sm_20\",
\".address_size 64\",

\".visible .entry kernel(\",
\"  .param .u64 kernel_param_0,\",
\"  .param .u64 kernel_param_1,\",
\"  .param .u64 kernel_param_2\",
\")\",
\"{\",
\"  .reg .f32   %f<4>;\",
\"  .reg .s32   %r<2>;\",
\"  .reg .s64   %rl<8>;\",

\"  ld.param.u64    %rl1, [kernel_param_0];\",
\"  mov.u32         %r1, %tid.x;\",
\"  mul.wide.s32    %rl2, %r1, 4;\",
\"  add.s64         %rl3, %rl1, %rl2;\",
\"  ld.param.u64    %rl4, [kernel_param_1];\",
\"  add.s64         %rl5, %rl4, %rl2;\",
\"  ld.param.u64    %rl6, [kernel_param_2];\",
\"  add.s64         %rl7, %rl6, %rl2;\",
\"  ld.global.f32   %f1, [%rl3];\",
\"  ld.global.f32   %f2, [%rl5];\",
\"  add.f32         %f3, %f1, %f2;\",
\"  st.global.f32   [%rl7], %f3;\",
\"  ret\",
\"}\"]\n"
    Generator(generate_vdecl_list v_list);
    Generator(generate_kernel_fdecl_list kernel_f_list);
    Generator(generate_fdecl_list f_list);
    Verbatim("\nint main(void) { return vlc(); }")
  ]
  in program

