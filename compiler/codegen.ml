open Ast
open Environment
open Utils

exception Empty_expression_list
exception Empty_parameter_list
exception Empty_fdecl_list
exception Empty_vdecl_list
exception Empty_list
exception Unknown_type_of_var
exception Unknown_type_of_vdecl
exception Unknown_type_of_param
exception Type_mismatch of string
exception Invalid_operation
exception Not_implemented

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


(*------------------------------------------------------------*)
(*---------------------Expressions----------------------------*)
(*------------------------------------------------------------*)
let rec generate_expression expression env =
  match expression with
    | Function_Call(id, exp) ->
      Environment.combine env [
        Generator(generate_id id);
        Verbatim("(");
        Generator(generate_expression_list exp);
        Verbatim(")")
      ]
(*     | Binop(e1, op, e2) ->
      let variable_type = (infer_type e1 env) in
      (match datatype with
        | Array ->
    let func = match op with
            | Add -> 
    Environment.combine env [
    Verbatim("int *d_a;\n\
        int *d_b;\n\
        int *d_c;\n\
        cudaMalloc(&d_a, by")] *)
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
    Environment.combine env [
      Generator(generate_variable_type t);
      Verbatim(" ");
      Generator(generate_id d.name);
      Verbatim("[");
      Verbatim(String.concat "][" (List.map string_of_int array_dimensions));
      Verbatim("]");
      Verbatim(";\n")
    ]
  | String| Integer -> 
    Environment.combine env [
      Generator(generate_variable_type d.v_type);
      Verbatim(" ");
      Generator(generate_id d.name);
      Verbatim(";\n");
    ]
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
  Verbatim("=");
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




(*--------------------------------------------------------*)
(*---------------------program----------------------------*)
(*--------------------------------------------------------*)
let generate_program program =
  let v_list = fst(program) in 
  let f_list = snd(program) in
  let env = Environment.create in
  let program, env = 
  Environment.combine env [
    Verbatim("#include <stdio.h>\n#include <stdlib.h>\n\
__global__ void vecAdd(float *a, float *b, float *c, int n)\
\t{\n\
\tint id = blockIdx.x*blockDim.x+threadIdx.x;\n\
if (id < n)\n\
{c[id] = a[id] + b[id];\n}\
}\n\n");
    Generator(generate_vdecl_list v_list);
    Generator(generate_fdecl_list f_list);
    Verbatim("\nint main(void) { return vlc(); }")
  ]
  in program

