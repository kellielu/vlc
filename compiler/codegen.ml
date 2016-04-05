open Ast
open Environment

exception Empty_list
exception Unknown_type
exception Type_mismatch



let generate_variable_type variable_type env =
  match variable_type with
    | String -> Environment.combine env [Verbatim("string")]
    | Integer -> Environment.combine env [Verbatim("int")]
    | _ -> raise Unknown_type

let generate_vdecl d env =
  Environment.combine env [
    Generator(generate_variable_type d.v_type);
    Verbatim(" ");
    Verbatim(d.name)
  ]




(*---------------------expressions----------------------------*)
let rec generate_expression expression env =
  match expression with
    Function_Call(i, exp) ->
      Environment.combine env [
        Verbatim(i);
        Verbatim("(");
        Generator(generate_expression_list exp);
        Verbatim(")")
      ]
    | String_Literal(s) -> Environment.combine env [Verbatim("\"" ^ s ^ "\"")]
    | Integer_Literal(i) -> Environment.combine env [Verbatim(string_of_int i)]
    | Identifier(i) -> Environment.combine env [Verbatim(i)]
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
    | [] -> raise Empty_list




(*---------------------parameters----------------------------*)
let rec generate_nonempty_parameter_list param_list env =
  match param_list with
    | param :: [] -> Environment.combine env [Generator(generate_vdecl param)]
    | param :: tail ->
      Environment.combine env [
        Generator(generate_vdecl param);
        Verbatim(", ");
        Generator(generate_nonempty_parameter_list tail)
      ]
    | [] -> raise (Empty_list)
and generate_parameter_list param_list env =
  match param_list with
    | [] -> Environment.combine env [Verbatim("")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_parameter_list tail)]





(*---------------------statements----------------------------*)
let rec generate_statement statement env =
  match statement with
    | Declaration(d) -> Environment.combine env [
	Generator(generate_vdecl d);
	Verbatim(";")
      ]
    | Expression(e) -> Environment.combine env [
        Generator(generate_expression e);
        Verbatim(";")
      ]
    | Assignment (s, e) ->
        Environment.combine env [
          Verbatim(s);
          Verbatim(" ");
          Verbatim(" = ");
	  Verbatim(" ");
          Generator(generate_expression e)
        ]
    | Return(e) ->
      Environment.combine env [
        Verbatim("return ");
        Generator(generate_expression e);
        Verbatim(";")
      ]
    | Initialization(d, e) -> Environment.combine env [
	Generator(generate_vdecl d);
	Verbatim(" ");
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





(*---------------------fdecls----------------------------*)
let generate_fdecl f env =
  Environment.combine env [
    Generator(generate_variable_type f.r_type);
    Verbatim(" ");
    Verbatim(f.name);
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
    | [] -> raise (Empty_list)
and generate_fdecl_list fdecl_list env =
  match fdecl_list with
    | [] -> Environment.combine env [Verbatim("")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_fdecl_list tail)]





(*---------------------vdecl list----------------------------*)
let rec generate_nonempty_vdecl_list vdecl_list env =
  match vdecl_list with
    | vdecl :: [] -> Environment.combine env [Generator(generate_vdecl vdecl)]
    | vdecl :: tail ->
      Environment.combine env [
        Generator(generate_vdecl vdecl);
        Verbatim(", ");
        Generator(generate_nonempty_vdecl_list tail)
      ]
    | [] -> raise (Empty_list)
and generate_vdecl_list vdecl_list env =
  match vdecl_list with
    | [] -> Environment.combine env [Verbatim("")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_vdecl_list tail)]




(*---------------------program----------------------------*)
let generate_program program =
  let v_list = fst(program) in 
  let f_list = snd(program) in
  let env = Environment.create in
  let program, env = 
  Environment.combine env [
    Verbatim("#include <stdio.h\n\n#include <stdlib.h\n\n#include <stdint.h\n\n#include <libvector.hpp>\n\n#include <string>\n\n");
    Generator(generate_vdecl_list v_list);
    Generator(generate_fdecl_list f_list);
    Verbatim("\nint main(void) { return vlc_main(); }")
  ]
  in program


