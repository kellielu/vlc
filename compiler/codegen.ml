open Ast
open Environment

exception Empty_list
exception Unknown_type
exception Type_mismatch

let rec infer_type expression env =
  let f type1 type2 =
    match type1 with
      | Some(t) -> (if t = type2 then Some(t) else raise Type_mismatch)
      | None -> Some(type2) in
    let match_type expression_list =
      let a = List.fold_left f None expression_list in
        match a with
          | Some(t) -> t
          | None -> raise Empty_list in
    match expression with
      | Integer_Literal(_) -> Integer
      | String_Literal(_) -> String
      | Function_Call(i, es) -> (match i with
         | Identifier("print") -> Void)
         
let generate_identifier identifier env =
  match identifier with
    Identifier(s) -> Environment.combine env [Verbatim(s)]

let generate_variable_type variable_type env =
  match variable_type with
    | String -> Environment.combine env [Verbatim("string")]
    | Integer -> Environment.combine env [Verbatim("int")]
    | _ -> raise Unknown_type

let generate_expression expression env =
  match expression with
    Function_Call(i, exp) ->
      Environment.combine env [
        Generator(generate_identifier i);
        Verbatim("(");
        Generator(generate_expression_list exp);
        Verbatim(")")
      ]
    | String_Literal(s) -> Environment.combine env [Verbatim("\"" ^ s ^ "\"")]
    | Integer_Literal(i) -> Environment.combine env [Verbatim(Int.to_string i)]
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
and generate_declaration d env =
  match d with
    | Declaration (d, i) ->
        Environment.combine env [
          Generator(generate_variable_type d);
          Verbatim(" ");
          Generator(generate_identifier i)
        ]

//////////////////////////////

let rec generate_nonempty_decl_list decl_list env =
  match decl_list with
    | decl :: [] -> Environment.combine env [Generator(generate_declaration decl)]
    | decl :: tail ->
      Environment.combine env [
        Generator(generate_declaration decl);
        Verbatim(", ");
        Generator(generate_nonempty_decl_list tail)
      ]
    | [] -> raise Empty_list
    
let generate_decl_list decl_list env =
  match decl_list with
    | [] -> Environment.combine env [Verbatim("void")]
    | decl :: tail -> Environment.combine env [Generator(generate_nonempty_decl_list tail)]

////////////////////////////////


let rec generate_statement statement env =
  match statement with
    | Expression(e) ->
      Environment.combine env [
        Generator(generate_expression e);
        Verbatim(";")
      ]
    | Variable_Declaration_Assignment (i, e) ->
        Environment.update identifier variable_type (
        Environment.combine env [
          Generator(generate_variable_type t);
          Verbatim(" ");
          Generator(generate_identifier i);
          Verbatim(" = ");
          Generator(generate_expr e)
        ])
    | Variable_Declaration(d) ->
        Environment.update i d (Environment.combine env [
          Generator(generate_declaration d);
          Verbatim(" ");
          Generator(generate_identifier i)
        ])
    | Function_Declaration(t, i, ds, ss) ->
        Environment.combine env [
          Generator(generate_variable_type t);
          Verbatim(" ");
          Generate(generate_identifier i);
          Verbatim("(");
          Generator(generate_declaration ds);
          Verbatim(") {\n");
          Generator(generate_statement_list ss);
          Verbatim("}");
       ]
    | Return(e) ->
      Environment.combine env [
        Verbatim("return ");
        Generator(generate_expression e);
        Verbatim(";")
      ]
and generate_statement_list statement_list env =
  match statement_list with
    | [] -> Environment.combine env []
    | statement :: tail ->
        Environment.combine env [
          Generate(generate_statement statement);
          Verbatim("\n");
          Generator(generate_statement_list tail)
        ]

let generate_toplevel tree =
  let env = Environment.create in
  Environment.combine env [
    Verbatim("#include <stdio.h\n\n#include <stdlib.h\n\n#include <stdint.h\n\n#include <libvector.hpp>\n\n#include <string>\n\n");
    Generator(generate_statement_list tree);
    Verbatim("\nint main(void) { return 1; }")
  ]
