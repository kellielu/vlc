open Sast
open Utils
open Exceptions
(* For sprintf *)
open Printf
open String
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

(* Generates the ptx function string *)
(* Fill in once you have the generation for other ptx types in the sast *)
let generate_ptx_function ptx_function = 
	sprintf "test"

(* Writing out to PTX file *)
let write_ptx filename ptx_string = 
  let file = open_out (filename ^ ".ptx") in 
  fprintf file "%s" ptx_string

(* Main function for generating all ptx files*)
let rec generate_ptx_function_files program = 
  let ptx_function_list = Utils.triple_snd(program) in
  let rec generate_ptx_files ptx_func_list =
  	match ptx_func_list with
  		| [] -> ()
  		| hd::tl ->
  			write_ptx (Utils.idtos(hd.ptx_fdecl_name)) (generate_ptx_function hd);
  			generate_ptx_files tl
  in generate_ptx_files ptx_function_list


