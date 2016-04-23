(* Collection of exceptions for different parts of the compiler *)

(*-------------------------------------Scanner-------------------------------------*)
exception Bad_dedent
(*-------------------------------------Parser-------------------------------------*)
exception Array_parsing_error
exception Invalid_data_type of string

exception Lexing_error of string  (* Unused atm *)
exception Parsing_error of string (* Unused atm *)
(*-------------------------------------Processor-------------------------------------*)
exception Missing_eof
(*-------------------------------------Utils-------------------------------------*)
(*-------------------------------------Semantic Analyzer-------------------------------------*)
exception Cannot_infer_expression_type
exception Exception of string

(*-------------------------------------Codegen C-------------------------------------*)
exception Unknown_variable_type
exception Unknown_data_type
exception Unknown_type_of_param
exception Unknown_higher_order_function_call
exception Unknown_type_of_vdecl
(*-------------------------------------Codegen PTX-------------------------------------*)
