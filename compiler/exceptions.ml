(* Collection of exceptions for different parts of the compiler *)

(*-------------------------------------Scanner-------------------------------------*)
exception Bad_dedent
(*-------------------------------------Parser-------------------------------------*)
exception Array_parsing_error
exception Invalid_type of string

exception Lexing_error of string  (* Unused atm *)
exception Parsing_error of string (* Unused atm *)
(*-------------------------------------Processor-------------------------------------*)
exception Missing_eof
(*-------------------------------------Utils-------------------------------------*)

(*-------------------------------------Semantic Analyzer-------------------------------------*)

(*-------------------------------------Environment-------------------------------------*)

(*-------------------------------------Codegen C-------------------------------------*)

(*-------------------------------------Codegen PTX-------------------------------------*)
