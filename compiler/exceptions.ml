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
exception Already_declared
exception Name_not_found of string
exception Invalid_environment
exception Variable_not_found_in_scope
exception Function_not_defined
exception Cannot_pop_empty_variable_scope_stack
exception Variable_already_declared
exception Not_an_array_expression
exception Type_mismatch of string
exception Empty_array_expression_list
exception Variable_not_declared
exception Cannot_assign_expression
exception NO_STRINGS_ALLOWED_IN_GDECL
exception Void_type_in_gdecl
exception Invalid_input_argument_to_map
exception Not_a_valid_constant
exception Nonarray_argument_passed_into_higher_order_function
exception Function_already_declared
exception Have_statements_after_return_break_continue
exception Invalid_statement_in_for
exception Constants_missing_in_defg
exception Cannot_pop_original_scope
exception Variable_scope_not_initialized
exception Empty_array_access
exception Invalid_array_expression
exception Array_elements_not_all_same_type
exception Not_a_valid_variable_declaration
exception String_not_accepted_by_operator
exception Conditional_must_be_a_boolean
exception Invalid_accessor_value
(*-------------------------------------Codegen C-------------------------------------*)
exception Unknown_variable_type
exception Unknown_operator
exception Unknown_data_type
exception Unknown_type_of_param
exception Unknown_higher_order_function_call
exception Unknown_type_of_vdecl
exception Unknown_type_of_expression
exception Unknown_variable_statement
exception Unknown_type_of_statement
(*-------------------------------------Codegen PTX-------------------------------------*)
exception PTXCREMENT_GENERATED_ERROR
