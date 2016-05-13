(* Collection of exceptions for different parts of the compiler *)
exception C'est_La_Vie
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
exception Not_implemented_yet
(*-------------------------------------Semantic Analyzer-------------------------------------*)
exception Cannot_infer_expression_type of string
exception Exception of string
exception Already_declared
exception Name_not_found of string
exception Invalid_environment
exception Variable_not_found_in_scope of string
exception Function_not_defined of string
exception Cannot_pop_empty_variable_scope_stack
exception Variable_already_declared of string
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
exception Variable_scope_not_initialized
exception Empty_array_access
exception Invalid_array_expression
exception Array_elements_not_all_same_type
exception Not_a_valid_variable_declaration
exception Conditional_must_be_a_boolean
exception Invalid_accessor_value
exception Higher_order_function_call_only_takes_defg_functions
exception Cannot_perform_operation_on_string of string 
exception Cannot_perform_operation_on_array of string
exception No_Hof_Allowed
exception Cannot_pop_empty_expression_stack
exception Cannot_access_empty_expression_stack
exception Invalid_expression_stack_access
exception Defg_arrays_must_be_defined_with_constants
exception Casting_not_allowed_in_defg
exception Missing_return_type
exception Return_type_doesn't_match
(*-------------------------------------Codegen C-------------------------------------*)
exception Unknown_variable_type
exception Unknown_operator
exception Unknown_data_type
exception Unknown_type_of_param
exception Unknown_higher_order_function_call of string 
exception Unknown_type_of_vdecl 
exception Unknown_type_of_expression
exception Unknown_variable_statement
exception Unknown_type_of_statement
(*-------------------------------------Codegen PTX-------------------------------------*)
exception PTXCREMENT_GENERATED_ERROR
exception Value_return_ptx_test
