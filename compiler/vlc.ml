type action = Tokens | Ast | Compile

let _ = 
	if Array.length Sys.argv < 2 then
		print_string ("hi")
	else
    let action = List.assoc Sys.argv.(1) [ ("-t", Tokens);
                                           ("-a", Ast);
                                           ("-c", Compile) ] and
filename = Sys.argv.(2) in
let file_in = open_in filename in
      let lexbuf = Lexing.from_channel file_in in
      let token_list = Processor.get_tokens lexbuf in
      let processed_token_list = Processor.get_tokens_with_dedents token_list [] in
      let program = Processor.parser processed_token_list in
      match action with
          Tokens ->
            print_string (Utils.token_list_to_string processed_token_list)
        | Ast ->
            print_string (Utils.program_to_string program)
        | Compile ->
            print_string (Codegen.generate_program program ^ "\n")