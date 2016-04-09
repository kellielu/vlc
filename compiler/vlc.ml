type action = Tokens | Ast | Compile | Sast

let _ = 
	if Array.length Sys.argv < 2 then
  print_string (
      "Usage: vlc [mode] <source file>\n" ^
      "\t-t: Prints token stream\n" ^
      "\t-a: Pretty prints Ast as a program\n" ^
      "\t-s: Pretty prints Sast as a program\n" ^
      "\t-c: Compiles to C\n" )
	else
    let action = List.assoc Sys.argv.(1) [ ("-t", Tokens);
                                           ("-a", Ast);
                                           ("-s", Sast);
                                           ("-c", Compile) ] and
filename = Sys.argv.(2) in
let file_in = open_in filename in
      let lexbuf = Lexing.from_channel file_in in
      let token_list = Processor.get_token_list lexbuf in
      let program = Processor.parser token_list in
      let sast = Semant.analyze program in
      match action with
          Tokens ->
            print_string (Utils.token_list_to_string token_list)
        | Ast ->
            print_string (Utils.program_to_string program)
        | Sast -> 
            print_string (Utils.sast_to_string sast)
        | Compile ->
            print_string (Codegen.generate_program sast ^ "\n")
