type action = Tokens | Ast | Compile | Sast | Run

let _ = 
	if Array.length Sys.argv < 2 then
  print_string (
      "Usage: vlc [mode] <VLC program file>\n" ^
      "\t-t: prints tokens read in by scanner\n" ^
      "\t-a: prints ast as a program\n" ^
      "\t-s: prints sast as a program\n" ^
      "\t-c: compiles VLC program to CUDA C file and PTX files\n" ^
      "\t-r: compiles and runs VLC program\n" )
	else
    let action = List.assoc Sys.argv.(1) [ ("-t", Tokens);
                                           ("-a", Ast);
                                           ("-s", Sast);
                                           ("-c", Compile);
                                           ("-r", Run)] and
filename = Sys.argv.(2) in
let file_in = open_in filename in
      let lexbuf = Lexing.from_channel file_in in
      let token_list = Processor.get_token_list lexbuf in
      let program = Processor.parser token_list in
      let sast = Semant.analyze program in
      match action with
        | Tokens ->
            print_string (Utils.token_list_to_string token_list)
        | Ast ->
            print_string (Utils.program_to_string program)
        | Sast -> 
            print_string (Utils.sast_to_string sast)
        | Compile ->
            ignore(Codegen_c.generate_program filename sast)
(*         | Run -> Acts as an interpreter *)
