open Ast

let var_values = StringMap.make

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.expr Scanner.token lexbuf in
  print_endline (program)
