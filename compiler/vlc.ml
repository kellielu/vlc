open Ast

let var_values = StringMap.make

let rec eval = function 
    Lit(x) -> x
  | Var(v) -> Array.get var_values v


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
