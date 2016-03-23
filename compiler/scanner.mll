(*
* VLC Scanner
*)

{ open Parser }

let num = ['0'-'9']

rule token = parse
(* Literals *)
| 
(* Comments *)
(* Identifiers *)
|
(* End of File *)
| eof {EOF}