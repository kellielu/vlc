open Parser

exception MissingEOF
let last_token = ref EOF

let token_to_string = function
    TERMINATOR -> "TERMINATOR" | INDENT -> "INDENT"
  | DEDENT -> "DEDENT" | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN" | COLON -> "COLON"
  | COMMA -> "COMMA" 
  | DEF -> "DEF"
  | ASSIGNMENT -> "ASSIGNMENT" 
  | EOF -> "EOF" 
  | IDENTIFIER(s) -> "IDENTIFIER(" ^ s ^ ")"
  | INTEGER_LITERAL(i) -> "INTEGER_LITERAL(" ^ string_of_int i ^ ")"
  | DEDENT_COUNT(i) -> "DEDENT_COUNT(" ^ string_of_int i ^ ")"
  | STRING_LITERAL(s) -> "STRINGLITERAL(" ^ s ^ ")"
  | RETURN -> "RETURN"
  | DATATYPE(a) -> "DATATYPE(" ^ a ^ ")"
  | DEDENT_EOF(i) -> "DEDENT_EOF(" ^ string_of_int i ^ ")"


 let token_list_to_string token_list = 
  let rec helper token_list acc_string = 
    if(List.length (token_list)) = 0 then
      acc_string
    else
      helper (List.tl token_list) ((token_to_string(List.hd token_list)) ^ "\n" ^ acc_string)
  in 
  helper token_list ""


(* Gets the original tokens from the scanner *)
let get_tokens lexbuf = 
  let rec next lexbuf token_list = 
  match Scanner.token lexbuf with
    | DEDENT_EOF(c) as eof-> eof :: token_list
    | t -> t :: (next lexbuf token_list)
  in next lexbuf []

(* Replaces DEDENT_COUNT with DEDENTS *)
let rec get_tokens_with_dedents original_token_list new_token_list= 
  let rec fill_dedent count mylist = 
    if count <= 0 then mylist 
    else 
      fill_dedent (count-1) (List.rev(DEDENT::List.rev(mylist)))
  in
  if (List.length(original_token_list)) != 0 then
    match (List.hd original_token_list) with
      | DEDENT_COUNT(c) -> 
        let temp = fill_dedent c new_token_list in
        get_tokens_with_dedents (List.tl original_token_list) temp
      | DEDENT_EOF(c) ->
        let temp1 = (List.rev (TERMINATOR::(List.rev new_token_list))) in
        let temp = fill_dedent c temp1 in
        List.rev(EOF::(List.rev temp));
      | _ as token -> get_tokens_with_dedents (List.tl original_token_list) (List.rev (token :: (List.rev new_token_list)))
  else
    new_token_list

(* Function that uses above two functions *)
let get_token_list lexbuf = 
  let original_token_list = get_tokens lexbuf in 
  let new_token_list = get_tokens_with_dedents original_token_list []
in new_token_list

(* Parse function *)
let parser token_list = 
  let token_list = ref(token_list) in
  let tokenizer _ =
    match !token_list with
        | head :: tail -> 
            last_token := head;
            token_list := tail;
            head
        | [] -> raise (MissingEOF) in
  let program = Parser.program tokenizer (Lexing.from_string "") in 
  program
