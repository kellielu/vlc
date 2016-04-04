{ 
	open Parser 
	exception Bad_dedent
	let indent_stack = Stack.create()

	let get_eof () = 
      let indent_length = Stack.length indent_stack - 1 in 
      DEDENT_EOF(indent_length)	
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let whitespace = [' ' '\t']
let newline = '\n' | "\r\n"

rule token = parse
	| newline { indent lexbuf}
	| whitespace { token lexbuf }

	| '(' { LPAREN }
	| ')' { RPAREN }
	| ':' { COLON }
	| '=' { ASSIGNMENT }
	| ("string" | "int") as input { DATATYPE(input) }
	| "return" { RETURN }
	| "def"	   { DEF }
	| (letter | '_')(letter | digit | '_')* as id { IDENTIFIER(id) }
	| '"' (([' '-'!' '#'-'&' '('-'[' ']'-'~'] | '\\' [ '\\' '"' 'n' 'r' 't' '''])* as stringliteral) '"' { STRING_LITERAL(stringliteral) }
	| digit* as integerliteral { INTEGER_LITERAL(int_of_string integerliteral) }
	| eof { get_eof() }

and indent = parse
	| whitespace* newline       { indent lexbuf }
	| whitespace* eof 			{ get_eof() }
	| whitespace* as indentation
		{
	        let indent_length = (String.length indentation) in
	        let stacktop_length = (Stack.top indent_stack) in
	        if indent_length > stacktop_length then
	          begin
	          Stack.push indent_length indent_stack;
	          INDENT
	          end
	        else if indent_length = stacktop_length then 
	          TERMINATOR
	        else
	          let count = 
	            let rec popped_from_stack counter =
	                if (Stack.top indent_stack) > indent_length then
	                    begin
	                    ignore(Stack.pop indent_stack);
	                    popped_from_stack (counter + 1)
	                    end
	                else if (Stack.top indent_stack) < indent_length then -1
	                else counter
	            in popped_from_stack 0
	          in 
	          if count = - 1 then raise (Bad_dedent)
	          else DEDENT_COUNT(count)
      }
	{
		Stack.push 0 indent_stack
	}