{ 
	open Parser 
	exception Bad_dedent
	let indent_stack = Stack.create()
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
	| "print"  { PRINT }
	| (letter | '_')(letter | digit | '_')* as id { IDENTIFIER(id) }
	| '"' ([' '-'!' '#'-'&' '('-'[' ']'-'~'] | '\\' [ '\\' '"' 'n' 'r' 't' '''])* as stringliteral '"' { STRING_LITERAL(stringliteral) }
	| digit* as integerliteral { INTEGER_LITERAL(int_of_string integerliteral) }
	| eof { EOF }

and indent = parse
	| whitespace* newline       { indent lexbuf }
	| whitespace* eof 			{ EOF }
	| whitespace* as indentation
		{
			let indent_length = String.length indentation in 
			let stacktop_length = Stack.top indent_stack in 
			if indent_length > stacktop_length then
				begin
				Stack.push indent_length indent_stack;
				INDENT
				end
			else if indent_length = stacktop_length then
				TERMINATOR
			else
			    let result =
    				let rec popped_from_stack =  
    					if(Stack.top indent_stack) > indent_length then
    						begin
    						ignore(Stack.pop indent_stack);
    						ignore(DEDENT);
    						popped_from_stack 
    						end
    					else if (Stack.top indent_stack) < indent_length then -1
    					else 1
    				in popped_from_stack
    			in
				if result = -1 then raise (Bad_dedent)
                else TERMINATOR

		}
	{
		Stack.push 0 indent_stack
	}