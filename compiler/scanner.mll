{ 
	open Parser 
	let line_number = ref 1
	let indent_stack = Stack.create()

	let get_eof () = 
		let indent_stack_length = (Stack.length indent_stack) - 1 in 
		DEDENT_EOF(indent_stack_length)
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let whitespace = [' ' '\t']
let newline = '\n' | "\r\n"

rule token = parse
	| newline {incr line_number; indent lexbuf}
	| whitespace { token lexbuf }

	| '(' { LPAREN }
	| ')' { RPAREN }
	| ':' { COLON }
	| '=' { ASSIGNMENT }
	| "string" | "int" as datatype { DATATYPE(datatype) }
	| "return" { RETURN }
	| "print"  { PRINT }
	| (letter | '_')(letter | number | '_')* as id { IDENTIFIER(id) }
	| '"' ([' '-'!' '#'-'&' '('-'[' ']'-'~'] | '\\' [ '\\' '"' 'n' 'r' 't' '''])* as stringliteral '"' { STRING_LITERAL(stringliteral) }
	| numeric* as integerliteral { INTEGER_LITERAL(integerliteral) }
	| eof { get_eof () }

and indent = parse
	| whitespace* newline 		{ incr line_number; indent lexbuf }
	| whitespace* eof 			{ get_eof() }
	| whitespace* as indentation
		{
			let indent_length = String.length indentation in 
			let stacktop_length = String.top indent_stack in 
			if indent_length > stacktop_length then
				begin
				Stack.push indent_length indent_stack
				INDENT
				end
			else if indent_length = stacktop_length then
				TERMINATOR
			else
				let number_popped = 
					let rec popped_from_stack count =  
						if(Stack.top indent_stack) > indent_length then
							begin
							ignore(Stack.pop indent_stack)
							popped_from_stack (count + 1)
							end
						else if (Stack.top indent_stack) < indent_length then -1
						else count
					in popped_from_stack 0
				in
				DEDENT_COUNT(number_popped)
		}
	{
		Stack.push 0 indent_stack
	}