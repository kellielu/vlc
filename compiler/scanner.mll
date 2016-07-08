(* Notes: Need to add exceptions for scanner errors *)
{ 
	open Parser 
	(* open Exceptions *)

	let indent_stack = Stack.create()

	let get_eof () = 
      let indent_length = Stack.length indent_stack - 1 in 
      DEDENT_EOF(indent_length)	
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let whitespace = [' ' '\t']
let sign = ['+' '-']
let exp = ['e' 'E']
let newline = '\n' | "\r\n"

rule token = parse
    | whitespace* "//"         			{ single_line_comment lexbuf }
  	| whitespace* "/*"         			{ multi_line_comment lexbuf }
	| newline 							{ indent lexbuf }
	| whitespace 						{ token lexbuf }

	(* Punctuation *)
	| '(' 		{ LPAREN }
	| ')' 		{ RPAREN }
	| ':' 		{ COLON }
	| '=' 		{ ASSIGNMENT }
	| '[' 		{ LBRACKET }
	| ']' 		{ RBRACKET }
	| '{' 		{ LCURLY }
	| '}' 		{ RCURLY }
	| ',' 		{ COMMA }	

	(* Arithmetic Operators *)
	| '+' 		{ ADD }
	| '-' 		{ SUBTRACT }
	| '*' 		{ MULTIPLY }
	| '/' 		{ DIVIDE }
	| '%' 		{ MODULO }
	| '.'		{ DOT }
	| '^' 		{ EXPONENT }
	| "**" 		{ MATRIX_MULTIPLICATION }

	| "+=" 		{ ADD_EQUAL }
	| "-="		{ SUBTRACT_EQUAL } 
	| "/="		{ DIVIDE_EQUAL }
	| "*="		{ MULTIPLY_EQUAL }

	(* Bit Operators *)
	| ">>" 		{ BITSHIFT_RIGHT }
	| "<<"		{ BITSHIFT_LEFT }
	| "&"		{ BITWISE_AND}
	| "|" 		{ BITWISE_OR }

	(* Logic Operators *)
	| "and"  	{ AND }
	| "or"	 	{ OR }
	| "not" 	{ NOT }
	| "xor"	 	{ XOR }

	(* Comparison Operators *)
	| "==" 		{ EQUAL }
	| "!="		{ NOT_EQUAL }
	| ">"		{ GREATER_THAN }
	| ">="		{ GREATER_THAN_EQUAL }
	| "<"		{ LESS_THAN }
	| "<=" 		{ LESS_THAN_EQUAL }

	(* Datatypes *)
	| (   "string"  
		| "float" 	| "double"
		| "bool" 	| "void" 
		| "ubyte" 	| "byte"
		| "uint"	| "int" 
		| "ulong" 	| "long" ) as input { DATATYPE(input) }

	(* Conditionals and Loops *)
	| "if" 			{ IF }
	| "else"   		{ ELSE }
	| "for" 		{ FOR }
	| "while"		{ WHILE }
	| "break"		{ BREAK }
	| "continue"	{ CONTINUE }

	(* Function Declarations and Attributes *)
	| '~' 			{ TILDA }
	| "return" 		{ RETURN }
	| "def"	   		{ DEF }
	| "defg"   		{ DEFG }
	| "consts" 		{ CONSTS }

	| ("true" | "false") as booleanlit 																											{ BOOLEAN_LITERAL(bool_of_string booleanlit) }
	| '"' (([' '-'!' '#'-'&' '('-'[' ']'-'~'] | '\\' [ '\\' '"' 'n' 'r' 't' '''])* as stringlit) '"' 											{ STRING_LITERAL(stringlit) }
	| digit+ as intlit 																															{ INTEGER_LITERAL(int_of_string intlit) }
	| (digit+ '.' digit* | '.' digit+ | digit+ ('.' digit*)? 'e' '-'? digit+ | '.' digit+ 'e' '-'? digit+) as fplit 							{ FLOATING_POINT_LITERAL(float_of_string fplit) }
	| (letter | '_')(letter | digit | '_')* as id 																								{ IDENTIFIER(id) }
	| eof 																																		{ get_eof() }

(* Blocks for comments *)
and single_line_comment = parse
	| newline 							{ indent lexbuf }
	| eof 								{ get_eof() }
	| _ 								{ single_line_comment lexbuf }

and multi_line_comment = parse
	| newline 							{ multi_line_comment lexbuf }
	| "*/"								{ token lexbuf }
	| _ 								{ multi_line_comment lexbuf }

(* Block for handling white space delimiting *)
and indent = parse
	| whitespace* newline       		{ indent lexbuf }
	| whitespace* eof 					{ get_eof() }
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
	          	(* Function that pops indent lengths from the stack until we reach the appropriate indent length *)
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
	          if count = - 1 then raise (Exceptions.Bad_dedent)
	          else DEDENT_COUNT(count)
      }
	{
		Stack.push 0 indent_stack
	}

