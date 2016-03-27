%{ open Ast %}

%token TERMINATOR INDENT DEDENT
%token LPAREN RPAREN COLON
%token ASSIGNMENT
%token RETURN
%token PRINT
%token <string> DATATYPE

%token <string> STRING_LITERAL
%token <int> INTEGER_LITERAL

%token <string> IDENTIFIER

%token <int> DEDENT_COUNT
%token <int> DEDENT_EOF


%right ASSIGNMENT

%start program
%type < Ast.program> program

%%

program:								/* [var_decls], [func_decls] */
	| 									{ [], [] } 	/* empty program */
	| program variable_statement 		{ ($2 :: fst $1), snd $1 } 	/* */
	| program func_decl 				{ fst $1, ($2 :: snd $1) }

identifier:
	| IDENTIFIER { Identifier($1) }

datatype:
	| DATATYPE  {string_to_variable_type $1}


expression:
	| identifier LPAREN expression list RPAREN 			{ Func_Call($1,[]) }
	| STRING_LITERAL        							{ String_Literal($1) }
	| INTEGER_LITERAL									{ Integer_Literal($1) }