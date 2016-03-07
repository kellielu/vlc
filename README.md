# VLC : Language Reference Manual
### Table Of Contents
##### [1. Introduction](#introduction)
##### [2. Types and Declarations](#types-and-declarations)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[2a. Primitives](#primitives)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[2b. Non-primitives](#non-primitives)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Strings](#strings)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Arrays](#arrays)

##### [3. Lexical conventions](#lexical-conventions)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3a. Whitespace](#whitespace)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3b. Comments](#comments)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3c. Identifiers](#identifiers)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3d. Keywords](#keywords)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3e. Literals](#literals)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Integer Literal](#integer-literals)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Floating Point Literal](#floating-point-literals)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Boolean Literals](#boolean-literals)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Character Literals](#character-literals)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- String Literals](#string-literals)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3e. Separators](#separators)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3f. Functions](#functions)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Regular Functions](#regular-functions)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- GPU Functions](#gpu-functions)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- Higher Order Functions](#higher-order-functions)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Map](#map)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Reduce](#reduce)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[3g. Casting](#casting)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Primitive Types](#primitive-types)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Non-Primitive Types](#non-primitive-types)

##### [4. Syntax](#syntax)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[4a. Control Flow](#control-flow)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[- If, Elif, and Else Statements](#if,-elif,-and-else-statements)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Ternary Operator](#ternary-operator)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-While Loops](#while-loops)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-For Loops](#for-loops)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[4b. Scope](#scope)

##### [5. Expressions](#expressions)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[5a. Arithmetic Operators](#arithmetic-operators)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Traditional Arithmetic Operators](#traditional-arithmetic-operators)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Array Arithmetic Operators](#array-arithmetic-operators)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Scalar Array Arithmetic Operators](#scalar-array-arithmetic-operators)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[5b. Logic Operators](#logic-operators)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[5c. Operator Precedence and Associativity](#operator-precedence-and-asociativity)

##### [6. External Declarations](#external-declarations)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[6a. `main` function and Code Execution](#`main`-function`-and-code-execution)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[6b. `import` Statements](#`import`-statements)

<div style="page-break-after: always;"></div>

# Introduction
VLC is a Python-like high level language for GPU(Graphical Processing Unit) programming on Nvidia GPUs.

VLC is primarily intended for numerical computation, which can be performed orders of magnitude faster on parallelizable GPU architecture than on traditional x86 architecture. VLC is intended to provide convenient and safe access to the GPU’s computational power by abstracting common lower level operations - for example, data transfer between the CPU and the GPU - from the user.  

Other functionality provided by VLC include built-in higher order `map` and `reduce` functions that utilize the parallel capabilities of a GPU.

<div style="page-break-after: always;"></div>
# Types and Declarations

The VLC language has two data types: primitives and non-primitives. 

## Primitives

` <primitive_type> myVar = <value>` declares a primitive type *primitive_type* called *myVar* with value *value*

A primitive cannot be *null*.

| Primitive | Description
---         | ---
| `byte` | An 8-bit signed two's complement integer between -128 and 127
| `int`       | 32-bit signed two's complement integer between -2147483647 and +2147483647
| `float`     | Single precision 32-bit IEEE 754 floating point number with absolute value between 1.4E-45 and 3.4028235E38                    
| `bool`      | A Boolean `true` or `false`                                
| `char`      | 16-bit alphanumeric character, valid escape `"\"` character, or punctuation in the ASCII character set 

## Non-primitives
Declared but uninitialized non-primitives assume a `null` value. Non-primitives cannot be declared `null`, but can only take on the `null` value if they have not been initialized. 

### Strings
| Non-Primitive  | Description 
---              |---
| `string`       | A sequence that can be made of characters, valid escape `"\"` characters, or punctuation, immutable. 

`string myString = "This is a string"` declares a string with name `myString` and value *"This is a string"*

### Arrays
Arrays are objects hold a fixed number of primitives or non-primitives. All elements must be values of a single type, unless otherwise specified for special cases.

| Non-Primitive | Description
---     | ---
| ` <type> [] myArray`                                  | 1-Dimensional array of type *type*
| ` <type> [][] my2DArray`                       | 2-Dimensional array of type *type*
| ` <type> [][][]...[] myArray`                         | n-Dimensional array of type *type*
For any array `a`, `a[i][j][k]...` retrieves the element at the ith index of the first dimension, jth index of the second, kth at the third etc.

| Array Declarations | Description
---     | ---
| ` <type>[][] my2DArray = block(myArray,n)`            | 2-Dimensional array created from `myArray` by blocking every n-elements of `myArray`
| ` <type> [n] myArray = {<type1>,<type2>,<type3>...}`  | Initializes `myArray` with n user-specified *type*
| ` <primitive_type> [n] myArray = {0}`                | Initializes `myArray` with n zeros
| ` <primitive_type> [n] myArray = {*}`                | Initializes `myArray` with n random *primitive_type* 

<div style="page-break-after: always;"></div>
# Lexical Conventions
## Whitespace
Whitespace refers to the space, horizontal tab, form feed and new line characters. White space is used to separate tokens as well as determine scope. Other than in these uses, it is ignored.

*WHITESPACE* = `[' ' '\n' '\r' '\t']`

Like Python, whitespace found after a newline in VLC denotes the scope of a statement. A statement located within a scope of another statement should contain at least one recognized delimiting white space character at its start.

*DELIMITING WHITESPACE* = `[ ' ' '\t']`

See below for an example.

    if (i!=0):
        i = i + 1


(Note `i = i + 1` is not aligned with `if` control statement, but begins several white spaces to the right of the line. This defines `i = i + 1` to  be in the scope of the `if` block)
VLC allows tabs and interprets them as four spaces.

Multi-line statements can be achieved through the use of the line-join character `\`, as shown in the following example:

    if \
    (i=0): 
        i = i + 1

## Comments
VLC comments follow standard comment conventions of C, C++, and Java.

`//` denotes single line comments.

`/*` and `*/` denote start and termination of multi-line comments. 

Per C, C++, and Java comment conventions, comments cannot be nested within each other. For example the sequence `/* /* */ */` is not fully recognized as a comment. Only the substring `/* /* */` is recognized as a comment.

*COMMENT* = `'/' '*'+ [^'*']* '*'+ '/'    |     '/' '/' [^'\n']*`

## Identifiers
An identifier is a case-sensitive sequence of characters consisting of letters, numbers, or underscore, and the first character in an identifier cannot be a number. 
Identifiers may not take the form of reserved keywords.

*ID* = `['a'-'z' 'A'-'Z' '_'  ] ['a'-'z' 'A'-'Z' '_' '1'-'9']*`

## Keywords
`int`  `float` `char` `bool`  `string` `null` `if` `elif` `else` `for`  `while` `continue` `break` `return` `auto` `map` `reduce` `name` `def` `defg` `import` `const` 

## Literals
#### Integer Literals
An integer constant is an optionally signed sequence of digits.  An integer constant can take the form of a `byte` or `int`primitive. A `byte` primitive ranges from -128 to 127 and an  `int` primitive ranges from -2147483648 to 2147483648.

*INT* = `[‘+’ ‘-’]?[‘0’-’9’]+`

#### Floating Point Literals
A floating point constant is denoted by an optionally signed integer, a decimal point, a fraction part, an "e" or "E" and an optionally signed exponent. A floating point constant can take the form `float`. A `float` primitive's absolute value ranges from approximately 1.4E-45 to 3.4E38.

Either the fraction part or the integer part must be present, and either the decimal point or the "e" and signed exponent must be present.

*FLOAT* = 

      ['+' '-']?['0'-'9']+'.'['0'-'9']*(['e' 'E']['+' '-']?['0'-'9']+)? 
    | ['+' '-']?['0'-'9']*'.'['0'-'9']+(['e' 'E']['+' '-']?['0'-'9']+)?
    | ['+' '-']?['0'-'9']['e' 'E']['+' '-']?['0'-'9']+
#### Boolean Literals
A boolean has two possible values, true or false. These are denoted by the identifiers "true" and "false".

*BOOL* = `'true'|'false'`

#### Character Literals
A character literal is denoted by enclosing single quotes ' ', and can be constructed from alphanumeric characters, traditional punctuation characters, and the specified valid escape characters. 

| Valid Escape Sequence | Description
---| ---
| `\’` | Single quote
| `\"` | Double quote
| `\\` | Backslash 
| `\n` | New Line
| `\r` | Carriage Return
| `\t` | Horizontal Tab

*CHAR* = `''' ([' '-'!' '#'-'&' '('-'[' ']'-'~'] | '\\' ['\\' '"' 'n' 'r' 't' ''']) '''`
### String Literals
A string constant is denoted by enclosing double quotes " ", and can be constructed from alphanumeric characters, traditional punctuation characters, and the specified valid escape characters. 

| Valid Escape Sequence | Description
---| ---
| `\’` | Single quote
| `\"` | Double quote
| `\\` | Backslash
| `\n` | New Line
| `\r` | Carriage Return
| `\t` | Horizontal Tab

*STRING* = `'"' ([' '-'!' '#'-'&' '('-'[' ']'-'~'] | '\\' [ '\\' '"' 'n' 'r' 't' '''])* '"'`

### Separators
A separator is a character that separates tokens. White space is also used as a separator, unless it is defining scope. 

| Character | Separator
---| ---
| `'('`| {LPAREN}
| `')'` | {RPAREN}
| `':'` | {COLON}
| `'['` | {LBRACKET}
| `']'` | {RBRACKET}
| `'.'` | {DOT}
| `','` | {COMMA}


### Functions
#### Regular Functions
Functions are declared using the `def` keyword, and must specify their arguments and argument types, return type, and end in a colon`:`. Functions cannot be declared within other functions.

The scope of a function is defined by whitespace - that is, all statements that are part of the function cannot be aligned with the function declaration, but must be "indented", or prefaced by at least one whitespace character to be defined within the function scope.  

All function arguments that are primitive types are passed by value, meaning all arguments are copied to the function. This means that changes to the argument within the function will not change the argument's value outside of the function.

All function arguments that are non-primitive types are passed by reference, meaning changes to the argument will change the argument's value outside of the function.

Function declaration:
`<return type> def <function name>(<type1> arg1, <type2> arg2...):`

#### GPU Functions
The GPU function `defg` creates a user-defined function that is meant to be run on the GPU kernel. `defg` functions must specify arguments and argument types, return type, and end in a colon `:`. A `defg` function cannot be declared within other functions and may not call other functions. These `defg` functions will be called by the higher-order functions `map` and `reduce`.

There are N array-dependent arguments that must be declared in `defg` that will be called by `map` function taking N arrays as input. There are exactly two array-dependent arguments that must be declared in a `defg` that will be called by the `reduce` function. (See below for a more detailed explanation.) Each array-dependent argument is an identifier for a single element in the array(s) that are being handled by `map` and `reduce`.

Besides array-dependent arguments, `defg` also takes constant arguments as input, which are defined as an argument list into `defg` using the keyword `const`. These `const`arguments cannot be changed by the `defg` function, but can be referenced by the `defg` function. The type of the `const` argument and an `ID` for the argument must be specified in the declaration of a `defg` function, and the values for `const` arguments must be provided in the `defg` function when it is called by `map` or `reduce`. (See the example for `map` to understand how to use `const`) 

Furthermore, these `const` arguments are only copied once to global memory in the GPU kernel so that they can be accessed by all threads. 

GPU function declaration:
`<return type> defg <function name> (<type1> arg_1, <type2> arg_2...):`


`<return type> defg <function name> (<type1> arg, const = const[<type1> const_arg1, <type2> const_arg2 ...]):`

#### Higher Order Functions
VLC contains built-in higher order functions `map` and `reduce` which take a `defg` and arrays as arguments. These built-in higher order functions provide needed abstraction for users who do not wish to be boggled by the specifics of GPU computing but still want to take advantage of GPU parallelism.

The first parameter in a `map` or `reduce` function must be a `defg`. For the remaining parameters, `reduce` takes in only one array as the second input, but `map` may take a variable number of arrays so long as they are the same dimensions. All input arrays may not be `null`. 

If the input arrays are multi-dimensional, each dimension must have fixed-length rows.  
The output of `map` is an N-dimensional array of the same size as all the input arrays to `map`, where `defg` has been applied to the element in the corresponding index as the output. 
The output of `reduce` is one element obtained by performing pair-wise reduction on adjacent members of the input array to `reduce`.

Given the nature of pairwise reduce algorithm, in order to obtain correct results, the `defg` function inputted to `reduce` should be commutative.

`map` and `reduce` are also reserved keywords and may not be used by the user to define any other variable, constant, or function.

| Higher Order Function  | Description 
---              |---
| `map(<defg>, <array1>, <array2>...)`    |  Function that takes as input a function `defg` with X open paremeters, and X N-dimensional arrays, performs `defg` on the X arrays and returns one resulting N-dimensional array of the return type of `defg`. 
| `reduce(<defg>, <array>)` |  Function that takes as input a function `defg` with two open paremeters and one array, performs pairwise reduction on every pair in the array, and returns final reduced result of the return type of `defg`

Functions `defg` passed to `map` and `reduce`:

1) Must have the corresponding number of arguments specified by map (X) and reduce (two)

2) Must have arguments that are the same type as the the array(s) passed into map and reduce. In the case of map, the order of the argument types to `defg` should match the order of the arrays inputted into `map`.

3) Must specify any `const` arguments using `const=[arg1, arg2,arg3...]` when passed to `map` or `reduce`

##### Map
    bool defg inPlane(float x, float y, float z,const=[float a, float b, float c, float d]):
        return a*x + b*y + c*z == d
        
    main(): 
        int [4] x_coords = {3,-2,5,1}
        int [4] y_coords = {4,-1,6,9}
        int [4] z_coords = {6,0,-4,1}
        bool [4] point_in_plane = map(inPlane(const=[3,-2,3,18], x_coords, y_coords, z_coords) 
        print point_in_plane // {false, false, false, true}


##### Reduce
    int defg sum(int x, int y):
        return x+y
    
    main():
        int [4] numbers = {0,1,2,3}
        int sum = reduce(sum,numbers)
        print sum // 6

### Casting
#### Primitive Types
`byte`,`int`, `float` are primitive types that can be cast to each other. When casting from lower-bit type to a higher-bit type, for example from a `byte` to a `int`, there is no loss of precision. Likewise, casting a higher-bit-type to a lower-bit-type with a value that fits into the lower-bit-type will also generate no loss of precision.

For `int` to `byte` conversions, the latter 8 bits of the `int` are set as the value of the `byte` and the former 24 bits of the `int` are dropped. Performing an unsafe conversion between `int` and `byte` can cause the program to execute falsely. 

Casting from a floating-point-type,`float`, to an integer,`byte` or `int`, type drops the fractional part of the floating point type.

#### Non-primitive Types
VLC is a strongly typed language, and does not allow casting between non-primitive types.


# Syntax
## Control Flow

### If, Elif, and Else Statements

VLC uses standard `if` `else` `elif` control statements. These control statements take a boolean expression as input, and execute branching according to the value of the boolean expression. 

An `if` may be followed by optional multiple `elif` statements and an optional`else` statement, and `if` and `elif` statements need not be concluded with an `else`.

Furthermore, every `if`,`else`, and `elif` block defines a new scope, which is determined by white space characters. `if`,`elif`, and `else` can also be nested in other `if`,`elif`, and `else` loops. 

The below example demonstrates proper use of`if`,`elif`, and `else` loops.
##### Example:
    int num = 5
    if(num < 5):
        print "Number is less than five!"
    elif(num >= 5 and num < 10):
        print "Number is between five and ten!"
    elif(num >= 1 and num < 20):
        print "Number is between ten and twenty!"
    else:
        print "Number is twenty or greater!"

#### Ternary Operator
VLC also provides a shortcut `if` `else` ternary operator. The below example shows a case setting integer `x` to `<valueA>` if `<condition>` is true, and set to `<valueB>` if `<condition>` is false.
##### Example:
`int x = <valueA> if (<condition>=true) else <valueB>`

### While Loops

VLC supports traditional `while` loops that take a boolean expression `condition` as an input. The substatements within the scope of a while loop are repeated so long as the `condition` evaluates to true. 

Scope within a `while` loop is defined by white space characters. See White Space section for further clarification. 

Users can break out of a `while` loop using the `break` keyword, or skip to the next iteration of a `while` loop using the `continue` keyword.

A `while` loop in VLC has the following syntax:

##### Example
    int x = 5
    while ( x > 1 ):
        x = x - 1 
    print x // 1



### For Loops
`for` loops in VLC take as input an `iterator assignment`, a boolean expression `condition`, and an `iterating statement` each separated by a comma `,` . 

Scope within a `for` loop is defined by preceding white space characters. See White Space section for further clarification. 

The substatements within the `for` loop will execute if `condition` is `true`, with the next iteration of the loop increasing the iterator defined in the `iterator assignment` by the `iterating statement`.

Users can break out of `for` loop iteration using the `break` keyword, or skip to the next iteration of a `for` loop using the `continue` keyword.

In essence, VLC supports traditional `for` loops that follow the below structure.
        
##### Example:
        int [5] array;
        for(int i = 0, i < 5, i++):
            array[i] = i * i
        print array //0 1 4 9 16 
## Scope
Scoping in VLC is static, and follows the conventions of block-level scoping. 
Variables defined at the top level of a program are available in the global scope of the program.
<div style="page-break-after: always;"></div>
# Expressions
## Arithmetic Operators
### Traditional Arithmetic Operators

Traditional arithmetic operators can be used between two primitives of type `byte` `int` `long` `float` or `double` . Operators must be used between two elements of the same primitive type. 

| Traditional Arithmetic Operators  | Description 
|---   | ---
| `+`  | Addition operator
| `-`  | Subtraction operator
| `/`  | Division operator
| `*`  | Multiplication operator
| `%`  | Modulo operator
| `^`  | Exponent/Power operator
| `log(<type>)` `log(<type>,n)`| Logarithmic operator
| `<<` | Bitshift left
| `>>` | Bitshift right

### Array Arithmetic Operators
Array arithmetic operators can be used between two arrys consisting of primitive types `byte` `int` `long` `float` or `double`. Operators must be used between two arrays that are of equal length and that contain the same primitive type.

| Array Arithmetic Operators | Description
| ---| ---
| `arr1+arr2` | Pairwise element addition on two arrays of equal length, returns array of equal length
| `arr1-arr2` | Pairwise element subtraction on two arrays of equal length, returns array of equal length
| `arr1/arr2` | Pairwise element division on two arrays of equal length, returns array of equal length
| `arr1*arr2` | Pairwise element multiplication on two arrays of equal length,returns array of equal length
| `arr1.arr2` | Dot product on two arrays of equal length
| `arr1**arr2`  | Matrix multiplication on two arrays of appropriate dimensions for matrix multiplication, works on arrays up to 2-Dimensional arrays

### Scalar Array Arithmetic Operators
Scalar array arithmetic operators can be used between an array that contains primitive types of  `byte` `int` `long` `float` or `double` and a scalar factor of primitive type  `byte` `int` `long` `float` or `double`.  
Elements in the array and the scalar factor must be of the same primitive type. 

| Scalar Array Arithmetic Operators| Description
|---|---
| `array + n` | Adds scalar factor *n* to every element in array, returns array of equal length 
| `array - n` | Subtracts scalar factor *n* from every element in array, returns array of equal length
| `array / n` | Divides every element in array by scalar factor *n* , returns array of equal length
| `array * n` | Multiples every element in array by scalar factor *n* ,returns array of equal length
| `array ^ n`| Raises every element in array to power of scalar factor *n* , returns array of equal length
| `log(array)` `log(array,n)`| Takes log of every element in array in base *e* or in base of scalar factor *n* , returns array of equal length

## Logic Operators
VLC supports the following logic operators, which are most often used as arguments in control statements `if` `elif` `else` `while` and `for`.

| Logic Operators 
|---
|`and` `or` `not` `xor` `!=` `==` `>=` `<=` `>` `<`|

`and` and `or` logic operators are evaluated using short circuiting principles.

## Operator Precedence and Associativity

Operators are listed below from highest to lowest precedence, and operators listed on the same level share the same level of precedence

| Operator Hierarchy | Operators
| ---|---
|1| Logarithmic `log`, Power `^`, Dot Product for Arrays `.`, Matrix Multiplication for 2D Arrays `**`
|2| `*` (Multiplication), `/`(Division)
|3| Addition `+`, Subtraction `-`
|4| Bitshift Operators `<<`, `>>`
|5| Relational Logic Operators `and` ,`or`, `not` ,`xor`, `==`,`>=`, `<=`, `<`, `>` 
|6| Assignment `=`

The `=` assignment operator is right associative. All other operators are left-associative.

<div style="page-break-after: always;"></div>
# External Declarations
## `main` function and Code Execution
VLC code execution begins at global statements, and then proceeds to execute at a predefined `main` function in the file.


## `import` Statements

The `#import` keyword allows VLC to import code from other VLC files.
When importing other VLC files, `main` functions are ignored in the imported files.

For example, if we have file *a.vlc* that imports *b.vlc* , any `main` function in *b.vlc* will be ignored. 





