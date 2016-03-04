# VLC : Language Reference Manual

# Introduction
VLC is a Python-like high level language intended to operate on Nvidia Graphical Processing Units ( GPUs ). 
The primary implementation for VLC is for numerical computation, which can be performed orders of magnitude faster on parallelizable GPU architecture than on x86. 
VLSC is intended to provide convenient and safer access to the GPU’s computational power by circumventing some of the lower level minutiae of CUDA such as data transfer between the CPU and GPU.
Other functionality provided by VLC includes map/reduce, and filtering. 

# Lexical Conventions
## Comments
VLC comments follow standard comment conventions of C, C++, and Java.

`//` denotes single line comments.

`/*` and `*/` denote start and termination of multi-line comments.
## Identifiers
An identifier is a case-sensitive sequence of characters consisting of letters, numbers, or underscore. The first character in an identifier cannot be a number. 

## Keywords
`int` `long`  `float` `double` `char` `bool` `continue` `break` `if` `double` `else` `for` `auto` `while` `return` `map` `reduce` `name` `def`

## Whitespace
Like Python, whitespace in VLC denotes the scope of a statement.
A statement located within a scope of another statement should contain at least one space character at its start.

See below for an example.

    if (i!=0):
      i = i + 1


(Note `i = i + 1` is not aligned with `if` control statement, but begins several white spaces to the right of the line.)
VLC allows tabs, and interprets them as four spaces.

## Constants
### Integers
An integer constant is sequence of digits that does not begin with 0.
### Floating Point Constants
A floating point constant is denoted by an optionally signed integer, a decimal point, a fraction part, an "e" or "E" and an optionally signed exponent.
Either the fraction part or the integer part must be present, and either the decimal point or the "e" and signed exponent must be present.
### Strings
A string constant is denoted by enclosing quotes " ".


# Types and Declarations
## Primitives
| Primitive | Description
---         | ---
| `int`       | 32-bit signed integer
| `long`      | 64-bit signed integer                                                     
| `float`     | Single precision 32-bit IEEE 754 floating point number                    
| `double`    | Double precision 64-bit IEEE 754 floating point number                    
| `bool`      | A Boolean `true` or `false`                                                             
| `char`      | 16-bit alphanumeric character, valid escape `"\"` character, or punctuation 

` <primitive_type> myVar = <value>` declares a primitive type *primitive_type* called *myVar* with value *value*

## Non-primitives
### String
| Non-Primitive  | Description 
---              |---
| `string`       | A sequence that can be made of characters, valid escape `"\"` characters, or punctuation, immutable

`string myString = "This is a string"` declares a string with name `myString` and value *"This is a string"*

### Arrays
Objects that hold a fixed number of values of a single type

| Non-Primitive | Description
---     | ---
| ` <type> [] myArray`                                  | 1-Dimensional array of *type*
| ` <type> [][] my2DArray`                              | 2-Dimensional array of *type*
| ` <type> [][][]...[] myArray`                         | n-Dimensional array of *type*

| Array Declarations | Description
---     | ---
| ` <type>[][] my2DArray = block(myArray,n)`            | 2-Dimensional array created from `myArray` by blocking every n-elements of `myArray`
| ` <type> [n] myArray = {<type1>,<type2>,<type3>...}`  | Initializes `myArray` with n user-specified *type*
| ` <primitive_type> [10] myArray = {0}`                | Initializes `myArray` with 10 zeros
| ` <primitive_type> [10] myArray = {*}`                | Initializes `myArray` with 10 random *primitive_type* 

n-dimensional arrays are distinct from arrays of arrays for memory mapping/access.

### Structs
Analogs to C-structs that defines a physically grouped list of variables to be placed under one name in a block of memory.
Items in structs can be accessed by index or by their names.

| Non-Primitive  | Description 
---              |---
| `struct(<type1> name1, <type2> name2 ....)` | A grouped list of variables contiguous in memory, can  be referenced and accessed by one name

| Struct Declarations  | Description 
---              |---
| `name struct(<type1> name1, <type2> name2 ....) myStruct` | `name` keyword declares a new struct object called `myStruct` consisting of *type1* with name *type2* *...*   
| `myStruct object = {name1 = <type1>,name2 = <type2>...}`  | Initializes `object` as type `myStruct` with specified types
                             | 
### Functions
#### Regular Functions
Functions are declared using the `def` keyword, and must specify their arguments and return type, and a terminating `:`. The scope of a function is defined by whitespace - that is, all statements that are part of the function cannot be aligned with the function declaration, but must be "indented", or prefaced by at least one whitespace character.  
All function arguments that are primitive types are passed by value, meaning all arguments are copied to the function. A argument that has been passed into a function may have it's value changed within a function, but will maintain it's original value outside the function.
All function argumenst that are non-primitive types are passed by reference, meaning changes to the argument will change the argument's value outside of the function.

Function declaration:
`<return type> def <name>(<type1> arg1, <type2> arg2...):`

#### Higher Order Functions
VLC contains built-in higher order functions that are optimized to be performed on the GPU.
These words are reserved keywords and may not be used by the user to define any variable, constant, or function.  

| Higher Order Function  | Description 
---              |---
| `map(<func>, <array1>, <array2>...)`    |  Function that takes as input a function `func` with X open paremeters, and X arrays, performs `func` on the X arrays and returns one resulting array. `map` also accepts 2-Dimensional arrays.
| `reduce(<func>, <array>)` |  Function that takes as input a function `func` with two open paremeters and an array of types `array`, performs pairwise reduction on every pair in `array`, and returns final reduced result. `reduce` also accepts 2-Dimensional arrays.


Functions `func` passed to `map` and `reduce` 
    1) Must have the corresponding number of arguments specified by map (X) and reduce (two)
    2) Must have arguments that are the same type as the `array` passed into map and reduce. In the case of map, the order of the argument types to `func` should be match the type of each array


##### Map
    int def triple(int x):
        return x * 3
    int [4] numbers = {0,1,2,3}
    int [4] triple_numbers = map(triple, numbers) 
    print triple_numbers //0 3 6 9

##### Reduce
    int def sum(int x, int y):
        return x + y
    int [4] numbers = {0,1,2,3}
    int sum = reduce(sum,numbers)
    print sum // 6

### Casting
#### Primitive Types
`int`,`long`, `float`,and `double` can be cast to eachother. When casting from lower-bit type to a higher-bit type, for example from a `float` to a `double`, there is no loss of precision. 
Furthermore, casting a higher-bit-type to a lower-bit-type with a value that fits into the lower-bit-type will also generate no loss of precision.
For `double` to `float` conversions, 
Casting from a floating-point-type to an integer type drops the fractional part of the floating point type.
#### Non-primitive Types
VLC is a strongly typed language, and does not allow casting between non-primitive types.
# Syntax
## Control Flow
VLC uses standard `if` `else` `elif` control statements. 
VLC also accepts `while` loops.

VLC `for` loops come in two flavors. 
## Scope
Scoping in VLC is static, and follows the conventions of block-level scoping. 
Variables defined at the top level of a program are available in the global scope of the program.
# Expressions
## Arithmetic and Logical Operators
| Operator  | Description 
| +,-,/,*,% | Vector parallel addition, subtraction, division, multiplication and modulo, Arithmetic operations on primitives.
| .         | Dot Product, only accepts 1-Dimensional Arrays
| **        | Matrix Multiplication, only accepts 2-Dimensional Arrays
| `log(myArray, n)`,`log(myArray, n, floor)` | Takes log of all elemnents in `myArray`
| `myArray^n` | Raises every element in`myArray` to the nth power
| &&, ||, !=, !, ==, XOR | Standard logic operators
| <<,>> |Bit-shift operators

# External Declarations
## `main` function and Code Execution
VLC code execution begins at a predefined `main` function in the file.
If such a function cannot be found, code execution begins at global statements.
## `import` Statements

The `import` keyword allows VLC to import code from other VLC files.
When importing other VLC files, `main` functions are ignored in the imported files.

For example, if we have file *a.vlc* that imports *b.vlc* , any `main` function in *b.vlc* will be ignored. 



