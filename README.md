# Rum Compiler [![Travis Status](https://travis-ci.org/vrom911/Compiler.svg?branch=master)](https://travis-ci.org/vrom911/Compiler)

> ***Yo, Ho, Ho, and the Compilation of Rum!***

ITMO compilers course project: Implementation of Compiler for <i>Rum</i> language.

- [Compiler](#rum-compiler)
    - [Getting Started](#getting-started)
        - [Build](#build)
        - [Run](#run)
        - [Build Dependencies](#build-dependencies)
    - [Rum Language](#rum-language)
        - [Syntax](#syntax)
    - [Features](#features)
        - [Types](#types)
        - [Variables](#variables)
        - [Operators](#operators)
    - [Expressions](#expressions)
        - [Operator Precedence Table](#operator-precedence-table)
    - [Statements](#statements)
    - [Standard library](#standard-library)
    - [Parser](#parser)
    - [AST](#ast)
    - [Environment](#environment)
    - [Interpreter](#interpreter)
    - [Stack Machine](#stack-machine)
        - [Translation](#translation)
        - [Interpretation](#interpretation)
    - [Compiler](#compiler)
    - [Testing](#testing)
        - [Running the tests](#running-the-tests)
    - [License](#license)
    - [Acknowledgments](#acknowledgments)


## Getting Started

To run on your local machine follow the next steps:
### Build
To build the Compiler using [stack][stack] try next command:
```sh
stack build
```
### Run
Then run the `.rum` file with the option you need

```sh
stack exec Compiler -- -flag file_name.rum
```
where next flags are available
* `-i` -- to interpret the program from `file_name.rum`
* `-s` -- to compile the program from `file_name.rum` into the presentation of the stack machine with the further interpretation of the stack machine
* `-c` -- to compile the program from `file_name.rum` into executable `file_name`


### Build Dependencies
Consider next requirements
* [stack][stack]
* [LLVM 4.0][llvm]

## Rum Language

**Rum** is an artificial dynamically typed programming language, which was developed in learning purposes.


### Syntax

Simple program example
```c
fun fact(n : int) : int
begin
  if n < 2 
     then return 1
     else return n * fact(n-1)
  fi
end

n := read();
write(fact(n))
```
For the further examples let's assume
```c 
S := "Yo-ho-ho, and the Bottle of Rum!"
```
## Features
Next features were implemented for the *Rum* language.
### Types

* `int` -- integer
* `char` -- symbol
* `str` -- string (reference type)
* `arr` -- array (reference type)
* `void` -- void type
* `bool` -- boolean (`false` and `true` are represented as `0` and `1` respectively, the integer not equal to `0` is considered as `true`)

### Variables
All variable names must begin with a letter of the alphabet or an underscore `_` or dollar sign `$`. After the first initial letter, variable names can also contain letters, numbers and one of these symbols: `_`, `$`.
Keywords can not be used as variable names. In *Rum* language these words are reserved: `skip`, `if`, `then`, `else`, `fi`, `repeat`, `until`, `do`, `od`, `while`, `for`, `fun`, `begin`, `end`, `return`, `true`, `false`.

*Note:* All references are considered to start with capital letter when all other variables from with lowercase.

### Operators
- *Arithmetic* : `+`, `-`, `*`, `/`, `%`, `^`
- *Logical* : `==`, `!=`, `<=`, `<`, `>=`, `>`
- *Comparison* : `&&`, `||`
- *Assignment* : `:=`
- *Unary* : `-`
- *Void* : `skip`

## Expressions
*Rum* language supports different kind of expressions with consideration of precedence and associativity  
- [**Variables**](#variables)  -- local and global variables support.
- **Operations**
    - *Binary* -- operations of any nesting level using arithmetic [operators](#operators)
    - *Logical operations* -- using logical [operators](#operators)
    - *Comparison operations* -- using comparison [operators](#operators)
    - *Unary operations* -- using unary [operators](#operators)
- **Functions call** 
    can be expression and require to evaluate the value to use it in statements
    ```c
    S := strmake(5, 'm')
    ```

### Operator Precedence Table


| Precedence | Operators         | Operator Name | Associativity |
| :--------: | ----------------- | ------------- | :-----------: |
| 0          | `()`, `[]`        | Parentheses   | left          |
| 1          | `-`               | Unary minus   | prefix        |
| 2          | `^`               | Power         | right         |
| 3          | `*`, `\`, `%`     | Arithmetic    | left          |
| 4          | `+`, `-`          | Plus, minus   | left          |
| 5          | `==`, `!=`,  `<=`, `<`, `>=`, `>` | Logical |none |
| 6          | `&&`              | And           | left          |
| 7          | `!!`              | Or            | left          |
| 8          | `:=`              | Assignment    | right         |

## Statements
Each statement is separated by `;` except the last one in the program, where the program is finished list of statements.
*Rum* implements next types of statements:
- **Assignment**
    ```c
    x := 911;
    y := read();
    z := x && (y * 2 - 5 ^ 1);
    ar := {1, 2, 3}
    ```
- **Function call**
    -- can be separated statement also, e.g.
     ```c
     write(42)
     ```
- **Void statement** `skip`
- **Conditional statement** 
    -- several kinds of `if` statements are supported
    ```c
    if strcmp(Bottle, "Rum") == 0 
        then yell("Yo, Ho, Ho!")
    fi
    ```
    also alternative ending supported
    ```c
    if strcmp(Bottle, "Rum") == 0 
        then yell("Yo, Ho, Ho!")
        else Mood := "sad" 
    fi
    ```
    and unlimited nesting level of alternative with `elif`
    ```c
    if strcmp(Bottle, "Rum") == 0 
        then yell("Yo-ho-ho!")
        elif strcmp(Language, "Rum") == 0 
            then 
                yell("Yo, Ho, wait..");
                Mood := "exited"
            else Heart := "broken"
    fi
    ```
- **Loops**
    - *Repeat Until*
    ```c
    repeat
        yell("Yo-ho-ho!");
        drink();
        rumQuantity := rumQuantity - tumbler
    until rumQuantity == 0;
    ```
    - *While*
    ```c
    while (n != 0)
    do
        sing(strcat(n, " men on the dead man's chest"));
        n := n - 1;
        sing("Yo-ho-ho, and the Bottle of Rum")
    od
    ```
    - *For*
    ```c
    sum := 0;
    for i := 1, i <= 100, i := i+1
    do
        sum := sum + 1
    od
    ```
    - *Function*
        -- all function declarations should be done in the beginning of the file. Important not to separate functions with any special symbols.
    ```c
    fun lang(l : str) : str
    begin
        if (strcmp(l, "Rum") == 0)
        then return "☠️"
        else return "😞"
    end
    ```
    - *Return*
    
    
    
 
## Standard library
`    Rumlude` is the library of standard functions of the language. `RumludeFuns` is the special data type for representing them during parsing to *AST*.

In the `Compiler.Rum.Internal.Rumlude` module there is a map from string to `RumludeFuns` type, so it can be used at parsing stage. The main function of the module is `runRumlude` which takes as arguments `RumludeFuns` representative and the list of the parameters, that are going to be arguments of the given function, and returns the result of the work of the standard function.

Here is the list of functions that are based in  module with usage examples:

- `read()`

    --  reads integer from standard input

- `write(n : int)`

  -- prints integer `n`  into standard output.

- `strlen(S : str) : int`  

    -- returns length of the given string `s`
    ```c
    write(strlen(S))
    > 32
    ```
- `strget(S : str, i : int) : int` 
    -- returns the symbol of the string on the given position `i` converted into `int`
    ```c
    write(strget(S, 1))
    > 111  -- letter 'o'
    ```

- `strsub(S : str, start : int,  len: int) : str`
    -- returns string, which is build by taking `len` symbols starting from `start` position
    ```
    D := strsub(S, 15, 18)

    > the Bottle of Rum!
    ```
- `strdup(S : str) : str`
    -- returns the duplicate of the string
    ```c
    D := strdup(S)
    D
    > Yo-ho-ho, and the Bottle of Rum!
    ```
- `strset(S: str, i : int, ch : char): str`
    -- replaces `i`-th symbol in the string `S` with symbol `ch`
    ```c
    strset(S, 1, 'u')
    > Yu-ho-ho, and the Bottle of Rum!
    ```
- `strcat(S1 : str, S2 : str): str`
    -- returns the result of concatenation of two strings
    ```
    strcat("Yo-ho-ho, ", "and the Bottle of Rum!")
    > Yo-ho-ho, and the Bottle of Rum!
    ```
- `strcmp(S : str, D : str) : int`
    -- returns the result of lexical comparison of two strings. The result is equals to `0` if strings `S` and `D` are equal, `-1` if `S` is less than `D` and `1` in other case
    ```c
    write(strcmp(S, D));
    write(strcmp(S, "Yo-ho-ho"))
    write(strcmp(S, "Yu-ho-ho"))
    write(strcmp(S, "Ya-ho-ho"))
    > 0
    > -1
    > -1
    > 1
    ```
- `strmake(n : int, ch : char) : str`
    -- builds the string of length `n` fulfilled with `ch` symbol
    ```c
    strmake(3, 'm')
    > mmm
    ```
- `arrlen(: arr) : int`
    -- returns length of the given array
- `arrmake(n : int, e : int) : arr`
    -- builds unboxed array of the length `n` fulfilled with the integer `e`
- `Arrmake(n : int, e : int) : arr`
    -- builds boxed array of the length `n` fulfilled with the integer `e`


## Parser
For parsing part of the project monadic parser combinator library [MegaParsec](https://github.com/mrkkrp/megaparsec#readme).
Source files are in the `Compiler.Rum.Internal.Parser` module of the project.
The input is the file and the result of the work of the parser is **AST**.
In case of parsing error the message with additional information (error row and column coordinates) will be shown. 
(TODO: make beautiful output of errors)
## AST
This module gives an idea about the model of the language. Data types and structure of the program can be found in `Compiler.Rum.Internal.AST` module. Almost all of the types were described before, so here will be the brief enumeration
- [Expression](#expressions)
- [Statement](#statements)
- Program -- the list of statements

## Environment
Different types of environment are used in interpreter and stack machine.

  `Interpret a` newtype was described in `Compiler.Rum.Internal.AST`which is monad transformer `StateT`. 
```haskell
newtype Interpret a = 
    Interpret { runInterpret :: StateT Environment (MaybeT IO) a}
    
data Environment = 
    Env { varEnv    :: VarEnv    -- to hold vars
        , refVarEnv :: RefVarEnv -- to hold reference vars
        , funEnv    :: FunEnv    -- to hold custom functions
        , isReturn  :: Bool      -- necessary for funs declarations
        }
```
For Stack Machine's Environment there is special type in `Compiler.Rum.StackMachine.Structure` 

```haskell
type InterpretStack = 
    ReaderT ([Instruction], Labels) (StateT StackEnvironment IO) ()
    
    data StackEnvironment = 
        SEnv { vars  :: RefSVarEnv -- store by reference
             , stack :: Stack
             , pos   :: Int        -- current position at Stack
             }
```
## Interpreter
Interpreter receives `Program` data type and returns evaluated result.
All source files are in the `Compiler.Rum.Interpreter` directory. In `Rummer` module all evaluation stuff for both expressions and statements take place. `interpret` function works under     `Interpret` environment and this is main working function for interpreter.

## Stack Machine
Work of this part can be separated into two steps.
### Translation
Translation of `Program` into readable by Stack Machine list of `Instruction`.

Here are permitted instructions
- `Nop`       -- No operation
- `SBinOp` -- Binary operation sign
- `SLogicOp` -- Binary operation sign
- `SCompOp` -- Binary operation sign
- `Push` -- Push a value onto the stack
- `Pop` -- Pop the most recent value from the stack
- `Jump`  -- Jump unconditionally to a location
- `JumpIfTrue` -- Jump if conditions are met
- `JumpIfFalse` Jump if conditions are not met
- `Load` -- Load ordinary variable
- `LoadRef` -- Load reference variable
- `Store`  -- Put Variable
- `Label`  -- Location coordinates
- `SFunCall`   -- Call a function
- `SRumludeCall`  -- Call function from standard
- `SReturn`       -- Return from a function
- `PushNArr`  -- Put array on stack
- `LoadArr`  -- Load array
- `StoreArr` -- Store array

### Interpretation
`Compiler.Rum.StackMachine.Stacker` module receives set of `Instruction` from work of the `Translator` and transforms data into result.
Main function of the module is `stacker`. It finds the label of starting position in the instructions and start execute them one by one.

## Compiler
And finally compilation. Implementation of the *Rum* language was made with [LLVM][llvm]. Special thanks for Stephen Diehl's [work][article] which explains the basics of how to build a compiler in Haskell.
For the implementation these two packages are required: 
* `llvm-hs-pure` -- pure Haskell representation of the LLVM Intermediate Representation (IR).
* `llvm-hs` -- FFI bindings to LLVM required for constructing the C representation of the LLVM IR and performing optimization and compilation.

Few consistent steps should be done, and the first one is code generation of LLVM IR from the *AST*. All necessary functions for code generation are located in `Compiler.Rum.Compiler.CodeGen`module and the transformation process is realized in the `Compiler.Rum.Compiler.Emmiter` module of the project. `Compiler.Rum.Compiler.JIT` module contains of optimizations for the language.




## Testing
There is a bunch of different kind of tests in `compiler-tests` [submodule][tests]. Each folder consist of different type of tests:

- `core` - number of tests to cover all  basic features of the language
- `expressions` & `deep-expressions` - huge amount of test basically for arithmetic expressions and IO
- `performance` - tests to compare Compiler efficiency with `gcc`.

### Running the tests
`cd` to the directory of the test which have to be run and use the next command:
```sh
make
```
This will run tests in the `-i`, `-s` and `-c` mode.

For example, 
```sh
cd compiler-tests/core && make
```


## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments
Stephen Diehl article ["Implementing a JIT Compiled Language with Haskell and LLVM"][article]

[llvm]: http://llvm.org
[stack]: http://docs.haskellstack.org/en/stable/README/
[article]: http://www.stephendiehl.com/llvm
[tests]: https://github.com/anlun/compiler-tests/tree/a781309237ab9aba872bfac80930fae04111c01a
