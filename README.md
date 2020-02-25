# El

El is an experimental programming language for semantics prototyping, as well as a reference implementation. Its goal is to provide a minimalistic syntax in which a programmer can write their own functions to mimick desired semantics.

## Getting started

Use [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) to build and run El.

```bash
stack build
stack run
```

You will enter the REPL mode and see the prompt.

## Programming in El

El computation model is fairly simple. The code is a chain of function composition. The last function in a line is evaluated first, its result is passed to the next function and so on.

### Arithmetic functions

| Function | Type | Arguments | Description |
| - | - | - | - |
| `___ADD___` | `___BINOP___` | `num num` | addition |
| `___SUB___` | `___BINOP___` | `num num` | subtraction |
| `___MUL___` | `___BINOP___` | `num num` | multiplication |
| `___DIV___` | `___BINOP___` | `num num` | floatint-point division |
| `___IDIV___` | `___BINOP___` | `int int` | integer division |
| `___MOD___` | `___BINOP___` | `int int` | modulo |

#### Examples
```
>___ADD___ 2 2
[("4","int")
>___MUL___ 1234 5678
[("7006652","int")]
```

### Type and function definition

| Function | Type | Arguments | Description |
| - | - | - | - |
| `___TYPE___` | `___BINOP___` | `any any` | type definition |
| `.*` | `any` | `=` `any` | variable assignment |

Variable assignment defines a function that takes no arguments and returns the value that was given to it at the assignment.

#### Examples
```>foo
[("foo","nil")]
>___TYPE___ [a-zA-Z]+ word
[("[a-zA-Z]+","word")]
>foo
[("foo","word")]
>foo = hello
[("foo","word")]
>foo
[("hello","word")]
```

### On the `nil` type and passing arguments to functions

Functions that are not currently defined in the environment are of type `nil` (not initialized). They take no arguments and their body is empty, meaning that there is nothing to evaluate. Whatever such a function receives in the input, it returns it together with the function itself.

It is possible to define your own empty functions, that act like `nil` but have a custom type.

This behaviour of passing the input further is generalized in two ways: if a function receives too few arguments on the input, or if the arguments are not of the expected type, it is considered an empty function; if a function receives too many arguments, it evaluates normally and the remaining arguments are appended to its result.

#### Examples
```>___ADD___
[("___ADD___","___BINOP___")]
>___SUB___ 1
[("___SUB___","___BINOP___"),("1","int")]
>___MUL___ 5 4 3 2 1
[("20","int"),("3","int"),("2","int"),("1","int")]
>hello, world!
[("hello,","nil"),("world!","nil")]
```
