
# Aback
Aback is a language that uses Polish notation. It is named after the [abacus](https://en.wikipedia.org/wiki/Abacus) and because it is the opposite of [Forth](https://en.wikipedia.org/wiki/Forth_%28programming_language%29), a *stack based* programming language that uses *reverse Polish notation*. In reality it's not really the opposite, the syntax is actually very similar.
With the `|>` operator, we can achieve the most human-readable form, a combination of *Polish* and *reverse Polish* notation.

# Getting started
## Installation
- Clone this repository<br>
`$ git clone git@github.com:jkenda/aback.git`
- Build the project<br>
`$ dune build`
- Install the program<br>
`$ dune install`

## Syntax highlighting
Syntax highlighting for Aback is only available for Vim.
Get the file at [`syntax/aback.vim`](syntax/aback.vim).

## Run your first example
The file `examples/hello.ab` contains the following code:

```
puts "Hello, world!\n"
```

Run it with `aback int examples/hello.ab`. This compiles the code to an intermediate representation and interprets it, outputting the result.

## Run another example
The next example outputs a sequence of *Fibonacci numbers*.

```
include "core/stack.ab"
macro LIMIT is 100 end

(output a Fibonacci sequence up to LIMIT)
1 0 while < over LIMIT do
    take a b in
        puti a |>
        + a b a
    end
    putc ' '
end drop drop |>
puts "\n"
```

If you run it with `aback int examples/fib.ab`, you will get the following output:

	1 1 2 3 5 8 13 21 34 55 89

Check out other examples in the `examples/`directory.

# Running the executable
The `aback` executable has three modes: `int` `check` and `print`. In the future, there will also be a `com` mode, which will compile the program to a *native executable binary*.

## `int`
As we've already seen, the `int` mode compiles the executable to an *intermediate representation* and interprets it.
Example:

	$ aback int examples/fib.ab
	1 1 2 3 5 8 13 21 34 55 89

## `check`
Aback always type checks the program before running it. With the `check` subcommand, we can *skip the running step*. Just type `aback check examples/fib.ab`. The program will output `OK.`

If we remove one `drop` word from the program, the type checker will *detect an error* and output

	$ aback check examples/fib.ab
	'examples/fib.ab':8:15:
	        int  left on the stack at the end of program

If we *forget to push and additional `a`* inside the while loop, the output will be

	$ aback check examples/fib.ab
	'examples/fib.ab':11:1:
	        cannot shrink stack inside loop

 As you can see, despite being a rudimentary stack based language, Aback is *type safe*.

## `print`
The print command outputs the intermediate representation.
Example:

	$ aback print examples/hello.ab
	program:
	0| (PUSH (Int 14))
	1| (PUSH (Ptr 0))
	2| PUTS
	
	strings: |Hello, world!\n\000|
	storage size: 0
	
	$ aback print examples/fib.ab
	program:
	 0| (PUSH (Int 0))
	 1| (PUSH (Int 1))
	 2| (PUSH (Int 100))
	 3| (PEEK (0, 0))
	 4| (PEEK (1, 1))
	 5| (PUT 1)
	 6| LT
	 7| (DO 19)
	 8| (TAKE 0)
	 9| (TAKE 1)
	10| (PUT 0)
	11| PUTI
	12| (PUT 0)
	13| (PUT 1)
	14| (PUT 0)
	15| ADD
	16| (PUSH (Char ' '))
	17| PUTC
	18| (END_WHILE 2)
	19| (TAKE 0)
	20| (TAKE 0)
	21| (PUSH (Int 1))
	22| (PUSH (Ptr 0))
	23| PUTS

	strings: |\n\000|
	storage size: 2

# The language
In Aback, the syntax is made up from *words* that are separated by white space. The only exceptions are *strings* which start and end with with `"` and can contain any sequence of characters and *comments* which start with `(` and end with `)`.

## Example
To explain the language, let's go through the `fib.ab` example line by line.

```
include "core/stack.ab"
```
`include` is a word that opens the file on the path following it and simply *inserts the contents* in its place.
```
macro LIMIT is 100 end
```
The `macro` word takes whatever is in between the words `is` and `end` and inserts it where it encounters the word `LIMIT`. In this case, we are defining a constant.
```
(output a Fibonacci sequence up to LIMIT)
```
is a comment. Everything between the brackets is ignored.
```
1 0 while < over LIMIT do
```
The words `1` and `0` push the numbers 0 and 1 onto the stack.
`< over LIMIT` is the condition of the while loop. First, `LIMIT` is pushed onto the stack, then the `over` word pushes the element under the top of the stack to the top and the `<` word compares them.
```
    take a b in
```
takes two elements from the top of the stack - in this case 0 and 1 - and makes them available as variables.
```
        puti a |>
```
pushes the value of the variable `a` onto the stack and outputs it.
```
        + a b a
```
`a` is pushed onto the stack, then the sum of `a` and `b`.
```
    end
```
ends the scope of `take`.
```
    putc ' '
```
puts a space onto the stack and outputs it.
```
end 2drop |>
```
ends the while loop and drops 2 elements from the stack.
```
puts "\n"
```
pushes the string onto the stack and outputs it.
To be exact, the string is *stored elsewhere*. What is actually pushed to the stack are a pointer to the string and its length.

## Order of evaluation
Words are evaluated from *right to left* and from *bottom to top*. To output the sum of numbers 1 and 2, we would write
`puti + 1 2` which expands to
```
(PUSH (Int 2))
(PUSH (Int 1))
ADD
PUTI
```
Keep this in mind when manipulating the stack with such operators as `dup` and `over`. It is also useful to think of the *leftmost* element as the *top of the stack*.

## The `|>` operator
Polish notation makes sense *inside* individual statements or when pushing multiple elements to the stack, but not so much *between* statements.
The `|>` operator breaks blocks of Polish notation into reverse Polish notation.

`1 2 3` expands to
```
(PUSH (Int 3))
(PUSH (Int 2))
(PUSH (Int 1))
```
while `1 |> 2 3` expands to
```
(PUSH (Int 1))
(PUSH (Int 3))
(PUSH (Int 2))
```
The operator can be thought of as a *delimiter between statements*. For example
```
$ cat examples/hello.ab
puts "Hello," |>
puts " world!\n"

$ aback int examples/hello.ab
Hello world!
```

## `take` and `peek`
These statements do *exactly what their names suggest*. `take` takes *n* elements from the top of the stack and stores them into variables while `peek` peeks into the stack and stores *n* top elements into variables. The variables can be used inside the statements as many times as you like.

Variables can be defined using these statements like this:
```
3.14 take pi in
	putf pi |>
	putf * pi pi
end
```

## Functions
There are two types of functions - *macros* and *procedures*. Procedures are called while macros are expanded in place. This means macros can't be recursive but procedures can.

### Macros
Macros are named sequences of words that are expanded wherever their name is encountered. You may remember a macro called `LIMIT` that defined a constant but what you don't know is that we've encountered two more - `over` and `2drop`. Let's see their definitions:
```
macro over a' b' -> b' a' b' is
    peek _ b in
        b
    end
end

macro drop 1 -> 0 is
    take _ in
    end
end

macro 2drop 2 -> 0 is
    drop drop
end
```
Here, you can see two types of macros - *generically typed macros* and *numbered macros*. There are also *untyped macros* - `LIMIT` is one of them.

*Typed macros* are type checked so that their signature matches the types of the elements on the top of the stack when they are entered and exited.

*Numbered macros* only need to conform to the number of input and output elements.

Of course, there are also typed macros with concrete types. Here is one:
```
macro add
    int int
    -> int
is
    +
end
```
We've just defined a word `add` that expands to `+`: 
`add 1 2` &rarr; `+ 1 2`

Types aren't really needed for such a simple function so we could replace it with
```
macro add is + end
```

### Procedures
Procedures aren't implemented yet.
```Ocaml
raise @@ Not_implemented (loc, "Procs aren't implemented yet!")
```
