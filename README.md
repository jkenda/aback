# Aback
Aback is a language that uses Polish notation. It is named after the [abacus](https://en.wikipedia.org/wiki/Abacus) and because it is the opposite of [Forth](https://en.wikipedia.org/wiki/Forth_%28programming_language%29), a stack based programming language that uses reverse Polish notation. It is not really the opposite, the syntax is actually very similar.
With the `|>` operator, we can achieve the most human-readable form, a combination of Polish and reverse Polish notation.

# Getting started
## Installation
- Clone this repository
`$ git clone git@github.com:jkenda/aback.git`
- Build the project
`$ dune build`
- Install the program
`$ dune install`

## Syntax highlighting
Syntax highlighting for Aback is only available for Vim.
The file is available at [`syntax/aback.vim`](syntax/aback.vim).

## Run your first example
The file `examples/hello.ab` contains the following code:

```Forth
puts "Hello, world!\n"
```

Run it with `aback int examples/hello.ab`. This compiles the code to an intermediate representation and interprets it, outputting the result.

## Run another example
The next example outputs a sequence of Fibonacci numbers.

```Forth
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
The `aback` executable has three modes: `int` `check` and `print`. In the future, there will also be a `com` mode, which will compile the program to a native binary executable.

## `int`
As we've already seen, the `int` mode compiles the executable to an intermediate representation and interprets it.
Example:

	$ aback int examples/fib.ab
	1 1 2 3 5 8 13 21 34 55 89

## `check`
Aback always type checks the program before running it it. With the `check` subcommand, we can skip the running step. Just type `aback check examples/fib.ab`. The program will output `OK.`

If we remove one `drop` word from the program, the type checker will detect an error and output

	$ aback check examples/fib.ab
	'examples/fib.ab':8:15:
	        int  left on the stack at the end of program

If we forget to push and additional `a` inside the while loop, the output will be

	$ aback check examples/fib.ab
	'examples/fib.ab':11:1:
	        cannot shrink stack inside loop

 As you can see, despite being a rudimentary stack based language, Aback is type safe.

## `print`
The print command outputs the immediate representation.
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
