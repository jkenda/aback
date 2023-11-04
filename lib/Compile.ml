open Format
open Lexer
open Program

let print_data = function
    | Int n -> string_of_int n
    | Bool true -> "1"
    | Bool false -> "0"
    | Char c -> string_of_int (int_of_char c)
    (* TODO: print float in integer representation *)
    | Float f -> Int64.to_string @@ Obj.magic @@ Obj.repr f
    | Str_ptr n -> sprintf "[strings + %d]" n

let header = "
SYS_read  equ 0
SYS_write equ 1

STDIN  equ 0
STDOUT equ 1
STDERR equ 2

; write(fd, buf, count)
macro write fd, buf, count
{
	; syscall(SYS_write, fd, buf, count)
	mov rax, SYS_write
	mov rdi, fd
	mov rsi, buf
	mov rdx, count
	syscall
	cmp rax, 0
	jl _fatal_error
}

; write(fd, buf, count)
macro read fd, buf, count
{
	; syscall(SYS_read, fd, buf, count)
	mov rax, SYS_read
	mov rdi, fd
	mov rsi, buf
	mov rdx, count
	syscall
	cmp rax, 0
	jl _fatal_error
}

macro exit code
{
	mov rax, 60
	mov rdi, code
	syscall
}

format ELF64 executable
use64

segment readable executable
entry $
	; addroff = SP
	lea r8, [rsp - 8]
"

let footer = "
	exit 0

_fatal_error:
	exit rax

segment readable writable
"

let add_strings buffer strings =
    Buffer.add_string buffer "strings db \"";
    String.iter (function
        | '\000' | '\n' | '\r' | '\t' | '"' as c ->
                Buffer.add_string buffer @@ sprintf "\", %d, \"" @@ Char.code c
        | c -> Buffer.add_char buffer c) strings;
    Buffer.add_char buffer '"'

let to_fasm_x64_linux program =
    let buffer =
        Buffer.of_seq
        @@ String.to_seq header
    in

    let compile' (loc, instr) =
        let cmp op =
            "\tpop rax\n" ^
            "\tpop rbx\n" ^
            "\tcmp rax, rbx\n" ^
            "\txor rax, rax\n" ^
            "\tset" ^ op ^ ", al\n" ^
            "\tpush rax\n"
        and int_op op =
            raise @@ Not_implemented (loc, show_ir op)
        and float_op op =
            raise @@ Not_implemented (loc, show_ir op)
        and bool_op op =
            raise @@ Not_implemented (loc, show_ir op)
        and put op =
            "\tpop rsi\n" ^
            match op with
            | PUTS ->
                    "\tpop rdx\n" ^
                    "\twrite STDOUT, rsi, rdx\n"
            | PUTC ->
                    "\twrite STDOUT, rsi, 1\n"
            | PUTI -> raise @@ Not_implemented (loc, "PUTI")
            | PUTB -> raise @@ Not_implemented (loc, "PUTB")
            | PUTF -> raise @@ Not_implemented (loc, "PUTF")
            | _ -> raise @@ Unreachable (show_ir op)
        and cond_jmp id =
            "\tpop rax\n" ^
            "\tcmp rax, 0\n" ^
            "\tje " ^ string_of_int id ^ "\n"
        in

        Buffer.add_string buffer @@
        match instr with
        | (FN _ | FN_END) -> ""
        | IF id -> sprintf "if_%d:\n" id
        | THEN id -> cond_jmp id
        | ELSE id -> sprintf "\tjmp if_%d\n" id
        | END_IF _ -> ""

        | WHILE id -> sprintf "while_%d:\n" id
        | DO id -> cond_jmp id 
        | END_WHILE id -> sprintf "\tjmp while_%d\n" id

        | PEEK (depth, addr) -> sprintf "\tmov  [vars + 8 * %d], [rsp + 8 * %d]\n" addr depth
        | TAKE addr          -> sprintf "\tpop  [vars + 8 * %d]\n" addr
        | PUT  addr          -> sprintf "\tpush [vars + 8 * %d]\n" addr

        | PUSH d ->
                (match d with
                | Int n -> sprintf "\tpush %d\n" n
                | Bool true -> "\tpush 1\n"
                | Bool false -> "\tpush 0\n"
                | Char c -> sprintf "\tpush %c\n" c
                | Float f -> sprintf "\tpush %d\n" @@ Obj.magic @@ Obj.repr f
                | Str_ptr n -> sprintf "\tlea rax, [strings + %d]\n\tpush rax\n" n)

        | EQ -> cmp "e"
        | NE -> cmp "ne"
        | LT -> cmp "l"
        | LE -> cmp "le"
        | GT -> cmp "g"
        | GE -> cmp "ge"

        | ADD | SUB | MUL | DIV | MOD
        | BAND | BOR | BXOR | LSL | LSR as op -> int_op op
        | FADD | FSUB | FMUL | FDIV as op -> float_op op
        | AND | OR as op -> bool_op op
        | (PUTC | PUTS | PUTI | PUTF | PUTB) as op -> put op
    in
    Array.iter compile' @@ Array.combine program.loc program.ir;
    Buffer.add_string buffer footer;
    add_strings buffer program.strings;

    Buffer.to_bytes buffer
