open Format
open Lexer
open Program

let header = "
SYS_read  equ 0
SYS_write equ 1

STDIN  equ 0
STDOUT equ 1
STDERR equ 2

; write(fd, buf, count)
; %rsi = buf
; %rdx = count
macro write fd
{
	; syscall(SYS_write, fd, buf, count)
	mov rax, SYS_write
	mov rdi, fd
	syscall
	cmp rax, 0
	jl _fatal_error
}

; read(fd, buf, count)
macro read fd
{
	; syscall(SYS_read, fd, buf, count)
	mov rax, SYS_read
	mov rdi, fd
	syscall
	cmp rax, 0
	jl _fatal_error
}

macro exit code
{
	mov rdi, code
	mov rax, 60
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
	lea rax, [rsp - 8]
	cmp rax, r8
	jne _fatal_error
	exit 0

; puts(rsi, rdx)
puts:
    write STDOUT
    ret

; putc(rdi)
putc:
    mov [rsp - 8], rdi
    lea rsi, [rsp - 8]
    mov rdx, 1
    write STDOUT
    ret

; puti(rdi)
puti:
	push    rbx
	mov     rbx, rdi
.L3:
	test    rbx, rbx
	jns     .L2
	mov     edi, 45
	neg     rbx
	call    putc
	jmp     .L3
.L2:
	cmp     rbx, 9
	jle     .L4
	mov     rax, rbx
	mov     ecx, 10
	cqo
	idiv    rcx
	mov     rdi, rax
	call    puti
.L4:
	mov     rax, rbx
	mov     ecx, 10
	pop     rbx
	cqo
	idiv    rcx
	lea     edi, [rdx+48]
	jmp     putc

_fatal_error:
	exit rax


segment readable writable
"

let add_strings buffer strings =
    Buffer.add_string buffer "strs db \"";
    String.iter (function
        | '\000' | '\n' | '\r' | '\t' | '"' as c ->
                Buffer.add_string buffer @@ sprintf "\", %d, \"" @@ Char.code c
        | c -> Buffer.add_char buffer c) strings;
    Buffer.add_string buffer "\"\n"

let to_fasm_x64_linux program =
    let buffer =
        Buffer.of_seq
        @@ String.to_seq header
    in

    let has_else = Hashtbl.create 10 in
    let collect' = function
        | ELSE id -> Hashtbl.replace has_else id ()
        | _ -> ()
    in
    Array.iter collect' program.ir;

    let compile' (loc, instr) =
        let cmp op =
            "\t; " ^ show_ir op ^ "\n" ^
            let op =
                match op with
                | EQ -> "e"
                | NE -> "ne"
                | LT -> "l"
                | LE -> "le"
                | GT -> "g"
                | GE -> "ge"
                | _ -> raise @@ Unreachable (show_ir op)
            in
            "\tpop  rax\n" ^
            "\tpop  rbx\n" ^
            "\txor  rcx, rcx\n" ^
            "\tcmp  rax, rbx\n" ^
            "\tset" ^ op ^ " cl\n" ^
            "\tpush rcx\n"
        and int_op op =
            "\t; " ^ show_ir op ^ "\n" ^
            "\tpop  rax\n" ^
            "\tpop  rbx\n" ^
            (match op with
            | ADD -> "\tadd  rax, rbx\n"
            | SUB -> "\tsub  rax, rbx\n"
            | MUL -> "\timul  rax, rbx\n"
            | DIV | MOD ->
                    "\tcdq\n" ^
                    "\tidiv  rbx\n"
            | _ -> raise @@ Unreachable (show_ir op)) ^
            match op with
            | MOD -> "\tpush  rdx\n"
            | _ -> "\tpush rax\n"
        and float_op op =
            raise @@ Not_implemented (loc, show_ir op)
        and bool_op op =
            raise @@ Not_implemented (loc, show_ir op)
        and put op =
            "\t; " ^ show_ir op ^ "\n" ^
            match op with
            | PUTS ->
                    "\tpop  rsi\n" ^
                    "\tpop  rdx\n" ^
                    "\tcall puts\n"
            | PUTC ->
                    "\tpop rdi\n" ^
                    "\tcall putc\n"
            | PUTI ->
                    "\tpop  rdi\n" ^
                    "\tcall puti\n"

            | PUTF -> raise @@ Not_implemented (loc, "PUTF")
            | _ -> raise @@ Unreachable (show_ir op)
        and cond_jmp cond label =
            "\tpop  rax\n" ^
            sprintf "\tcmp  rax, %d\n" cond ^
            sprintf "\tje   %s\n" label
        in

        Buffer.add_string buffer @@
        match instr with
        | (FN _ | FN_END) -> ""
        | IF id ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "if_%d:\n" id
        | THEN id ->
                "\t; " ^ show_ir instr ^ "\n" ^
                cond_jmp 0
                @@ sprintf (
                    if Hashtbl.mem has_else id
                    then "else_%d"
                    else "end_if_%d") id
        | ELSE id ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "\tjmp end_if_%d\n" id ^
                sprintf "else_%d:\n" id
        | END_IF id ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "end_if_%d:\n" id

        | WHILE id ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "while_%d:\n" id
        | DO id ->
                "\t; " ^ show_ir instr ^ "\n" ^
                cond_jmp 0 @@ sprintf "end_while_%d" id 
        | END_WHILE id ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "\tjmp  while_%d\n" id ^
                sprintf "end_while_%d:\n" id

        (* TODO: if addr below 6, use only registers *)
        | PEEK (depth, addr) ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "\tmov  rax, [rsp + 8 * %d]\n" depth ^
                sprintf "\tmov  [vars + 8 * %d], rax\n" addr
        | TAKE addr ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "\tpop  [vars + 8 * %d]\n" addr
        | PUT  addr ->
                "\t; " ^ show_ir instr ^ "\n" ^
                sprintf "\tpush [vars + 8 * %d]\n" addr

        | PUSH d ->
                "\t; " ^ show_ir instr ^ "\n" ^
                (match d with
                | Int n -> sprintf "\tpush %d\n" n
                | Bool true -> "\tpush 1\n"
                | Bool false -> "\tpush 0\n"
                | Char c -> sprintf "\tpush %d\n" (int_of_char c)
                | Float f -> sprintf "\tpush %d\n" @@ Obj.magic @@ Obj.repr f
                | Str_ptr n -> sprintf "\tlea  rax, [strs + %d]\n\tpush rax\n" n)

        | EQ | NE | LT | LE | GT | GE as op -> cmp op

        | ADD | SUB | MUL | DIV | MOD
        | BAND | BOR | BXOR | LSL | LSR as op -> int_op op
        | FADD | FSUB | FMUL | FDIV as op -> float_op op
        | AND | OR as op -> bool_op op
        | (PUTC | PUTS | PUTI | PUTF) as op -> put op
    in
    Array.iter compile' @@ Array.combine program.loc program.ir;
    Buffer.add_string buffer footer;
    add_strings buffer program.strings;
    Buffer.add_string buffer @@ sprintf "vars rq %d\n" program.storage_size;

    Buffer.to_bytes buffer
