open Format
open Lexer
open Program

let header = "
wbsiz equ 1024

format ELF64 executable
use64

segment readable executable
entry $
	; set base pointer
	mov rbp, rsp

"

let footer = "
    ; exit 0
    mov rdi, 0
    jmp exit

; puts(rsi, rdx)
puts:
    mov rcx, 0
puts_loop:
    cmp rcx, rdx
    jge puts_ret

    mov dil, [rsi]

    ; put char into buffer
    mov rax, [wblen]
    mov [writebuf + rax], dil
    inc [wblen]

    ; flush on newline
    cmp dil, 10
    je flush

    inc rsi
    inc rcx
    jmp puts_loop
puts_ret:
    ret

; putc(rdi)
putc:
    mov rax, [wblen]
    mov [writebuf + rax], dil
    inc [wblen]

    ; flush on newline
    cmp dil, 10
    je flush

    ; flush if full
    cmp [wblen], wbsiz
    jge flush

    ret

flush:
    ; write(STDOUT, writebuf, *wblen)
    mov rax, 1 ; SYS_write
    mov rdi, 1 ; STDOUT
    mov rsi, writebuf
    mov rdx, [wblen]
	syscall
    mov [wblen], 0
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

; exit(rdi)
exit:
    push rdi
    call flush
    mov rax, 60
    pop rdi
    syscall


segment readable writable
"

let arg_regs = ["rdi"; "rsi"; "rdx"; "r10"; "r8"; "r9"]
let pop_syscall_regs loc n =
    let rec get' acc = function
        | n, _ when n < 0 -> raise @@ Error (loc, "syscall number must be > 0")
        | _, [] -> raise @@ Error (loc, "syscall number too large")
        | 0, _ -> acc
        | n, r :: tl -> get' (acc ^ sprintf "\tpop %s\n" r) (n - 1, tl)
    in
    get' "" (n, arg_regs)

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

    let compile' i (_, instr) =
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
            "\tpop  rcx\n" ^
            (match op with
            | ADD -> "\tadd  rax, rcx\n"
            | SUB -> "\tsub  rax, rcx\n"
            | MUL -> "\timul rax, rcx\n"
            | DIV | MOD ->
                    "\tcdq\n" ^
                    "\tidiv  rcx\n"
            | BAND -> "\tand  rax, rcx\n"
            | BXOR -> "\tor   rax, rcx\n"
            | BOR  -> "\txor  rax, rcx\n"
            | LSL  -> "\tshl  rax, cl\n"
            | LSR  -> "\tshr  rax, cl\n"
            | _ -> raise @@ Unreachable (show_ir op)) ^
            match op with
            | MOD -> "\tpush  rdx\n"
            | _ -> "\tpush rax\n"
        and float_op op =
            "\t; " ^ show_ir op ^ "\n" ^
            "\tmovsd xmm0, [rsp]\n" ^
            (match op with
            | FADD -> "\taddsd xmm0, [rsp + 8]\n"
            | FSUB -> "\tsubsd xmm0, [rsp + 8]\n"
            | FMUL -> "\tmulsd xmm0, [rsp + 8]\n"
            | FDIV -> "\tdivsd xmm0, [rsp + 8]\n"
            | _ -> raise @@ Unreachable (show_ir op)) ^
            "\tpop qword [rsp - 8]\n" ^
            "\tmovsd [rsp], xmm0\n"
        and bool_op op =
            "\t; " ^ show_ir op ^ "\n" ^
            "\tpop  rax\n" ^
            "\tpop  rbx\n" ^
            (match op with
            | AND -> "\tand  rax, rbx\n"
            | OR  -> "\tor   rax, rbx\n"
            | _ -> raise @@ Unreachable (show_ir op)) ^
            "\tpush rax\n"
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
                let addr = program.storage_size - addr in
                let reg =
                    if addr < 6 then sprintf "r1%d" addr
                    else "rax" 
                in
                sprintf "\tmov %s, [rsp + 8 * %d]\n" reg depth ^
                if addr >= 6 then sprintf "\tmov  [vars + 8 * %d], rax\n" (addr - 6)
                else ""
        | TAKE addr ->
                "\t; " ^ show_ir instr ^ "\n" ^
                let addr = program.storage_size - addr in
                let loc =
                    if addr < 6 then sprintf "r1%d" addr
                    else sprintf "[vars + 8 * %d]" (addr - 6)
                in
                sprintf "\tpop  %s\n" loc
        | PUT  addr ->
                "\t; " ^ show_ir instr ^ "\n" ^
                let addr = program.storage_size - addr in
                let loc =
                    if addr < 6 then sprintf "r1%d" addr
                    else sprintf "[vars + 8 * %d]" (addr - 6)
                in
                sprintf "\tpush %s\n" loc

        | LOAD t ->
                "\t; " ^ show_ir instr ^ "\n" ^
                let size =
                    match t with
                    | Char -> "word"
                    | _    -> "qword"
                in
                "\tpop rax\n" ^
                sprintf "\tpush %s [rax]\n" size
        | STORE t ->
                "\t; " ^ show_ir instr ^ "\n" ^
                let size =
                    match t with
                    | Char -> "word"
                    | _    -> "qword"
                in
                "\tpop rax\n" ^
                sprintf "\tpop %s [rax]\n" size

        | PUSH d ->
                "\t; " ^ show_ir instr ^ "\n" ^
                (match d with
                | Int i ->
                        if i > 0xFF then
                            sprintf "\tmov rax, %d\n" i ^
                            sprintf "\tpush rax\n"
                        else
                            sprintf "\tpush %d\n" i
                | Float f ->
                        let i = Int64.of_nativeint @@ Obj.raw_field (Obj.repr f) 0 in
                        if i > (Int64.of_int 0xFF) then
                            sprintf "\tmov rax, %s\n" (Int64.to_string i) ^
                            sprintf "\tpush rax\n"
                        else
                            sprintf "\tpush %s\n" (Int64.to_string i)
                | Bool true -> "\tpush 1\n"
                | Bool false -> "\tpush 0\n"
                | Char c -> sprintf "\tpush %d\n" (int_of_char c)
                | Local_ptr (space, off) -> sprintf "\tlea rax, [%s + %d]\n\tpush rax\n" space off)

        | SYSCALL nargs ->
                "\t; " ^ show_ir instr ^ "\n" ^
                let loc = program.loc.(i) in
                "\tpop rax\n" ^
                pop_syscall_regs loc nargs ^
                "\tsyscall\n" ^
                "\tpush rax\n"

        | ITOF ->
                "\t; " ^ show_ir instr ^ "\n" ^
                "\tcvtsi2sd xmm0, [rsp]\n" ^
                "\tmovsd [rsp], xmm0\n"
        | FTOI ->
                "\t; " ^ show_ir instr ^ "\n" ^
                "\tcvttsd2si rax, [rsp]\n" ^
                "\tmov [rsp], rax\n"

        | EQ | NE | LT | LE | GT | GE as op -> cmp op

        | ADD | SUB | MUL | DIV | MOD
        | BAND | BOR | BXOR | LSL | LSR as op -> int_op op
        | FADD | FSUB | FMUL | FDIV as op -> float_op op
        | AND | OR as op -> bool_op op
        | (PUTC | PUTS | PUTI) as op -> put op
    in
    Array.iteri compile' @@ Array.combine program.loc program.ir;
    Buffer.add_string buffer footer;
    add_strings buffer program.strings;
    Buffer.add_string buffer @@ sprintf "vars rq %d\n" program.storage_size;
    Hashtbl.iter (fun name (typ, space) ->
        let typ_to_str = function
            | (Char : typ) -> "rb"
            | _ -> "rq"
        in
        Buffer.add_string buffer @@ sprintf "mem_%s %s %d\n" name (typ_to_str typ) space;
    ) program.mem;
    Buffer.add_string buffer "wblen dq 0\n";
    Buffer.add_string buffer "writebuf rb wbsiz\n";

    Buffer.to_bytes buffer
