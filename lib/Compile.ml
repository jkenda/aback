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
    je puts_flush

    ; flush if full
    cmp [wblen], wbsiz
    jge puts_flush

    jmp puts_inc

puts_flush:
    call flush

puts_inc:
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
        | n, r :: tl -> get' (acc ^ sprintf "pop %s" r) (n - 1, tl)
    in
    get' "" (n, arg_regs)

let add_strings buffer strings =
    Buffer.add_string buffer "strs db \"";
    String.iter (function
        | '\000' | '\t' | '\r' | '\n' | '"' as c ->
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

    let compile' acc (loc, instr) =
        let cmp op =
            (*"; " ^ show_ir op ^ "" ^ *)
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
            [
                ["pop"; "rax"];
                ["pop"; "rbx"];
                ["xor rcx, rcx"];
                ["cmp rax, rbx"];
                ["set" ^ op ^ " cl"];
                ["push"; "rcx"]
            ]
        and int_op op =
            (* "; " ^ show_ir op ^ "" ^ *)
                [["pop"; "rax"]] @
                [["pop"; "rcx"]] @
                (match op with
                | ADD -> [["add";  "rax"; ","; "rcx"]]
                | SUB -> [["sub";  "rax"; ","; "rcx"]]
                | MUL -> [["imul"; "rax"; ","; "rcx"]]
                | DIV | MOD ->
                        [["cdq"];
                        ["idiv"; "rcx"]]
                | BAND -> [["and"; "rax"; ","; "rcx"]]
                | BXOR -> [["or "; "rax"; ","; "rcx"]]
                | BOR  -> [["xor"; "rax"; ","; "rcx"]]
                | LSL  -> [["shl"; "rax"; ","; "cl" ]]
                | LSR  -> [["shr"; "rax"; ","; "cl" ]]
                | _ -> raise @@ Unreachable (show_ir op)) @
                match op with
                | MOD -> [["push"; "rdx"]]
                | _   -> [["push"; "rax"]]
        and float_op op =
            (* "; " ^ show_ir op ^ "" ^ *)
            [
                ["movsd xmm0, [rsp]"];
                [match op with
                | FADD -> "addsd xmm0, [rsp + 8]"
                | FSUB -> "subsd xmm0, [rsp + 8]"
                | FMUL -> "mulsd xmm0, [rsp + 8]"
                | FDIV -> "divsd xmm0, [rsp + 8]"
                | _ -> raise @@ Unreachable (show_ir op)];
                ["pop qword [rsp - 8]"];
                ["movsd [rsp], xmm0"]
            ]
        and bool_op op =
            (* "; " ^ show_ir op ^ "" ^ *)
            [
                ["pop"; "rax"];
                ["pop"; "rbx"];
                [match op with
                | AND -> "and  rax, rbx"
                | OR  -> "or   rax, rbx"
                | _ -> raise @@ Unreachable (show_ir op)];
                ["push"; "rax"]
            ]
        and put op =
            (* "; " ^ show_ir op ^ "" ^ *)
            match op with
            | PUTS -> [
                    ["pop"; "rsi"];
                    ["pop"; "rdx"];
                    ["call puts"]]
            | PUTC -> [
                    ["pop"; "rdi"];
                    ["call putc"]]
            | PUTI -> [
                    ["pop"; "rdi"];
                    ["call puti"]]

            | _ -> raise @@ Unreachable (show_ir op)
        and cond_jmp cond label =
            [
                ["pop"; "rax"];
                [sprintf "cmp  rax, %d" cond];
                [sprintf "je   %s" label]
            ]
        in

        (match instr with
        | (FN _ | FN_END) -> []
        | IF id -> [
                (* "; " ^ show_ir instr ^ "" ^ *)
                [sprintf "if_%d:" id]]
        | THEN id ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                cond_jmp 0
                @@ sprintf (
                    if Hashtbl.mem has_else id
                    then "else_%d"
                    else "end_if_%d") id
        | ELSE id -> [
                (* "; " ^ show_ir instr ^ "" ^ *)
                [sprintf "jmp end_if_%d" id];
                [sprintf "else_%d:" id]]
        | END_IF id -> [
                (* "; " ^ show_ir instr ^ "" ^ *)
                [sprintf "end_if_%d:" id]]

        | WHILE id -> [
                (* "; " ^ show_ir instr ^ "" ^ *)
                [sprintf "while_%d:" id]]
        | DO id ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                cond_jmp 0 @@ sprintf "end_while_%d" id
        | END_WHILE id -> [
                (* "; " ^ show_ir instr ^ "" ^ *)
                [sprintf "jmp  while_%d" id];
                [sprintf "end_while_%d:" id]]

        (* TODO: if addr below 6, use only registers *)
        | PEEK (depth, addr) ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                let addr = program.storage_size - addr in
                let reg =
                    if addr < 6 then sprintf "r1%d" addr
                    else "rax" 
                in
                    ["mov"; sprintf "%s, [rsp + 0x%x]" reg (8 * depth)] ::
                        if addr >= 6 then [["mov"; sprintf "[vars + 0x%x], rax" (8 * (addr - 6))]]
                        else []
        | TAKE addr ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                let addr = program.storage_size - addr in
                let loc =
                    if addr < 6 then sprintf "r1%d" addr
                    else sprintf "[vars + 0x%x]" (8 * (addr - 6))
                in
                [["pop"; loc]]
        | PUT  addr ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                let addr = program.storage_size - addr in
                let loc =
                    if addr < 6 then sprintf "r1%d" addr
                    else sprintf "[vars + 0x%x]" (8 * (addr - 6))
                in
                [["push"; loc]]

        | LOAD t ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                let size =
                    match t with
                    | Char -> "word"
                    | _    -> "qword"
                in
                [
                    ["pop"; "rax"];
                    ["push"; size ^ " [rax]"]
                ]
        | STORE t ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                let size =
                    match t with
                    | Char -> "word"
                    | _    -> "qword"
                in
                [
                    ["pop"; "rax"];
                    [sprintf "pop"; size ^ " [rax]"]
                ]

        | PUSH d ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                (match d with
                | Int i ->
                        if i > 0xFF then [
                            ["mov"; "rax"; ","; string_of_int i];
                            ["push"; "rax"]]
                        else [
                            ["push"; string_of_int i]]
                | Float f ->
                        let i = Int64.of_nativeint @@ Obj.raw_field (Obj.repr f) 0 in
                        if i > (Int64.of_int 0xFF) then [
                            [sprintf "mov rax, %s" (Int64.to_string i)];
                            ["push"; "rax"]]
                        else [
                            [sprintf "push"; Int64.to_string i]]
                | Bool true -> [["push"; "1"]]
                | Bool false -> [["push"; "0"]]
                | Char c -> [["push"; string_of_int @@ int_of_char c]]
                | Local_ptr (space, off) ->
                        if off = 0 then [
                            ["push"; space]]
                        else [
                            ["lea"; sprintf "rax, [%s + %d]" space off];
                            ["push"; "rax"]])

        | SYSCALL nargs ->
                (* "; " ^ show_ir instr ^ "" ^ *)
                [
                    ["pop"; "rax"];
                    pop_syscall_regs loc nargs ::
                    ["syscall"];
                    ["push"; "rax"]
                ]

        | ITOF -> [
                (* "; " ^ show_ir instr ^ "" ^ *)
                ["cvtsi2sd xmm0, [rsp]"];
                ["movsd [rsp], xmm0"]]
        | FTOI -> [
                (* "; " ^ show_ir instr ^ "" ^ *)
                ["cvttsd2si rax, [rsp]"];
                ["mov [rsp], rax"]]

        | EQ | NE | LT | LE | GT | GE as op -> cmp op

        | ADD | SUB | MUL | DIV | MOD
        | BAND | BOR | BXOR | LSL | LSR as op -> int_op op
        | FADD | FSUB | FMUL | FDIV as op -> float_op op
        | AND | OR as op -> bool_op op
        | (PUTC | PUTS | PUTI) as op -> put op) :: acc
    in
    let rec opti passes instrs =
        let rec opti' acc = function
            | [] -> List.rev acc
            | ["push"; a] :: ["pop"; b] :: tl when a = b -> opti' acc tl
            | ["push"; a] :: ["push"; b] :: ["pop"; c] :: ["pop"; d] :: [op; e; ","; f] :: tl
                when c = e && d = f ->
                    opti' ([op; c; ","; b] :: ["mov"; c; ","; a] :: acc) tl
            | ["push"; a] :: ["pop"; b] :: tl -> opti' (["mov"; b; ","; a] :: acc) tl
            | instr :: instrs -> opti' (instr :: acc) instrs
        in
        let next_pass = opti' [] instrs in
        if next_pass = instrs then passes, instrs
        else opti (passes + 1) next_pass
    in

    let passes, instrs =
        Array.fold_left compile' [] (Array.combine program.loc program.ir)
        |> List.rev
        |> List.flatten
        |> opti 1
    in
    printf "optimization: %d passes\n%!" passes;
    List.iter (function
        | [instr] when String.ends_with ~suffix:":" instr ->
                Buffer.add_string buffer instr;
                Buffer.add_char buffer '\n'
        | instr :: data ->
                Buffer.add_char buffer '\t';
                Buffer.add_string buffer (sprintf "%-4s " instr);
                List.iter (fun word -> Buffer.add_string buffer word; Buffer.add_char buffer ' ') data;
                Buffer.add_char buffer '\n'
        | instr ->
                Buffer.add_char buffer '\t';
                List.iter (fun word -> Buffer.add_string buffer word; Buffer.add_char buffer ' ') instr;
                Buffer.add_char buffer '\n'
        ) instrs;
    Buffer.add_string buffer footer;
    add_strings buffer program.strings;
    Hashtbl.iter (fun name (typ, space) ->
        let typ_to_str = function
            | (Char : typ) -> "rb"
            | _ -> "rq"
        in
        Buffer.add_string buffer @@ sprintf "mem_%s %s %d\n" name (typ_to_str typ) space;
    ) program.mem;
    Buffer.add_string buffer @@ sprintf "vars rq %d\n" program.storage_size;
    Buffer.add_string buffer "wblen dq 0\n";
    Buffer.add_string buffer "writebuf rb wbsiz\n";

    Buffer.to_bytes buffer
