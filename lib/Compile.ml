open Format
open Lexer
open Program

let header =
"format ELF64 executable
use64

segment readable executable
entry $
	; set base pointer
	mov rbp, rsp

"

let footer =
"
    ; exit 0
    mov rdi, 0
    jmp exit

wbsiz equ 1024

; puts(rsi, rdx)
puts:
    mov rcx, 0
.puts_loop:
    cmp rcx, rdx
    jge .puts_ret

    mov dil, [rsi]

    ; put char into buffer
    mov rax, [wblen]
    mov [writebuf + rax], dil
    inc [wblen]

    ; flush on newline
    cmp dil, 10
    je .puts_flush

    ; flush if full
    cmp [wblen], wbsiz
    jge .puts_flush

    jmp .puts_inc

.puts_flush:
    call flush

.puts_inc:
    inc rsi
    inc rcx
    jmp .puts_loop
.puts_ret:
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

; exit(rdi)
exit:
    push rdi
    call flush
    mov rax, 60
    pop rdi
    syscall
"

let arg_regs = ["rdi"; "rsi"; "rdx"; "r10"; "r8"; "r9"]
let pop_syscall_regs loc n =
    let rec get' acc = function
        | n, _ when n < 0 -> raise @@ Error (loc, "syscall number must be > 0")
        | _, [] -> raise @@ Error (loc, "syscall number too large")
        | 0, _ -> List.rev acc
        | n, r :: tl -> get' (["pop"; r] :: acc) (n - 1, tl)
    in
    get' [] (n, arg_regs)

let neg_comp = function
    | "e"  -> "ne"
    | "ne" -> "e"
    | "l"  -> "ge"
    | "le" -> "g"
    | "g"  -> "le"
    | "ge" -> "l"
    | _ -> raise @@ Unreachable "cond"

let cmp_op op =
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
        ["cmp"; "rax"; ","; "rbx"];
        ["set" ^ op; "al"];
        ["push"; "rax"]
    ]

let int_op op =
        [["pop"; "rax"]] @
        [["pop"; "rcx"]] @
        (match op with
        | ADD -> [["add";  "rax"; ","; "rcx"]]
        | SUB -> [["sub";  "rax"; ","; "rcx"]]
        | MUL -> [["imul"; "rax"; ","; "rcx"]]
        | DIV | MOD ->
                [["cdq"];
                ["idiv"; "rcx"]]
        | LAND -> [["and"; "rax"; ","; "rcx"]]
        | LXOR -> [["or "; "rax"; ","; "rcx"]]
        | LOR  -> [["xor"; "rax"; ","; "rcx"]]
        | LSL  -> [["shl"; "rax"; ","; "cl" ]]
        | LSR  -> [["shr"; "rax"; ","; "cl" ]]
        | _ -> raise @@ Unreachable (show_ir op)) @
        match op with
        | MOD -> [["push"; "rdx"]]
        | _   -> [["push"; "rax"]]

let float_op op = [
        ["movsd"; "xmm0"; ","; "[rsp]"];
        (match op with
        | FADD -> ["addsd"; "xmm0"; ","; "[rsp + 8]"]
        | FSUB -> ["subsd"; "xmm0"; ","; "[rsp + 8]"]
        | FMUL -> ["mulsd"; "xmm0"; ","; "[rsp + 8]"]
        | FDIV -> ["divsd"; "xmm0"; ","; "[rsp + 8]"]
        | _ -> raise @@ Unreachable (show_ir op));
        ["pop"; "qword [rsp - 8]"];
        ["movsd"; "[rsp]"; ","; "xmm0"]
    ]

let bool_op op = [
        ["pop"; "rax"];
        ["pop"; "rbx"];
        (match op with
        | AND -> ["and"; "rax, rbx"]
        | OR  -> ["or" ; "rax, rbx"]
        | _ -> raise @@ Unreachable (show_ir op));
        ["push"; "rax"]
    ]

let put_op = function
    | PUTS -> [
            ["pop";  "rsi"];
            ["pop";  "rdx"];
            ["call"; "puts"]]
    | PUTC -> [
            ["pop";  "rdi"];
            ["call"; "putc"]]
    | PUTI -> [
            ["pop";  "rdi"];
            ["call"; "puti"]]

    | op -> raise @@ Unreachable (show_ir op)

let cond_jmp cond label =
    [
        ["pop"; "rax"];
        ["cmp"; "al"; ","; string_of_int cond];
        [sprintf "je"; label]
    ]

let to_fasm_x64_linux program =
    let buffer =
        Buffer.of_seq
        @@ String.to_seq header
    in

    let has_else =
        let has_else' = Hashtbl.create 10 in
        let collect' = function
            | ELSE id -> Hashtbl.replace has_else' id ()
            | _ -> ()
        in
        Array.iter collect' program.ir;
        has_else'
    in

    let compile' acc (loc, instr) =
        (match instr with
        | (FN _ | FN_END) -> []
        | IF id ->
                [[sprintf ".if_%d:" id]]
        | THEN id ->
                cond_jmp 0
                @@ sprintf (
                    if Hashtbl.mem has_else id
                    then ".else_%d"
                    else ".end_if_%d") id
        | ELSE id -> [
                ["jmp"; sprintf ".end_if_%d" id];
                [sprintf ".else_%d:" id]]
        | END_IF id ->
                [[sprintf ".end_if_%d:" id]]

        | WHILE id ->
                [[sprintf ".while_%d:" id]]
        | DO id ->
                cond_jmp 0
                @@ sprintf ".end_while_%d" id
        | END_WHILE id -> [
                ["jmp"; sprintf ".while_%d" id];
                [sprintf ".end_while_%d:" id]]

        (*
            TODO: when you add procedures, this will have to change.
            It won't be as simple, PUT will have to know if it's coming before or after a rewrite,
            TAKE and PEEK will have to know if they're ever rewritten.
            For starters just remove the optimization and let them always read and write to memory.
        *)
        | PEEK (depth, addr) ->
                let addr = program.storage_size - addr in
                let reg =
                    if addr < 6 then sprintf "r1%d" addr
                    else "rax" 
                in
                    ["mov"; reg; ","; sprintf "[rsp + 0x%x]" (8 * depth)] ::
                        if addr >= 6 then [["mov"; sprintf "[takes + 0x%x]" (8 * (addr - 6)); ","; "rax"]]
                        else []
        | TAKE addr ->
                let addr = program.storage_size - addr in
                let loc =
                    if addr < 6 then sprintf "r1%d" addr
                    else sprintf "[takes + 0x%x]" (8 * (addr - 6))
                in
                [["pop"; loc]]
        | PUT addr ->
                let addr = program.storage_size - addr in
                let loc =
                    if addr < 6 then sprintf "r1%d" addr
                    else sprintf "[takes + 0x%x]" (8 * (addr - 6))
                in
                [["push"; loc]]

        | LOAD t ->
                let reg =
                    match t with
                    | Char -> "dil"
                    | _    -> "rdi"
                in
                [["pop"; "rax"];
                 ["mov"; reg; ","; "[rax]"];
                 ["push"; "rdi"]]
        | STORE t ->
                let reg =
                    match t with
                    | Char -> "dil"
                    | _    -> "rdi"
                in
                [["pop"; "rax"];
                 ["pop"; "rdi"];
                 ["mov"; "[rax]"; ","; reg]]

        | PUSH d ->
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
                            [sprintf "mov"; "rax"; ","; (Int64.to_string i)];
                            ["push"; "rax"]]
                        else [
                            [sprintf "push"; Int64.to_string i]]
                | Bool true -> [["push"; "1"]]
                | Bool false -> [["push"; "0"]]
                | Char c -> [["push"; string_of_int @@ int_of_char c]]
                | Ptr (space, off) ->
                        if off = 0 then [
                            ["push"; space]]
                        else [
                            ["lea"; "rax"; ","; sprintf "[%s + %d]" space off];
                            ["push"; "rax"]])

        | SYSCALL nargs ->
                ["pop"; "rax"] ::
                pop_syscall_regs loc nargs @
                [["syscall"];
                ["push"; "rax"]]

        | ITOF -> [
                ["cvtsi2sd xmm0, [rsp]"];
                ["movsd [rsp], xmm0"]]
        | FTOI -> [
                ["cvttsd2si rax, [rsp]"];
                ["mov [rsp], rax"]]

        | EQ | NE | LT | LE | GT | GE as op -> cmp_op op

        | ADD | SUB | MUL | DIV | MOD
        | LAND | LOR | LXOR | LSL | LSR as op -> int_op op
        | FADD | FSUB | FMUL | FDIV as op -> float_op op
        | AND | OR as op -> bool_op op
        | (PUTC | PUTS | PUTI) as op -> put_op op) :: acc
    in

    let rec opti passes instrs =
        let rec opti' acc = function
            | [] -> List.rev acc

            (* load/store *)
            | ["mov"; "rax"; ","; off] :: ["imul"; "rax"; ","; mul] :: ["add"; "rax"; ","; addr] :: tl
                when not (String.contains off  '[' ||
                          String.contains mul  '[' ||
                          String.contains addr '[') ->
                    opti' (["lea"; "rax"; ","; sprintf "[%s + %s * %s]" addr off mul] :: acc) tl
            | ["mov"; "rax"; ","; addr] :: ["mov"; reg; ","; "qword [rax]"] :: tl
                when reg.[0] = 'r' ->
                    opti' (["mov"; reg; ","; "[" ^ addr ^ "]"] :: acc) tl
            | ["push"; a] :: ["mov"; "rax"; ","; addr] :: ["pop"; "qword [rax]"] :: tl
                when addr.[0] = 'r' ->
                    opti' (["mov"; "[" ^  addr ^ "]"; ","; a] :: acc) tl

            | ["lea"; "rax"; ","; addr] :: ["push"; "qword [rax]"] :: tl ->
                    opti' (["push"; addr] :: acc) tl
            | ["lea"; "rax"; ","; addr] :: ["pop"; "qword [rax]"] :: tl ->
                    opti' (["pop"; addr] :: acc) tl
            | ["lea"; "rax"; ","; addr] :: ["mov"; a; ","; "qword [rax]"] :: tl ->
                    opti' (["mov"; a; ","; addr] :: acc) tl

            (* arith. operations *)
            | ["push"; a] :: ["push"; b] :: ["pop"; r1] :: ["pop"; r2] :: [op; e; ","; f] :: tl
                when a = r1 && r1 = e && r2 = f ->
                    opti' ([op; r1; ","; b] :: acc) tl
            | ["push"; a] :: ["push"; b] :: ["pop"; r1] :: ["pop"; r2] :: [op; e; ","; f] :: tl
                when r1 = e && r2 = f ->
                    opti' ([op; r1; ","; b] :: ["mov"; r1; ","; a] :: acc) tl

            | ["push"; b] :: ["mov"; r1; ","; a] :: ["pop"; r2] :: [op; e; ","; f] :: tl
                when r1 = e && r2 = f && List.mem op ["add"; "sub"] ->
                    opti' ([op; r1; ","; b] :: ["mov"; r1; ","; a] :: acc) tl

            (* cond. jump *)
            | [set; "al"] :: ["cmp"; "al"; ","; value] :: ["je"; label] :: tl
                when String.starts_with ~prefix:"set" set ->
                    let cond = String.sub set 3 (String.length set - 3) in
                    let cond =
                        if value = "0" then neg_comp cond
                        else cond
                    in
                    opti' (["j" ^ cond; label] :: acc) tl

            (* push-pop *)
            | ["push"; src] :: ["pop"; dst] :: tl when src = dst ->
                    opti' acc tl
            | ["push"; src] :: ["pop"; dst] :: tl
                when not (String.starts_with ~prefix:"[" src &&
                          String.starts_with ~prefix:"[" dst) ->
                              opti' (["mov"; dst; ","; src] :: acc) tl

            (* mov-mov *)
            | ["mov"; r0; ","; src] :: ["mov"; dst; ","; r1] :: tl
                when r0 = r1 && r0.[0] = 'r' &&
                    not (String.starts_with ~prefix:"[" src &&
                         String.starts_with ~prefix:"[" dst) ->
                    opti' (["mov"; dst; ","; src] :: acc) tl
            | ["mov"; a; ","; b] as mova :: ["mov"; c; ","; d] :: tl
                when a = d && b = c ->
                    opti' (mova :: acc) tl
            | hd :: tl -> opti' (hd :: acc) tl
        in
        let next_pass = opti' [] instrs in
        if next_pass = instrs then passes, instrs
        else opti (passes + 1) next_pass
    in

    let passes, instrs =
        Array.fold_left compile' [] (Array.combine program.loc program.ir)
        |> List.rev
        |> List.flatten
        |> opti 0
    in
    printf "optimization: %d passes\n%!" passes;

    (* add instructions *)
    List.iter (function
        | [instr] when String.ends_with ~suffix:":" instr ->
                Buffer.add_string buffer instr;
                Buffer.add_char buffer '\n'
        | instr :: data ->
                Buffer.add_char buffer '\t';
                Buffer.add_string buffer (sprintf "%-5s " instr);
                List.iter (fun word -> Buffer.add_string buffer word; Buffer.add_char buffer ' ') data;
                Buffer.add_char buffer '\n'
        | instr ->
                Buffer.add_char buffer '\t';
                List.iter (fun word -> Buffer.add_string buffer word; Buffer.add_char buffer ' ') instr;
                Buffer.add_char buffer '\n'
        ) instrs;

    (* add footer *)
    Buffer.add_string buffer footer;

    Buffer.add_string buffer "\nsegment readable\n";

    (* add strings *)
    Buffer.add_string buffer "strs db ";
    ignore @@
    String.fold_left (fun prev_was_num c ->
        match c with
        | '\000' | '\t' | '\r' | '\n' | '"' as c ->
                Buffer.add_string buffer @@
                if prev_was_num then
                    sprintf "%d, " @@ Char.code c
                else
                    sprintf "\", %d, " @@ Char.code c;
                true
        | c ->
                if prev_was_num then
                    Buffer.add_string buffer @@
                    sprintf "\"%c" c
                else
                    Buffer.add_char buffer c;
                false) true program.strings;
    Buffer.truncate buffer (Buffer.length buffer - 2);
    Buffer.add_char buffer '\n';

    Buffer.add_string buffer "\nsegment readable writeable\n";

    (* reserve space for vars *)
    Hashtbl.iter (fun name typ ->
        let typ_to_str = function
            | (Char : typ) -> "rb"
            | _ -> "rq"
        in
        Buffer.add_string buffer @@ sprintf "var_%s %s %d\n" name (typ_to_str typ) 1;
    ) program.vars;

    (* reserve space for arrays *)
    Hashtbl.iter (fun name (typ, space) ->
        let typ_to_str = function
            | (Char : typ) -> "rb"
            | _ -> "rq"
        in
        Buffer.add_string buffer @@ sprintf "mem_%s %s %d\n" name (typ_to_str typ) space;
    ) program.mem;

    (* reserve space for take/peek vars *)
    Buffer.add_string buffer @@ sprintf "takes rq %d\n" program.storage_size;

    (* reserve space for write buffer *)
    Buffer.add_string buffer "wblen dq 0\n";
    Buffer.add_string buffer "writebuf rb wbsiz\n";

    Buffer.to_bytes buffer
