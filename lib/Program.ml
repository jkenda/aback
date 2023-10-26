open Format
open Lexer

type data =
    | Int of int
    | Bool of bool
    | Char of char
    | Float of float
    | Ptr of int
[@@deriving show { with_path = false }]

type ir =
    | PUSH of data

    | EQ | NE | LT | LE | GT | GE

    | ADD | FADD
    | SUB | FSUB
    | MUL | FMUL
    | DIV | FDIV
    | MOD | FMOD

    | PUTC | PUTS | PUTI | PUTF | PUTB

    | IF of int | THEN of int | ELSE of int | END_IF of int
    | WHILE of int | DO of int | END_WHILE of int

    (* TODO: implement with take and peek *)
    | DUP | DROP
[@@deriving show { with_path = false }]

type program = {
    ir : ir list;
    strings : string array
}

type stack = data list
[@@deriving show { with_path = false }]

let exec program =
    let int_op op = function
        | Int i :: Int j :: rest -> Int (op i j) :: rest
        | a :: b :: _ -> raise @@ Failure (
            sprintf "expected Int Int, got %s %s" (show_data a) (show_data b))
        | _ -> raise @@ Failure "not enough data on stack"
    and int_cmp op = function
        | Int i :: Int j :: rest -> Bool (op i j) :: rest
        | a :: b :: _ -> raise @@ Failure (
            sprintf "expected Int Int, got %s %s" (show_data a) (show_data b))
        | stack -> raise @@ Failure (sprintf "not enough data on stack : %s" @@ show_stack stack)
    and float_op op = function
        | Float i :: Float j :: rest -> Float (op i j) :: rest
        | a :: b :: _ -> raise @@ Failure (
            sprintf "expected Float Float, got %s %s" (show_data a) (show_data b))
        | _ -> raise @@ Failure "not enough data on stack"
    and put t = function
        | Int i   :: rest when t = PUTI -> print_int i; rest
        | Bool b  :: rest when t = PUTB -> print_bool b; rest
        | Char c  :: rest when t = PUTC -> print_char c; rest
        | Float f :: rest when t = PUTF -> print_float f; rest
        | Ptr p :: Int _ :: rest when t = PUTS -> print_string (program.strings.(p)); rest
        | [] -> raise @@ Failure (sprintf "%s: not enough data on stack" (show_ir t))
        | stack -> raise @@ Failure (sprintf "Expected %s, got %s" (show_ir t) (show_data (List.hd stack)))
    and cond_jmp stack t f =
        match stack with
        | Bool true :: tl -> tl, t
        | Bool false :: tl -> tl, f
        | _ :: _ -> raise @@ Failure (sprintf "expected bool, got %s" (show_data (List.hd stack)))
        | [] -> raise @@ Failure "not enough data on stack"
    in

    let exec' stack ip = function
        | (IF _ | WHILE _ as ir) -> raise @@ Unreachable (sprintf "%s: please run postprocess" (show_ir ir))
        | THEN addr -> cond_jmp stack (ip + 1) addr
        | ELSE addr -> stack, addr

        | DO addr -> cond_jmp stack (ip + 1) addr 
        | END_WHILE addr -> stack, addr

        | END_IF _ -> raise @@ Unreachable "END_IF: please run postprocess"

        | PUSH d -> d :: stack, ip + 1

        | EQ -> int_cmp ( =  ) stack, ip + 1
        | NE -> int_cmp ( != ) stack, ip + 1
        | LT -> int_cmp ( <  ) stack, ip + 1
        | LE -> int_cmp ( <= ) stack, ip + 1
        | GT -> int_cmp ( >  ) stack, ip + 1
        | GE -> int_cmp ( >= ) stack, ip + 1

        | ADD -> int_op ( + ) stack, ip + 1
        | SUB -> int_op ( - ) stack, ip + 1
        | MUL -> int_op ( * ) stack, ip + 1
        | DIV -> int_op ( / ) stack, ip + 1
        | MOD -> int_op (mod) stack, ip + 1

        | FADD -> float_op ( +. ) stack, ip + 1
        | FSUB -> float_op ( -. ) stack, ip + 1
        | FMUL -> float_op ( *. ) stack, ip + 1
        | FDIV -> float_op ( /. ) stack, ip + 1
        | FMOD -> float_op (fun a b ->
                let _, c = modf a in
                let _, d = modf b in
                c /. d) stack, ip + 1

        | (PUTC | PUTS | PUTI | PUTF | PUTB) as t -> put t stack, ip + 1

        | DUP -> (try List.hd stack :: stack, ip + 1 with Failure _ ->
                raise @@ Failure "DUP: empty stack")
        | DROP -> (try List.tl stack, ip + 1 with Failure _ ->
                raise @@ Failure "DROP: empty stack")
    in
    let instr = Array.of_list program.ir in

    let rec exec'' stack ip =
        if ip >= Array.length instr then stack
        else
            let stack, ip = exec' stack ip instr.(ip) in
            exec'' stack ip
    in
    exec'' [] 0


