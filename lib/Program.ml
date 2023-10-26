open Format

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
[@@deriving show { with_path = false }]

type program = {
    ir : ir list;
    strings : string array
}

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
        | _ -> raise @@ Failure "not enough data on stack"
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
    in
    let exec' stack = function
        | PUSH d -> d :: stack

        | EQ -> int_cmp ( = ) stack
        | NE -> int_cmp ( = ) stack
        | LT -> int_cmp ( = ) stack
        | LE -> int_cmp ( = ) stack
        | GT -> int_cmp ( = ) stack
        | GE -> int_cmp ( = ) stack

        | ADD -> int_op ( + ) stack
        | SUB -> int_op ( - ) stack
        | MUL -> int_op ( * ) stack
        | DIV -> int_op ( / ) stack
        | MOD -> int_op (mod) stack

        | FADD -> float_op ( +. ) stack
        | FSUB -> float_op ( -. ) stack
        | FMUL -> float_op ( *. ) stack
        | FDIV -> float_op ( /. ) stack
        | FMOD -> float_op (fun a b ->
                let _, c = modf a in
                let _, d = modf b in
                c /. d) stack

        | (PUTC | PUTS | PUTI | PUTF | PUTB) as t -> put t stack
    in
    List.fold_left exec' [] program.ir
