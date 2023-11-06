open Format
open Lexer
open Preprocess

type data =
    | Int of int
    | Bool of bool
    | Char of char
    | Float of float
    | Local_ptr of string * int
[@@deriving show { with_path = false }]

let typ_of_data = function
    | Int _       -> (Int : typ)
    | Bool _      -> Bool
    | Char _      -> Char
    | Float _     -> Float
    | Local_ptr _ -> Ptr

let prep_of_data = function
    | Int _       -> Type Int
    | Bool _      -> Type Bool
    | Char _      -> Type Char
    | Float _     -> Type Float
    | Local_ptr _ -> Type Ptr

type ir =
    | PUSH of data

    | EQ | NE | LT | LE | GT | GE

    | ADD | FADD
    | SUB | FSUB
    | MUL | FMUL
    | DIV | FDIV
    | MOD

    | ITOF | FTOI

    | BAND | BOR | BXOR | LSL | LSR
    | AND  | OR

    | PUTC | PUTS | PUTI

    | SYSCALL of int

    | IF of int | THEN of int | ELSE of int | END_IF of int
    | WHILE of int | DO of int | END_WHILE of int
    | PEEK of int * int | TAKE of int
    | PUT of int
    | FN of string | FN_END
[@@deriving show { with_path = false }]

type program = {
    ir : ir array;
    loc : location array;
    strings : string;
    mem : (string, typ * int) Hashtbl.t;
    storage_size : int
}

type stack = data list
[@@deriving show { with_path = false }]

let interpret program =
    let storage = Array.make program.storage_size (Int 0) in
    let exec' stack ip instr =
        let int_op op = function
            | Int i :: Int j :: rest -> Int (op i j) :: rest
            | a :: b :: _ -> raise @@ Error (
                program.loc.(ip),
                sprintf "expected Int Int, got %s %s" (show_data a) (show_data b))
            | _ -> raise @@ Error (
                program.loc.(ip),
                "not enough data on stack")
        and cmp op = function
            | a :: b :: rest -> Bool (op a b) :: rest
            | stack -> raise @@ Error (
                program.loc.(ip),
                sprintf "not enough data on stack : %s" @@ show_stack stack)
        and float_op op = function
            | Float i :: Float j :: rest -> Float (op i j) :: rest
            | a :: b :: _ -> raise @@ Error (
                program.loc.(ip),
                sprintf "expected Float Float, got %s %s" (show_data a) (show_data b))
            | _ -> raise @@ Error (
                program.loc.(ip),
                "not enough data on stack")
        and bool_op op = function
            | Bool i :: Bool j :: rest -> Bool (op i j) :: rest
            | a :: b :: _ -> raise @@ Error (
                program.loc.(ip),
                sprintf "expected Bool Bool, got %s %s" (show_data a) (show_data b))
            | _ -> raise @@ Error (
                program.loc.(ip),
            "not enough data on stack")
        and put t = function
            | Int i   :: rest when t = PUTI -> print_int i; rest
            | Char c  :: rest when t = PUTC -> print_char c; rest
            | Local_ptr ("strs", offset) :: Int len :: rest when t = PUTS ->
                    print_string
                    @@ String.sub program.strings offset len;
                    rest
            | [] -> raise @@ Error ( program.loc.(ip),
                sprintf "%s: not enough data on stack" (show_ir t))
            | stack -> raise @@ Error ( program.loc.(ip),
                sprintf "Expected %s, got %s" (show_ir t) (show_data (List.hd stack)))
        and cond_jmp stack t f =
            match stack with
            | Bool true :: tl -> tl, t
            | Bool false :: tl -> tl, f
            | _ :: _ -> raise @@ Error ( program.loc.(ip),
                sprintf "expected bool, got %s" (show_data (List.hd stack)))
            | [] -> raise @@ Error ( program.loc.(ip),
                "not enough data on stack")
        in

        match instr with
        | (FN _ | FN_END) -> raise @@ Unreachable (sprintf "%s: please run postprocess" (show_ir instr))
        | IF _ | WHILE _ -> raise @@ Unreachable (sprintf "%s: please run postprocess" (show_ir instr))
        | THEN addr -> cond_jmp stack (ip + 1) addr
        | ELSE addr -> stack, addr
        | END_IF _ -> raise @@ Unreachable "END_IF: please run postprocess"

        | DO addr -> cond_jmp stack (ip + 1) addr 
        | END_WHILE addr -> stack, addr

        | PEEK (depth, addr) ->
                let data =
                    try List.nth stack depth
                    with _ -> raise @@ Error (program.loc.(ip), "stack underflow")
                in
                storage.(addr) <- data; stack, ip + 1
        | TAKE addr ->
                let data, stack =
                    match stack with
                    | hd :: tl -> hd, tl
                    | _ -> raise @@ Error (program.loc.(ip), "stack underflow")
                in
                storage.(addr) <- data; stack, ip + 1
        | PUT addr -> storage.(addr) :: stack, ip + 1

        | PUSH d -> d :: stack, ip + 1

        | EQ -> cmp ( =  ) stack, ip + 1
        | NE -> cmp ( <> ) stack, ip + 1
        | LT -> cmp ( <  ) stack, ip + 1
        | LE -> cmp ( <= ) stack, ip + 1
        | GT -> cmp ( >  ) stack, ip + 1
        | GE -> cmp ( >= ) stack, ip + 1

        | ADD -> int_op ( + ) stack, ip + 1
        | SUB -> int_op ( - ) stack, ip + 1
        | MUL -> int_op ( * ) stack, ip + 1
        | DIV -> int_op ( / ) stack, ip + 1
        | MOD -> int_op (mod) stack, ip + 1

        | FADD -> float_op ( +. ) stack, ip + 1
        | FSUB -> float_op ( -. ) stack, ip + 1
        | FMUL -> float_op ( *. ) stack, ip + 1
        | FDIV -> float_op ( /. ) stack, ip + 1

        | ITOF ->
                (match stack with
                | Int a :: rest -> Float (float_of_int a) :: rest, ip + 1
                | a :: _ -> raise @@ Error (program.loc.(ip),
                    sprintf "expected int, got %s" (show_data a))
                | _ -> raise @@ Error (program.loc.(ip), "not enough data on stack"))
        | FTOI ->
                (match stack with
                | Float a :: rest -> Int (int_of_float a) :: rest, ip + 1
                | a :: _ -> raise @@ Error (program.loc.(ip),
                    sprintf "expected float, got %s" (show_data a))
                | _ -> raise @@ Error (program.loc.(ip), "not enough data on stack"))

        | AND -> bool_op ( && ) stack, ip + 1
        | OR  -> bool_op ( || ) stack, ip + 1

        | BAND -> int_op ( land ) stack, ip + 1
        | BOR  -> int_op ( lor  ) stack, ip + 1
        | BXOR -> int_op ( lxor ) stack, ip + 1
        | LSL  -> int_op ( lsl  ) stack, ip + 1
        | LSR  -> int_op ( lsr  ) stack, ip + 1

        | SYSCALL n ->
                raise @@ Error (program.loc.(ip),
                    sprintf "syscall %d not implemented" n)

        | (PUTC | PUTS | PUTI) as t -> put t stack, ip + 1
    in

    let rec exec'' stack ip =
        if ip >= Array.length program.ir then stack
        else
            let stack, ip = exec' stack ip program.ir.(ip) in
            exec'' stack ip
    in
    let stack =
        try exec'' [] 0
        with Error (loc, msg) -> raise @@ Error (loc, "runtime exception: " ^ msg)
    in

    match stack with
    | [] -> ()
    | stack ->
            let stack = List.map typ_of_data stack in
            raise @@ Error (program.loc.(Array.length program.loc - 1),
                sprintf "%s left on the stack at the end of program"
                (print_typ_stack stack))
