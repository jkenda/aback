open Format
open Lexer
open Preprocess

type data =
    | Int of int
    | Bool of bool
    | Char of char
    | Float of float
    | Ptr of string * int
[@@deriving show { with_path = false }]

let typ_of_data = function
    | Int _       -> (Int : typ)
    | Bool _      -> Bool
    | Char _      -> Char
    | Float _     -> Float
    | Ptr _ -> Ptr

let prep_of_data = function
    | Int _       -> Type Int
    | Bool _      -> Type Bool
    | Char _      -> Type Char
    | Float _     -> Type Float
    | Ptr _ -> Type Ptr

type ir =
    | PUSH of data

    | EQ | NE | LT | LE | GT | GE

    | ADD | FADD
    | SUB | FSUB
    | MUL | FMUL
    | DIV | FDIV
    | MOD

    | ITOF | FTOI

    | LAND | LOR | LXOR | LSL | LSR
    | AND  | OR

    | PUTC | PUTS | PUTI

    | SYSCALL of int

    | IF of int | THEN of int | ELSE of int | END_IF of int
    | WHILE of int | DO of int | END_WHILE of int
    | PEEK of int * int | TAKE of int
    | PUT of int
    | LOAD of typ | STORE of typ
    | FN of string | FN_END
[@@deriving show { with_path = false }]

type program = {
    ir : ir array;
    loc : location array;
    strings : string;
    vars : (string, typ) Hashtbl.t;
    mem : (string, typ * int) Hashtbl.t;
    storage_size : int
}

type stack = data list
[@@deriving show { with_path = false }]

let typ_null = function
    | (Int : typ) -> Int 0
    | Float -> Float 0.0
    | Char -> Char '\000'
    | Bool -> Bool false
    | Ptr -> Ptr ("", 0)
    | String -> Ptr ("", 0)
    | CStr -> Ptr ("", 0)

let interpret program =
    let takes = Array.make program.storage_size (Int 0)
    and mem = Hashtbl.create (Hashtbl.length program.mem) in

    Hashtbl.iter (fun space (typ, size) ->
        Hashtbl.add mem ("mem_" ^ space) (Array.make size (typ_null typ)))
    program.mem;
    Hashtbl.iter (fun space typ ->
        Hashtbl.add mem ("var_" ^ space) (Array.make 1 (typ_null typ)))
    program.vars;

    let exec' stack ip instr =
        let int_op op = function
            | Int a :: Int b :: rest -> Int (op a b) :: rest
            | Char a :: Int b :: rest -> Char (char_of_int (op (int_of_char a) b)) :: rest
            | Ptr (space, a) :: Int b :: rest ->
                Ptr (space, a + b) :: rest
            | Ptr (space, a) :: Ptr (space', b) :: rest when space = space' ->
                Int (a - b) :: rest
            | a :: b :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected int int, got %s %s" (show_data a) (show_data b))
            | _ -> raise @@ Error (program.loc.(ip),
                "not enough data on stack")
        and cmp op = function
            | a :: b :: rest -> Bool (op a b) :: rest
            | stack -> raise @@ Error (program.loc.(ip),
                sprintf "not enough data on stack : %s" @@ show_stack stack)
        and float_op op = function
            | Float i :: Float j :: rest -> Float (op i j) :: rest
            | a :: b :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected float float, got %s %s" (show_data a) (show_data b))
            | _ -> raise @@ Error (program.loc.(ip),
                "not enough data on stack")
        and bool_op op = function
            | Bool i :: Bool j :: rest -> Bool (op i j) :: rest
            | a :: b :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected Bool Bool, got %s %s" (show_data a) (show_data b))
            | _ -> raise @@ Error (program.loc.(ip),
                "not enough data on stack")
        and put = function
            | Int  i :: rest when instr = PUTI -> print_int i; rest
            | Char c :: rest when instr = PUTC -> print_char c; rest
            | Ptr ("strs", offset) :: Int len :: rest when instr = PUTS ->
                    print_string
                    @@ String.sub program.strings offset len;
                    rest
            | Ptr (space, offset) :: Int len :: rest when instr = PUTS ->
                    let str =
                        Hashtbl.find mem space
                        |> Array.to_seq
                        |> Seq.map (function Char c -> c | d -> raise @@ Unreachable (show_data d))
                        |> String.of_seq
                    in
                    print_string
                    @@ String.sub str offset len;
                    rest
            | hd :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "invalid data for %s: %s" (show_ir instr) (show_data hd))
            | [] -> raise @@ Error (program.loc.(ip),
                sprintf "%s: not enough data on stack" (show_ir instr))
        and cond_jmp t f = function
            | Bool true :: tl -> t, tl
            | Bool false :: tl -> f, tl
            | hd :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected bool, got %s" (show_data hd))
            | [] -> raise @@ Error (program.loc.(ip),
                "not enough data on stack")
        in

        match instr with
        | FN _ | FN_END | IF _ | WHILE _ | END_IF _ ->
                raise @@ Unreachable (sprintf "%s: please run postprocess" (show_ir instr))

        | THEN addr | DO addr -> cond_jmp (ip + 1) addr stack
        | ELSE addr | END_WHILE addr -> addr, stack

        | PEEK (depth, addr) ->
                let data =
                    try List.nth stack depth
                    with _ -> raise @@ Error (program.loc.(ip), sprintf "PEEK %d: stack underflow" depth)
                in
                takes.(addr) <- data; ip + 1, stack
        | TAKE addr ->
                let data, stack =
                    match stack with
                    | hd :: tl -> hd, tl
                    | _ -> raise @@ Error (program.loc.(ip), "TAKE: stack underflow")
                in
                takes.(addr) <- data; ip + 1, stack
        | PUT addr -> ip + 1, takes.(addr) :: stack

        | LOAD t ->
                let space, addr, stack =
                    match stack with
                    | Ptr (space, addr) :: tl -> space, addr, tl
                    | hd :: _ -> raise @@ Error (program.loc.(ip),
                            sprintf "expected Ptr, got %s" (show_data hd))
                    | _ -> raise @@ Error (program.loc.(ip), "LOAD: stack underflow")
                in
                let data = (Hashtbl.find mem space).(addr) in

                if typ_of_data data = t then ip + 1, data :: stack
                else raise @@ Error (program.loc.(ip),
                        sprintf "expected %s, got %s" (show_typ t) (show_data data))
        | STORE t ->
                let space, addr, data, stack =
                    match stack with
                    | Ptr (space, addr) :: data :: tl -> space, addr, data, tl
                    | hd :: _ -> raise @@ Error (program.loc.(ip),
                            sprintf "expected Ptr, got %s" (show_data hd))
                    | _ -> raise @@ Error (program.loc.(ip), "stack underflow")
                in
                    (Hashtbl.find mem space).(addr) <- data;
                    if typ_of_data data = t then ip + 1, stack
                    else raise @@ Error (program.loc.(ip),
                            sprintf "expected %s, got %s" (show_typ t) (show_data data))


        | PUSH data -> ip + 1, data :: stack

        | EQ -> ip + 1, cmp ( =  ) stack
        | NE -> ip + 1, cmp ( <> ) stack
        | LT -> ip + 1, cmp ( <  ) stack
        | LE -> ip + 1, cmp ( <= ) stack
        | GT -> ip + 1, cmp ( >  ) stack
        | GE -> ip + 1, cmp ( >= ) stack

        | ADD -> ip + 1, int_op ( + ) stack
        | SUB -> ip + 1, int_op ( - ) stack
        | MUL -> ip + 1, int_op ( * ) stack
        | DIV -> ip + 1, int_op ( / ) stack
        | MOD -> ip + 1, int_op (mod) stack

        | FADD -> ip + 1, float_op ( +. ) stack
        | FSUB -> ip + 1, float_op ( -. ) stack
        | FMUL -> ip + 1, float_op ( *. ) stack
        | FDIV -> ip + 1, float_op ( /. ) stack

        | ITOF ->
                (match stack with
                | Int a :: rest -> ip + 1, Float (float_of_int a) :: rest
                | a :: _ -> raise @@ Error (program.loc.(ip),
                    sprintf "expected int, got %s" (show_data a))
                | _ -> raise @@ Error (program.loc.(ip), "not enough data on stack"))
        | FTOI ->
                (match stack with
                | Float a :: rest -> ip + 1, Int (int_of_float a) :: rest
                | a :: _ -> raise @@ Error (program.loc.(ip),
                    sprintf "expected float, got %s" (show_data a))
                | _ -> raise @@ Error (program.loc.(ip), "not enough data on stack"))

        | AND -> ip + 1, bool_op ( && ) stack
        | OR  -> ip + 1, bool_op ( || ) stack

        | LAND -> ip + 1, int_op ( land ) stack
        | LOR  -> ip + 1, int_op ( lor  ) stack
        | LXOR -> ip + 1, int_op ( lxor ) stack
        | LSL  -> ip + 1, int_op ( lsl  ) stack
        | LSR  -> ip + 1, int_op ( lsr  ) stack

        | SYSCALL n ->
                raise @@ Error (program.loc.(ip),
                    sprintf "syscall %d not implemented" n)

        | PUTC | PUTS | PUTI -> ip + 1, put stack
    in

    let rec exec'' ip stack =
        if ip >= Array.length program.ir then stack
        else
            let ip, stack = exec' stack ip program.ir.(ip) in
            exec'' ip stack
    in
    let stack =
        try exec'' 0 []
        with Error (loc, msg) -> raise @@ Error (loc, "runtime exception: " ^ msg)
    in

    match stack with
    | [] -> ()
    | stack ->
            List.iter (fun d -> print_endline @@ show_data d) stack;
            let typ_stack = List.map typ_of_data stack in
            raise @@ Error (program.loc.(Array.length program.loc - 1),
                sprintf "%s left on the stack at the end of program"
                (print_typ_stack typ_stack))
