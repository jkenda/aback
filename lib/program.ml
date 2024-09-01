open Format
open Common

type ir =
    | PUSH of data_ll

    | EQ | NE | LT | LE | GT | GE

    | ADD | FADD
    | SUB | FSUB
    | MUL | FMUL
    | DIV | FDIV
    | MOD

    | ITOF32 | ITOF64
    | FTOI32 | FTOI64

    | LAND | LOR | LXOR | LSL | LSR
    | AND  | OR

    | PUTC | PUTS

    | SYSCALL of int

    | IF of int | THEN of int | ELSE of int | END_IF of int
    | WHILE of int | DO of int | END_WHILE of int
    | PEEK of int * int | TAKE of int
    | PUT of int
    | LOAD of type_ll | STORE of type_ll
    | FN of string | FN_END
[@@deriving show { with_path = false }]

type program = {
    ir : ir array;
    loc : location array;
    strings : string;
    vars : (string, type_ll) Hashtbl.t;
    mem : (string, type_ll * int) Hashtbl.t;
    storage_size : int
}

type stack = data_ll list
[@@deriving show { with_path = false }]

let (type_null : type_ll -> data_ll) = function
    | I8 -> I8 0 | I16 -> I16 0 | I32 -> I32 0 | I64 -> I64 0
    | U8 -> U8 0 | U16 -> U16 0 | U32 -> U32 0 | U64 -> U64 0
    | F32 -> F32 0.0 | F64 -> F64 0.0
    | Char -> Char '\000' | Bool -> Bool false
    | Ptr _ -> Ptr (U64, "", 0)

let interpret program =
    let takes = Array.make program.storage_size (I64 0)
    and mem = Hashtbl.create (Hashtbl.length program.mem) in

    Hashtbl.iter (fun space (type_ll, size) ->
        Hashtbl.add mem ("mem_" ^ space) (Array.make size (type_null type_ll)))
    program.mem;
    Hashtbl.iter (fun space type_ll ->
        Hashtbl.add mem ("var_" ^ space) (Array.make 1 (type_null type_ll)))
    program.vars;

    let exec' stack ip instr =
        let int_op op = function
            | I8  a :: I8  b :: rest -> I8  (op a b) :: rest
            | I16 a :: I16 b :: rest -> I16 (op a b) :: rest
            | I32 a :: I32 b :: rest -> I32 (op a b) :: rest
            | I64 a :: I64 b :: rest -> I64 (op a b) :: rest

            | U8  a :: U8  b :: rest -> U8  (op a b) :: rest
            | U16 a :: U16 b :: rest -> U16 (op a b) :: rest
            | U32 a :: U32 b :: rest -> U32 (op a b) :: rest
            | U64 a :: U64 b :: rest -> U64 (op a b) :: rest

            | Ptr (t, s, a) :: U64 b :: rest ->
                Ptr (t, s, op a b) :: rest
            | Ptr (_, s, a) :: Ptr (_, s', b) :: rest when s = s' ->
                U64 (op a b) :: rest
            | a :: b :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected int int, got %s %s" (string_of_data_ll a) (string_of_data_ll b))
            | _ -> raise @@ Error (program.loc.(ip),
                "not enough data on stack")
        and cmp op = function
            | a :: b :: rest -> Bool (op a b) :: rest
            | stack -> raise @@ Error (program.loc.(ip),
                sprintf "not enough data on stack : %s" @@ show_stack stack)
        and float_op op = function
            | F32 i :: F32 j :: rest -> F32 (op i j) :: rest
            | F64 i :: F64 j :: rest -> F64 (op i j) :: rest
            | a :: b :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected float float, got %s %s" (string_of_data_ll a) (string_of_data_ll b))
            | _ -> raise @@ Error (program.loc.(ip),
                "not enough data on stack")
        and bool_op op = function
            | Bool i :: Bool j :: rest -> Bool (op i j) :: rest
            | a :: b :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected Bool Bool, got %s %s" (string_of_data_ll a) (string_of_data_ll b))
            | _ -> raise @@ Error (program.loc.(ip),
                "not enough data on stack")
        and put = function
            | U8 c :: rest when instr = PUTC -> print_char (char_of_int c); rest
            | Ptr (U8, "strs", offset) :: U64 len :: rest when instr = PUTS ->
                    print_string
                    @@ String.sub program.strings offset len;
                    rest
            | Ptr (U8, space, offset) :: U64 len :: rest when instr = PUTS ->
                    let str =
                        Hashtbl.find mem space
                        |> Array.to_seq
                        |> Seq.map (function U8 c -> Char.chr c | d -> raise @@ Unreachable (string_of_data_ll d))
                        |> String.of_seq
                    in
                    print_string
                    @@ String.sub str offset len;
                    rest
            | hd :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "invalid data for %s: %s" (show_ir instr) (string_of_data_ll hd))
            | [] -> raise @@ Error (program.loc.(ip),
                sprintf "%s: not enough data on stack" (show_ir instr))
        and cond_jmp t f = function
            | Bool true :: tl -> t, tl
            | Bool false :: tl -> f, tl
            | hd :: _ -> raise @@ Error (program.loc.(ip),
                sprintf "expected bool, got %s" (string_of_data_ll hd))
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
                    | Ptr (_, space, addr) :: tl -> space, addr, tl
                    | hd :: _ -> raise @@ Error (program.loc.(ip),
                            sprintf "expected Ptr, got %s" (string_of_data_ll hd))
                    | _ -> raise @@ Error (program.loc.(ip), "LOAD: stack underflow")
                in
                let data = (Hashtbl.find mem space).(addr) in

                if type_of_data_ll data = t then ip + 1, data :: stack
                else raise @@ Error (program.loc.(ip),
                        sprintf "expected %s, got %s" (string_of_type_ll t) (string_of_data_ll data))
        | STORE t ->
                let space, addr, data, stack =
                    match stack with
                    | Ptr (_, space, addr) :: data :: tl -> space, addr, data, tl
                    | hd :: _ -> raise @@ Error (program.loc.(ip),
                            sprintf "expected Ptr, got %s" (string_of_data_ll hd))
                    | _ -> raise @@ Error (program.loc.(ip), "stack underflow")
                in
                    (Hashtbl.find mem space).(addr) <- data;
                    if type_of_data_ll data = t then ip + 1, stack
                    else raise @@ Error (program.loc.(ip),
                            sprintf "expected %s, got %s" (string_of_type_ll t) (string_of_data_ll data))


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

        | ITOF32 | ITOF64 ->
                (match stack with
                | I32 a :: rest -> ip + 1, F32 (float_of_int a) :: rest
                | I64 a :: rest -> ip + 1, F64 (float_of_int a) :: rest
                | a :: _ -> raise @@ Error (program.loc.(ip),
                    sprintf "expected int, got %s" (show_data_ll a))
                | _ -> raise @@ Error (program.loc.(ip), "not enough data on stack"))
        | FTOI32 | FTOI64 ->
                (match stack with
                | F32 a :: rest -> ip + 1, I32 (int_of_float a) :: rest
                | F64 a :: rest -> ip + 1, I64 (int_of_float a) :: rest
                | a :: _ -> raise @@ Error (program.loc.(ip),
                    sprintf "expected float, got %s" (show_data_ll a))
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

        | PUTC | PUTS -> ip + 1, put stack
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
            List.iter (fun d -> print_endline @@ show_data_ll d) stack;
            let typ_stack = List.map type_of_data_ll stack in
            raise @@ Error (program.loc.(Array.length program.loc - 1),
                sprintf "%s left on the stack at the end of program"
                (string_of_types_ll typ_stack))
