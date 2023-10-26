open Lexer

type data =
    | Int of int
    | Float of float
    | Char of char
    | String of string
    | CStr of string
[@@deriving show { with_path = false }]

type prep =
    | Push of data
    | Type of t

    | Rev

    | Macro | Func | Is | End_func
    | If of int | Then of int | Else of int | End_if of int
    | While of int | Do of int | End_while of int
    | Peek of int | Take of int | In | End_peek of int | End_take of int
    | Let | Assign | Return

    | Eq | NEq | Lt | LEq | Gt | GEq

    | Add | FAdd
    | Sub | FSub
    | Mul | FMul
    | Div | FDiv
    | Mod | FMod

    | BAnd | BOr | BXor | Lsl | Lsr
    | And  | Or
    | Ref | Deref

    | Putc | Puts | Puti | Putf | Putb

    | Word of string
[@@deriving show { with_path = false }]

let preprocess tokens =
    let preprocess (acc, end_stack, next_id) word =
        match word with
        | (Rev : word) -> Rev :: acc, end_stack, next_id

        | Macro -> Macro :: acc, Macro :: end_stack, next_id
        | Func -> Func :: acc, Func :: end_stack, next_id
        | Is -> Is :: acc, end_stack, next_id

        | If ->
                let next = If (next_id + 1) in
                next :: acc, next :: end_stack, next_id + 1
        | Then -> Then next_id :: acc, end_stack, next_id
        | Else -> Else next_id :: acc, end_stack, next_id

        | While ->
                let next = While (next_id + 1) in
                next :: acc, next :: end_stack, next_id + 1
        | Do -> Do next_id :: acc, end_stack, next_id

        | Peek ->
                let next = Peek (next_id + 1) in
                next :: acc, next :: end_stack, next_id + 1
        | Take ->
                let next = Take (next_id + 1) in
                next :: acc, next :: end_stack, next_id + 1
        | In -> In :: acc, end_stack, next_id

        | End ->
                (match List.hd end_stack with
                | Func | Macro -> End_func :: acc, List.tl end_stack, next_id
                | If id -> End_if id :: acc, end_stack, next_id
                | While id -> End_while id :: acc, end_stack, next_id
                | Peek id -> End_peek id :: acc, end_stack, next_id
                | Take id -> End_take id :: acc, end_stack, next_id
                | _ | exception _ -> raise @@ Failure "end reqires matching macro | func | if | while | peek | take")

        | word ->
                (match word with
                | Int i -> Push (Int i)
                | Float f -> Push (Float f)
                | Char c -> Push (Char c)
                | String s -> Push (String s)
                | CStr s -> Push (CStr s)

                | Type t -> Type t

                | Let -> Let | Assign -> Assign | Return -> Return
                | Eq -> Eq | NEq -> NEq | Lt -> Lt | LEq -> LEq | Gt -> Gt | GEq -> GEq
                | Add -> Add | FAdd -> FAdd
                | Sub -> Sub | FSub -> FSub
                | Mul -> Mul | FMul -> FMul
                | Div -> Div | FDiv -> FDiv
                | Mod -> Mod | FMod -> FMod

                | BAnd -> BAnd | BOr -> BOr | BXor -> BXor | Lsl -> Lsl | Lsr -> Lsr
                | And -> And | Or -> Or
                | Ref -> Ref | Deref -> Deref

                | Putc -> Putc | Puts -> Puts | Puti -> Puti | Putf -> Putf | Putb -> Putb

                | Word w -> Word w
                | _ -> raise @@ Unreachable (show_word word)) :: acc, end_stack, next_id


    in
    let acc, _, _ = List.fold_left preprocess ([], [], 0) tokens in
    List.rev acc

