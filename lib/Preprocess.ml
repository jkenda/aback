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

    | Macro | Proc | Is | End_func
    | If of int | Then of int | Else of int | End_if of int
    | While of int | Do of int | End_while of int
    | Peek | Take | In | End_peek
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

let _print_prep prep =
    List.iter (fun x -> print_endline (show_prep x)) prep;
    print_newline ()

let preprocess tokens =
    let preprocess (acc, end_stack, next_id) word =
        match word with
        | (Rev : word) -> Rev :: acc, end_stack, next_id

        | Macro -> Macro :: acc, Macro :: end_stack, next_id
        | Proc -> Proc :: acc, Proc :: end_stack, next_id
        | Is -> Is :: acc, end_stack, next_id

        | If ->
                let next_id = next_id + 1 in
                let next = If next_id in
                next :: acc, next :: end_stack, next_id
        | Then -> Then next_id :: acc, end_stack, next_id
        | Else -> Else next_id :: acc, end_stack, next_id

        | While ->
                let next_id = next_id + 1 in
                let next = While next_id in
                next :: acc, next :: end_stack, next_id
        | Do -> Do next_id :: acc, end_stack, next_id

        | Peek -> Peek :: acc, Peek :: end_stack, next_id
        | Take -> Take :: acc, Take :: end_stack, next_id
        | In -> In :: acc, end_stack, next_id

        | End ->
                ((match List.hd end_stack with
                | Proc | Macro -> End_func
                | If id -> End_if id
                | While id -> End_while id
                | Peek
                | Take -> End_peek
                | _ | exception _ -> raise @@ Failure "end reqires matching macro | func | if | while | peek | take")
                    :: acc, List.tl end_stack, next_id)

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

