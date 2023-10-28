open Format
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

let rec include_file src =
    let text = read_lib_file src in
    try text
    |> lex
    |> preprocess
    |> List.rev
    with Failure msg ->
            print_endline msg;
            exit 1

and preprocess words =
    let preprocess'' (acc, end_stack, next_id, words) =
        match words with
        | [] -> acc, end_stack, next_id, []
        | Include :: String src :: tl -> include_file src @ acc, end_stack, next_id, tl
        | Include :: word :: _ -> raise @@ Failure (sprintf "expected string after include, got %s" (show_word word))
        | Include :: _ -> raise @@ Failure "expected string after include"

        | Rev :: tl -> Rev :: acc, end_stack, next_id, tl

        | Macro :: tl -> Macro :: acc, Macro :: end_stack, next_id, tl
        | Proc :: tl -> Proc :: acc, Proc :: end_stack, next_id   , tl
        | Is :: tl -> Is :: acc, end_stack, next_id               , tl

        | If :: tl ->
                let next_id = next_id + 1 in
                let next = If next_id in
                next :: acc, next :: end_stack, next_id, tl
        | Then :: tl -> Then next_id :: acc, end_stack, next_id, tl
        | Else :: tl -> Else next_id :: acc, end_stack, next_id, tl

        | While :: tl ->
                let next_id = next_id + 1 in
                let next = While next_id in
                next :: acc, next :: end_stack, next_id, tl
        | Do :: tl -> Do next_id :: acc, end_stack, next_id, tl

        | Peek :: tl -> Peek :: acc, Peek :: end_stack, next_id, tl
        | Take :: tl -> Take :: acc, Take :: end_stack, next_id, tl
        | In :: tl -> In :: acc, end_stack, next_id, tl

        | End :: tl ->
                ((match List.hd end_stack with
                | Proc | Macro -> End_func
                | If id -> End_if id
                | While id -> End_while id
                | Peek
                | Take -> End_peek
                | _ | exception _ -> raise @@ Failure "end reqires matching macro | func | if | while | peek | take")
                    :: acc, List.tl end_stack, next_id, tl)

        | word :: tl ->
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
                | _ -> raise @@ Not_implemented (show_word word)) :: acc, end_stack, next_id, tl


    in
    let rec preprocess' ((acc, _, _, words) as data) =
        match words with
        | [] -> List.rev acc
        | _ -> preprocess' @@ preprocess'' data
    in
    preprocess' ([], [], 0, words)

