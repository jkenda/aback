open Format
open Lexer

type data =
    | Int of int
    | Float of float
    | Char of char
    | Bool of bool
    | String of string
    | CStr of string
[@@deriving show { with_path = false }]

type prep =
    | Push of data
    | Type of t

    | Rev

    | Macro of int | Proc of int | Is | End_func of int
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
    text
    |> lex src
    |> preprocess
    |> List.rev

and preprocess words =
    let preprocess'' (acc, end_stack, next_id, words) =
        match words with
        | [] -> acc, end_stack, next_id, []
        | (_, Include) :: (_, String src) :: tl -> include_file src @ acc, end_stack, next_id, tl
        | (_, Include) :: (_, word) :: _ -> raise @@ Failure (sprintf "expected string after include, got %s" (show_word word))
        | (_, Include) :: _ -> raise @@ Failure "expected string after include"

        | (_, Rev) :: tl -> Rev :: acc, end_stack, next_id, tl

        | (_, Macro) :: tl ->
                let next_id = next_id + 1 in
                let next = Macro next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (_, Proc) :: tl ->
                let next_id = next_id + 1 in
                let next = Proc next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (_, Is) :: tl -> Is :: acc, end_stack, next_id, tl

        | (_, If) :: tl ->
                let next_id = next_id + 1 in
                let next = If next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (_, Then) :: tl -> Then next_id :: acc, end_stack, next_id, tl
        | (_, Else) :: tl -> Else next_id :: acc, end_stack, next_id, tl

        | (_, While) :: tl ->
                let next_id = next_id + 1 in
                let next = While next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (_, Do) :: tl -> Do next_id :: acc, end_stack, next_id, tl

        | (_, Peek) :: tl -> Peek :: acc, Peek :: end_stack, next_id, tl
        | (_, Take) :: tl -> Take :: acc, Take :: end_stack, next_id, tl
        | (_, In) :: tl -> In :: acc, end_stack, next_id, tl

        | (_, End) :: tl ->
                ((match List.hd end_stack with
                | Proc id | Macro id -> End_func id
                | If id -> End_if id
                | While id -> End_while id
                | Peek
                | Take -> End_peek
                | _ | exception _ -> raise @@ Failure "end reqires matching macro | func | if | while | peek | take")
                    :: acc, List.tl end_stack, next_id, tl)

        | (_, word) :: tl ->
                (match word with
                | Int i -> Push (Int i)
                | Float f -> Push (Float f)
                | Char c -> Push (Char c)
                | String s -> Push (String s)
                | CStr s -> Push (CStr s)
                | True -> Push (Bool true)
                | False -> Push (Bool false)

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

