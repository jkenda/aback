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
    | Type of typ

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

let rec include_file included_from src =
    let text = read_lib_file src in
    text
    |> lex src included_from
    |> preprocess
    |> List.rev

and preprocess words =
    let rec remove_comment = function
        | (_, (Word w : word)) :: tl when String.ends_with ~suffix:")" w -> tl
        | _ :: tl -> remove_comment tl
        | [] -> []
    in

    let preprocess'' (acc, end_stack, next_id, words) =
        match words with
        | [] -> acc, end_stack, next_id, []
        | (loc, Include) :: (_, String src) :: tl ->
                let included_from = loc.filename :: loc.included_from in
                include_file included_from src @ acc, end_stack, next_id, tl
        | (_, Include) :: (loc, _) :: _
        | (loc, Include) :: _ -> raise @@ Error (loc, "expected string after include")

        | (loc, Rev) :: tl -> (loc, Rev) :: acc, end_stack, next_id, tl

        | (loc, Macro) :: tl ->
                let next_id = next_id + 1 in
                let next = loc, Macro next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (loc, Proc) :: tl ->
                let next_id = next_id + 1 in
                let next = loc, Proc next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (loc, Is) :: tl -> (loc, Is) :: acc, end_stack, next_id, tl

        | (loc, If) :: tl ->
                let next_id = next_id + 1 in
                let next = loc, If next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (loc, Then) :: tl -> (loc, Then next_id) :: acc, end_stack, next_id, tl
        | (loc, Else) :: tl -> (loc, Else next_id) :: acc, end_stack, next_id, tl

        | (loc, While) :: tl ->
                let next_id = next_id + 1 in
                let next = loc, While next_id in
                next :: acc, next :: end_stack, next_id, tl
        | (loc, Do) :: tl -> (loc, Do next_id) :: acc, end_stack, next_id, tl

        | (loc, Peek) :: tl -> (loc, Peek) :: acc, (loc, Peek) :: end_stack, next_id, tl
        | (loc, Take) :: tl -> (loc, Take) :: acc, (loc, Take) :: end_stack, next_id, tl
        | (loc, In) :: tl -> (loc, In) :: acc, end_stack, next_id, tl

        | (loc, End) :: tl ->
                let ir, end_stack =
                    match end_stack with
                    | (_, Proc id)  :: e
                    | (_, Macro id) :: e -> End_func id, e
                    | (_, If id)    :: e -> End_if id, e
                    | (_, While id) :: e -> End_while id, e
                    | (_, Peek)     :: e
                    | (_, Take)     :: e -> End_peek, e
                    | _ | exception _ ->
                            raise @@ Error (loc,
                            "end reqires matching begin: one of macro, func, if, while, peek, take")
                in
                (loc, ir) :: acc, end_stack, next_id, tl

        | ((_, Word w) :: _) as words when String.starts_with ~prefix:"(" w ->
                acc, end_stack, next_id, remove_comment words

        | (loc, word) :: tl ->
                (loc, match word with
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
                | _ -> raise @@ Not_implemented (loc, show_word word)) :: acc, end_stack, next_id, tl


    in
    let rec preprocess' ((acc, end_stack, _, words) as data) =
        match words with
        | [] -> List.rev acc, end_stack
        | _ -> preprocess' @@ preprocess'' data
    in
    let acc, end_stack = preprocess' ([], [], 0, words) in
    if end_stack = [] then acc
    else
        let loc, _ = List.hd end_stack in
        raise @@ Error (loc, "no matching 'end'")


