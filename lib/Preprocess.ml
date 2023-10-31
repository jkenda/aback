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

    | Macro | Proc | Is | End_func
    | If | Then | Else | End_if
    | While | Do | End_while
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

let print_prep = function
    | Push a -> show_data a
    | Type t -> print_typ t

    | Rev -> "|>"

    | Macro -> "macro" | Proc -> "proc" | Is -> "is" | End_func -> "end"
    | If -> "if" | Then -> "then" | Else -> "else" | End_if -> "end"
    | While -> "while" | Do -> "do" | End_while -> "end"
    | Peek -> "peek" | Take -> "take" | In -> "in" | End_peek -> "end"
    | Let -> "let" | Assign -> ":=" | Return -> "->"

    | Eq -> "=" | NEq -> "!=" | Lt -> "<" | LEq -> "<=" | Gt -> ">" | GEq -> ">="

    | Add -> "+" | FAdd -> "+."
    | Sub -> "-" | FSub -> "-."
    | Mul -> "*" | FMul -> "*."
    | Div -> "/" | FDiv -> "/."
    | Mod -> "%" | FMod -> "%."

    | BAnd -> "&" | BOr -> "|" | BXor -> "^" | Lsl -> "<<" | Lsr -> ">>"
    | And  -> "&&" | Or -> "||"
    | Ref -> "@" | Deref -> "."

    | Putc -> "putc" | Puts -> "puts" | Puti -> "puti" | Putf -> "putf" | Putb -> "putb"

    | Word w -> w

let print_prep_stack =
    List.fold_left (fun acc typ -> acc ^ print_prep typ ^ " ") ""

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

    let preprocess'' (acc, end_stack, words) =
        match words with
        | [] -> acc, end_stack, []
        | (loc, Include) :: (_, String src) :: tl ->
                let included_from = loc.filename :: loc.included_from in
                include_file included_from src @ acc, end_stack, tl
        | (_, Include) :: (loc, _) :: _
        | (loc, Include) :: _ -> raise @@ Error (loc, "expected string after include")

        | (loc, Rev) :: tl -> (loc, Rev) :: acc, end_stack, tl

        | (loc, Macro) :: tl -> (loc, Macro) :: acc, (loc, Macro) :: end_stack, tl
        | (loc, Proc)  :: tl -> (loc, Proc)  :: acc, (loc, Proc) :: end_stack, tl
        | (loc, Is)    :: tl -> (loc, Is)    :: acc, end_stack, tl

        | (loc, If)   :: tl -> (loc, If)   :: acc, (loc, If)   :: end_stack, tl
        | (loc, Then) :: tl -> (loc, Then) :: acc, end_stack, tl
        | (loc, Else) :: tl -> (loc, Else) :: acc, end_stack, tl
        (* TODO: Else :: If -> Elif *)

        | (loc, While) :: tl -> (loc, While) :: acc, (loc, While) :: end_stack, tl
        | (loc, Do)    :: tl -> (loc, Do)    :: acc, end_stack, tl

        | (loc, Peek) :: tl -> (loc, Peek) :: acc, (loc, Peek) :: end_stack, tl
        | (loc, Take) :: tl -> (loc, Take) :: acc, (loc, Take) :: end_stack, tl
        | (loc, In)   :: tl -> (loc, In) :: acc, end_stack, tl

        | (loc, End) :: tl ->
                let ir, end_stack =
                    match end_stack with
                    | (_, Proc)  :: e
                    | (_, Macro) :: e -> End_func, e
                    | (_, If)    :: e -> End_if, e
                    | (_, While) :: e -> End_while, e
                    | (_, Peek)     :: e
                    | (_, Take)     :: e -> End_peek, e
                    | _ | exception _ ->
                            raise @@ Error (loc,
                            "end reqires matching begin: one of macro, func, if, while, peek, take")
                in
                (loc, ir) :: acc, end_stack, tl

        | ((_, Word w) :: _) as words when String.starts_with ~prefix:"(" w ->
                acc, end_stack, remove_comment words

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
                | _ -> raise @@ Not_implemented (loc, show_word word)) :: acc, end_stack, tl


    in
    let rec preprocess' ((acc, end_stack, words) as data) =
        match words with
        | [] -> List.rev acc, end_stack
        | _ -> preprocess' @@ preprocess'' data
    in
    let acc, end_stack = preprocess' ([], [], words) in
    if end_stack = [] then acc
    else
        let loc, _ = List.hd end_stack in
        raise @@ Error (loc, "no matching 'end'")


