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

    | Rev | Return

    | Macro | Proc | Is
    | If | Then | Else | End_if
    | While | Do | End_while
    | Peek | Take | In | End_peek
    | Mem | Assign
    | End

    | Eq | NEq | Lt | LEq | Gt | GEq

    | Add | FAdd
    | Sub | FSub
    | Mul | FMul
    | Div | FDiv
    | Mod

    | Itof | Ftoi

    | BAnd | BOr | BXor | Lsl | Lsr
    | And  | Or
    | Ref | Deref

    | Putc | Puts | Puti

    | Syscall

    | Word of string
[@@deriving show { with_path = false }]

let print_prep = function
    | Push a -> show_data a
    | Type t -> print_typ t

    | Rev -> "|>" | Return -> "->"

    | Macro -> "macro" | Proc -> "proc" | Is -> "is" | End -> "end"
    | If -> "if" | Then -> "then" | Else -> "else" | End_if -> "end"
    | While -> "while" | Do -> "do" | End_while -> "end"
    | Peek -> "peek" | Take -> "take" | In -> "in" | End_peek -> "end"
    | Mem -> "mem" | Assign -> ":="

    | Eq -> "=" | NEq -> "!=" | Lt -> "<" | LEq -> "<=" | Gt -> ">" | GEq -> ">="

    | Add -> "+" | FAdd -> "+."
    | Sub -> "-" | FSub -> "-."
    | Mul -> "*" | FMul -> "*."
    | Div -> "/" | FDiv -> "/."
    | Mod -> "%"

    | Itof -> "itof" | Ftoi -> "ftoi"

    | BAnd -> "&" | BOr -> "|" | BXor -> "^" | Lsl -> "<<" | Lsr -> ">>"
    | And  -> "&&" | Or -> "||"
    | Ref -> "@" | Deref -> "."

    | Putc -> "putc" | Puts -> "puts" | Puti -> "puti"

    | Syscall -> "syscall"

    | Word w -> w

let print_prep_stack =
    List.fold_left (fun acc typ -> acc ^ print_prep typ ^ " ") ""

let rec include_file included_from src =
    let text = read_lib_file included_from src in
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

    let end_stack = Stack.create () in
    let push_end data = Stack.push data end_stack in

    let preprocess'' (acc, words) =
        match words with
        | [] -> acc, []
        | (loc, Include) :: (_, String src) :: tl ->
                let included_from = loc.filename :: loc.included_from in
                include_file included_from src @ acc, tl
        | (_, Include) :: (loc, _) :: _
        | (loc, Include) :: _ -> raise @@ Error (loc, "expected string after include")

        | (loc, Macro) :: tl -> push_end (loc, Macro); (loc, Macro) :: acc, tl
        | (loc, Proc)  :: tl -> push_end (loc, Proc) ; (loc, Proc)  :: acc, tl
        | (loc, Is)    :: tl -> (loc, Is) :: acc, tl

        | (loc, Mem) :: tl -> push_end (loc, Mem); (loc, Mem) :: acc, tl

        | (loc, If)   :: tl -> push_end (loc, If); (loc, If) :: acc, tl
        | (loc, Then) :: tl -> (loc, Then) :: acc, tl
        | (loc, Else) :: tl -> (loc, Else) :: acc, tl
        (* TODO: Else :: If -> Elif *)

        | (loc, While) :: tl -> push_end (loc, While); (loc, While) :: acc, tl
        | (loc, Do)    :: tl -> (loc, Do) :: acc, tl

        | (loc, Peek) :: tl -> push_end (loc, Peek); (loc, Peek) :: acc, tl
        | (loc, Take) :: tl -> push_end (loc, Take); (loc, Take) :: acc, tl
        | (loc, In)   :: tl -> (loc, In) :: acc, tl

        | (loc, End) :: tl ->
                let ir =
                    match Stack.pop end_stack with
                    | (_, Proc)
                    | (_, Macro)
                    | (_, Mem)   -> End
                    | (_, If)    -> End_if
                    | (_, While) -> End_while
                    | (_, Peek)
                    | (_, Take) -> End_peek
                    | _ | exception _ ->
                            raise @@ Error (loc,
                            "end reqires matching begin: one of macro, func, if, while, peek, take")
                in
                (loc, ir) :: acc, tl

        | ((_, Word w) :: _) as words when String.starts_with ~prefix:"(" w ->
                acc, remove_comment words

        | (loc, word) :: tl ->
                (loc, match word with
                | Int i -> Push (Int i)
                | Float f -> Push (Float f)
                | Char c -> Push (Char c)
                | String s -> Push (String s)
                | CStr s -> Push (CStr s)
                | True -> Push (Bool true)
                | False -> Push (Bool false)

                | Rev -> Rev | Mem -> Mem

                | Type t -> Type t

                | Assign -> Assign | Return -> Return
                | Eq -> Eq | NEq -> NEq | Lt -> Lt | LEq -> LEq | Gt -> Gt | GEq -> GEq
                | Add -> Add | FAdd -> FAdd
                | Sub -> Sub | FSub -> FSub
                | Mul -> Mul | FMul -> FMul
                | Div -> Div | FDiv -> FDiv
                | Mod -> Mod

                | Itof -> Itof | Ftoi -> Ftoi

                | BAnd -> BAnd | BOr -> BOr | BXor -> BXor | Lsl -> Lsl | Lsr -> Lsr
                | And -> And | Or -> Or
                | Ref -> Ref | Deref -> Deref

                | Putc -> Putc | Puts -> Puts | Puti -> Puti

                | Syscall -> Syscall

                | Word w -> Word w
                | _ -> raise @@ Not_implemented (loc, show_word word)) :: acc, tl


    in
    let rec preprocess' ((acc, words) as data) =
        match words with
        | [] -> List.rev acc, end_stack
        | _ -> preprocess' @@ preprocess'' data
    in
    let acc, end_stack = preprocess' ([], words) in
    if Stack.is_empty end_stack then acc
    else
        let loc, _ = Stack.pop end_stack in
        raise @@ Error (loc, "no matching 'end'")
