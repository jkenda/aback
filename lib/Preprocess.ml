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
    | Literal of data
    | Type of typ

    | Sep | Return

    | Macro | Proc | Is
    | If | Then | Else
    | While | Do
    | Peek | Take
    | Mem | Var
    | End

    | Index | Assign

    | Dot_dot_dot

    | Syscall

    | Op of operator

    | Word of string
[@@deriving show { with_path = false }]

let print_prep = function
    | Literal a -> show_data a
    | Type t -> print_typ t

    | Sep -> ";" | Return -> "->"

    | Macro -> "macro" | Proc -> "proc" | Is -> "is"
    | If -> "if" | Then -> "then" | Else -> "else"
    | While -> "while" | Do -> "do"
    | Peek -> "peek" | Take -> "take"
    | Mem -> "mem" | Var -> "var"
    | End -> "end"

    | Index -> "[]" | Assign -> ":="

    | Dot_dot_dot -> "..."

    | Syscall -> "syscall"

    | Op Eq -> "=" | Op NEq -> "!=" | Op Lt -> "<" | Op LEq -> "<=" | Op Gt -> ">" | Op GEq -> ">="

    | Op Add -> "+" | Op FAdd -> "+."
    | Op Sub -> "-" | Op FSub -> "-."
    | Op Mul -> "*" | Op FMul -> "*."
    | Op Div -> "/" | Op FDiv -> "/."
    | Op Mod -> "%"

    | Op Itof -> "itof" | Op Ftoi -> "ftoi"

    | Op LAnd -> "&"  | Op LOr -> "|" | Op LXor -> "^" | Op Lsl -> "<<" | Op Lsr -> ">>"
    | Op And  -> "&&" | Op Or -> "||"
    | Op Ref -> "@"   | Op Deref -> "."

    | Op Putc -> "putc" | Op Puts -> "puts"

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

    let preprocess'' (acc, words) =
        match words with
        | [] -> acc, []

        | (loc, Include) :: (_, String src) :: tl ->
                let included_from = loc.filename :: loc.included_from in
                include_file included_from src @ acc, tl
        | (_, Include) :: (loc, _) :: _
        | (loc, Include) :: _ -> raise @@ Error (loc, "expected string after include")

        | ((_, Word w) :: _) as words when String.starts_with ~prefix:"(" w ->
                acc, remove_comment words

        | (loc, word) :: tl ->
                (loc, match word with
                | Int i     -> Literal (Int i)
                | Float f   -> Literal (Float f)
                | Char c    -> Literal (Char c)
                | String s  -> Literal (String s)
                | CStr s    -> Literal (CStr s)
                | True      -> Literal (Bool true)
                | False     -> Literal (Bool false)

                | Macro -> Macro | Proc -> Proc | Is -> Is
                | If -> If | Then -> Then | Else -> Else
                | While -> While | Do -> Do
                | Peek -> Peek | Take -> Take
                | Mem -> Mem | Var -> Var
                | End

                | Sep -> Sep

                | Type t -> Type t

                | Dot_dot_dot -> Dot_dot_dot

                | Assign -> Assign | Index -> Index | Return -> Return

                | Op op -> Op op

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
