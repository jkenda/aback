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
    | If | Then | Else | End_if
    | While | Do | End_while
    | Peek | Take | In | End_peek
    | Mem | Var | Index | Assign
    | End

    | Syscall

    | Op of operator

    | Word of string
[@@deriving show { with_path = false }]

let print_prep = function
    | Literal a -> show_data a
    | Type t -> print_typ t

    | Sep -> ";" | Return -> "->"

    | Macro -> "macro" | Proc -> "proc" | Is -> "is" | End -> "end"
    | If -> "if" | Then -> "then" | Else -> "else" | End_if -> "end"
    | While -> "while" | Do -> "do" | End_while -> "end"
    | Peek -> "peek" | Take -> "take" | In -> "in" | End_peek -> "end"
    | Mem -> "mem" | Var -> "var" | Index -> "[]" | Assign -> ":="

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

        | (loc, Var) :: tl -> push_end (loc, Var); (loc, Var) :: acc, tl
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
                    | (_, Var)
                    | (_, Mem)   -> End
                    | (_, If)    -> End_if
                    | (_, While) -> End_while
                    | (_, Peek)
                    | (_, Take) -> End_peek
                    | _ | exception _ ->
                            raise @@ Error (loc,
                            "end requires matching begin: one of macro, func, if, while, peek, take")
                in
                (loc, ir) :: acc, tl

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

                | Sep -> Sep | Var -> Var | Mem -> Mem

                | Type t -> Type t

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
