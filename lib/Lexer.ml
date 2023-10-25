type t =
    | Int
    | Float
    | Char
    | Ptr
[@@deriving show { with_path = false }]

type word =
    | Int of int
    | Float of float
    | Char of char
    | String of string
    | CStr of string

    | Type of t

    | Rev

    | Macro | Func | Is | End
    | If | Then | Else
    | While | Do (* while ... end *)
    | Peek | Take | In (* peek ... end, take ... end *)
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

    | Putc | Puts | Puti | Putf

    | Word of string
[@@deriving show { with_path = false }]

type words = word list [@@deriving show { with_path = false }]

let instr_of_word = function
    | "|>" -> Rev | "->" -> Return
    | "macro" -> Macro | "func" -> Func | "is" -> Is
    | "if" -> If | "then" -> Then | "else" -> Else | "end" -> End
    | "while" -> While | "do" -> Do
    | "peek" -> Peek | "take" -> Take | "in" -> In
    | "let" -> Let | ":=" -> Assign

    | "int" -> Type Int | "float" -> Type Float | "char" -> Type Char | "ptr" -> Type Ptr

    | "=" -> Eq | "/=" -> NEq
    | "<" -> Lt | "<=" -> LEq
    | ">" -> Gt | ">=" -> GEq

    | "+" -> Add | "+." -> FAdd
    | "-" -> Sub | "-." -> FSub
    | "*" -> Mul | "*." -> FMul
    | "/" -> Div | "/." -> FDiv
    | "%" -> Mod | "%." -> FMod

    | "&"  -> BAnd | "|"  -> BOr | "^" -> BXor
    | "<<" -> Lsl  | ">>" -> Lsr
    | "&&" -> And  | "||" -> Or
    | "@"  -> Ref  | "."  -> Deref

    | "putc" -> Putc
    | "puts" -> Puts
    | "puti" -> Puti
    | "putf" -> Putf

    | word ->
            if String.ends_with ~suffix:{|"|} word then
                if String.starts_with ~prefix:{|"|} word then
                    String (String.sub word 1 (String.length word - 2))
                else if String.starts_with ~prefix:"c\"" word then
                    CStr (String.sub word 2 (String.length word - 3))
                else
                    Word word
            else
                if String.length word = 3
                && String.starts_with ~prefix:"'" word
                && String.ends_with   ~suffix:"'" word then
                Char (String.get word 1)
            else
                match int_of_string_opt word with
                | Some i -> Int i
                | None ->
                        match float_of_string_opt word with
                        | Some f -> Float f
                        | None -> Word word

let lex text =
    let rec skip_whitespace i =
        if i >= String.length text then i
        else
            match text.[i] with
            | ' ' | '\n' | '\r' | '\t' -> skip_whitespace (i + 1)
            | _ -> i
    and get_string i =
        if i >= String.length text then i
        else
            match text.[i] with
            | '"' -> i
            | _ -> get_string (i + 1)
    and get_char i =
        if i >= String.length text then i
        else
            match text.[i] with
            | '\'' -> i
            | _ -> get_char (i + 1)
    and get_word i =
        if i >= String.length text then i
        else
            match text.[i] with
            | ' ' | '\n' | '\r' | '\t' -> i
            | _ -> get_word (i + 1)
    in
    let rec lex' (acc, i) =
        if i >= String.length text then acc
        else
            match text.[i] with
            | ' ' | '\n' | '\r' | '\t' ->
                    lex' (acc, skip_whitespace i)
            | '"' ->
                    let next = get_string (i + 1) in
                    lex' (String.sub text i (next - i + 1) :: acc, next + 1)
            | '\'' ->
                    let next = get_char (i + 1) in
                    lex' (String.sub text i (next - i + 1) :: acc, next + 1)
            | _ ->
                    let next = get_word i in
                    lex' (String.sub text i (next - i) :: acc, next)
    in
    lex' ([], 0)
    |> List.rev_map instr_of_word

let test actual expected =
    let matches = actual = expected in
    if matches then
        print_endline "OK"
    else
        print_endline (Format.asprintf "%s\n!=\n%s" (show_words actual) (show_words expected));
    matches

let%test _ = test (lex "+ 12 13 'c' 'cc'") ([Add; Int 12; Int 13; Char 'c'; Word "cc"])
