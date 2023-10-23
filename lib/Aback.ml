type t =
    | S32 | U32
    | S64 | U64
    | F32 | F64
[@@deriving show { with_path = false }]

type operation =
    | Int of int
    | Float of float
    | Char of int
    | Str of string
    | CStr of string

    | Type of t

    | Rev

    | Macro | Func | Is
    | If | Then | Else | End
    | While | Do
    | Peek | Take | In
    | Let | Assign | Return

    | Eq | NEq | Lt | LEq | Gt | GEq

    | Add | FAdd
    | Sub | FSub
    | Mul | FMul
    | Div | FDiv
    | Mod | FMod

    | BAnd | BOr | BXor | Lsl | Lsr
    | And  | Or

    | Putc | Puts | Puti

    | Word of string
    | Unknown
[@@deriving show { with_path = false }]

type operations = operation list [@@deriving show { with_path = false }]

let instr_of_word = function
    | "|>" -> Rev | "->" -> Return
    | "macro" -> Macro | "func" -> Func | "is" -> Is
    | "if" -> If | "then" -> Then | "else" -> Else | "end" -> End
    | "while" -> While | "do" -> Do
    | "peek" -> Peek | "take" -> Take | "in" -> In
    | "let" -> Let | ":=" -> Assign

    | "s32" -> Type S32 | "s64" -> Type S64
    | "u32" -> Type U32 | "u64" -> Type U64
    | "f32" -> Type F32 | "f64" -> Type F64

    | "=" -> Eq | "/=" -> NEq
    | "<" -> Lt | "<=" -> LEq
    | ">" -> Gt | ">=" -> GEq

    | "+" -> Add | "+." -> FAdd
    | "-" -> Sub | "-." -> FSub
    | "*" -> Mul | "*." -> FMul
    | "/" -> Div | "/." -> FDiv
    | "%" -> Mod | "%." -> FMod

    | "&" -> BAnd | "|" -> BOr | "^" -> BXor
    | "<<" -> Lsl | ">>" -> Lsr
    | "&&" -> And | "||" -> Or

    | "putc" -> Putc
    | "puts" -> Puts

    | word ->
            if String.ends_with ~suffix:{|"|} word then
                if String.starts_with ~prefix:{|"|} word then
                    Str (String.sub word 1 (String.length word - 2))
                else if String.starts_with ~prefix:"c\"" word then
                    CStr (String.sub word 2 (String.length word - 3))
                else
                    Unknown
            else
                if String.length word = 3
                && String.starts_with ~prefix:"'" word
                && String.ends_with   ~suffix:"'" word then
                Char (Char.code (String.get word 1))
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
        print_endline (Format.asprintf "%s\n!=\n%s" (show_operations actual) (show_operations expected));
    matches

let%test _ = test (lex "+ 12 13 'c' 'cc'") ([Add; Int 12; Int 13; Char (Char.code 'c'); Unknown])
