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
    | Peek | Take | In
    | Let | Assign | Return

    | Eq | NEq | Lt | LEq | Gt | GEq

    | Add | FAdd
    | Sub | FSub
    | Mul | FMul
    | Div | FDiv
    | Mod | FMod

    | BAnd | BOr | BXor
    | And  | Or

    | Putc | Puts | Puti

    | Word of string
    | Unknown
[@@deriving show { with_path = false }]

type operations = operation list [@@deriving show { with_path = false }]

let instr_of_word = function
    | ";;" -> Rev | "->" -> Return
    | "macro" -> Macro | "func" -> Func | "is" -> Is
    | "if" -> If | "then" -> Then | "else" -> Else | "end" -> End
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
    | "&&" -> And | "||" -> Or

    | "putc" -> Putc
    | "puts" -> Puts

    | word ->
            if String.ends_with ~suffix:{|"|} word then
                if String.starts_with ~prefix:{|"|} word then
                    Str (String.sub word 1 (String.length word - 1))
                else if String.starts_with ~prefix:"c\"" word then
                    CStr word
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

let pp = show_operation

let lex text =
    text
    |> Str.split @@ Str.regexp "[ \n\r\x0c\t]+"
    |> List.map instr_of_word

let test actual expected =
    let matches = actual = expected in
    if matches then
        print_endline "OK"
    else
        print_endline (Format.asprintf "%s\n!=\n%s" (show_operations actual) (show_operations expected));
    matches

let%test _ = test (lex "+ 12 13 'c' 'cc'") ([Add; Int 12; Int 13; Char (Char.code 'c'); Unknown])
