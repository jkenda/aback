open Format

type location = {
    filename : string;
    included_from : string list;
    expanded_from : location list;
    row : int;
    col : int;
}
[@@deriving show { with_path = false }]
let print_location loc = sprintf "'%s':%d:%d" loc.filename loc.row loc.col

exception Error of location * string
let print_error (loc, msg) =
    List.iter (fun loc      -> printf "expanded from %s\n" (print_location loc)) loc.expanded_from;
    printf "%s:\n" (print_location loc);
    printf "\t%s\n\n" msg;
    List.iter (fun filename -> printf "included from '%s'\n" filename) loc.included_from;

exception Not_implemented of location * string
exception Unreachable of string

let src_dirs = [
    "./";
    "~/.local/share/aback/"
]

let read_src_file filename =
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let read_lib_file filename =
    let rec open_file = function
        | [] -> raise @@ Failure (sprintf "cannot find file \"%s\"" filename)
        | dir :: rest -> 
                try
                    let f = open_in (dir ^ filename) in
                    let s = really_input_string f (in_channel_length f) in
                    close_in f; s
                with _ -> open_file rest
    in
    open_file src_dirs


type typ = Int | Float | Char | Bool | Ptr | String | CStr
[@@deriving show { with_path = false }]

type word =
    | Include

    | Int of int
    | Float of float
    | Char of char
    | String of string
    | CStr of string
    | True | False

    | Type of typ

    | Rev

    | Macro | Proc | Is | End
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

    | Putc | Puts | Puti | Putf | Putb

    | Word of string
[@@deriving show { with_path = false }]

type words = (location * word) list [@@deriving show { with_path = false }]

let instr_of_word (loc, word) =
    let word =
        match word with
        | "include" -> Include
        | "|>" -> Rev | "->" -> Return
        | "macro" -> Macro | "proc" -> Proc | "is" -> Is
        | "if" -> If | "then" -> Then | "else" -> Else | "end" -> End
        | "while" -> While | "do" -> Do
        | "peek" -> Peek | "take" -> Take | "in" -> In
        | "let" -> Let | ":=" -> Assign

        | "int" -> Type Int | "float" -> Type Float
        | "char" -> Type Char | "ptr" -> Type Ptr
        | "bool" -> Type Bool

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

        | "putc" -> Putc | "puts" -> Puts
        | "puti" -> Puti | "putf" -> Putf | "putb" -> Putb

        | "true" -> True | "false" -> False
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
                        Char word.[1]
                else
                    if String.length word = 4
                    && String.starts_with ~prefix:"'\\" word
                    && String.ends_with   ~suffix:"'" word then
                        Char (match word.[2] with
                        | 'n' -> '\n'
                        | 'r' -> '\r'
                        | 't' -> '\t'
                        | 'b' -> '\b'
                        | '\\' -> '\\'
                        | _ -> raise @@ Error (loc, "invalid escape character"))
                else
                    match int_of_string_opt word with
                    | Some i -> Int i
                    | None ->
                            match float_of_string_opt word with
                            | Some f -> Float f
                            | None -> Word word
        in
        loc, word

let lex filename included_from text =
    let rec skip_whitespace i loc =
        if i >= String.length text then i, loc
        else
            match text.[i] with
            | '\n'               -> skip_whitespace (i + 1) { loc with row = loc.row + 1; col = 1 }
            | ' '  | '\r' | '\t' -> skip_whitespace (i + 1) { loc with col = loc.col + 1 }
            | _ -> i, loc
    and get_string i loc =
        if i >= String.length text then i, loc
        else
            match text.[i] with
            | '"' -> i, loc
            | '\n' -> get_string (i + 1) { loc with row = loc.row + 1; col = 1 }
            | _    -> get_string (i + 1) { loc with col = loc.col + 1 }
    and get_char i loc =
        if i >= String.length text then i, loc
        else
            match text.[i] with
            | '\'' -> i, loc
            | '\n' -> raise @@ Error (loc, "unexpected newline in char literal")
            | _    -> get_char (i + 1) { loc with col = loc.col + 1 }
    and get_word i loc =
        if i >= String.length text then i, loc
        else
            match text.[i] with
            | ' ' | '\n' | '\r' | '\t' -> i, loc
            | _ -> get_word (i + 1) { loc with col = loc.col + 1 }
    in
    let rec lex' acc (i, loc) =
        if i >= String.length text then acc
        else
            match text.[i] with
            | ' ' | '\n' | '\r' | '\t' ->
                    lex' acc (skip_whitespace i loc)
            | '"' ->
                    let next, next_loc = get_string (i + 1) { loc with col = loc.col + 1 } in
                    lex' ((loc, String.sub text i (next - i + 1)) :: acc) (next + 1, next_loc)
            | '\'' ->
                    let next, next_loc = get_char (i + 1) { loc with col = loc.col + 1 } in
                    lex' ((loc, String.sub text i (next - i + 1)) :: acc) (next + 1, next_loc)
            | _ ->
                    let next, next_loc = get_word i loc in
                    lex' ((loc, String.sub text i (next - i)) :: acc) (next, next_loc)
    in
    lex' [] (0,
    {
        filename;
        included_from;
        expanded_from = [];
        row = 1; col = 1 })
    |> List.rev_map instr_of_word

let test actual expected =
    let matches = actual = expected in
    if matches then
        print_endline "OK"
    else
        print_endline (Format.asprintf "%s\n!=\n%s" (show_words actual) (show_words expected));
    matches

let loc = {
    filename = "[test]";
    included_from = [];
    expanded_from = [];
    row = 1;
    col = 1
}

let%test _ = test (lex "[test]" [] "+ 12 13 'c' 'cc'")
([
    { loc with col = 1  }, Add;
    { loc with col = 3  }, Int 12;
    { loc with col = 6  }, Int 13;
    { loc with col = 9  }, Char 'c';
    { loc with col = 13 }, Word "cc"
])
