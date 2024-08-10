open Format
open Common

let lib_dirs = [
    "./";
    "~/.local/share/aback/"
]

(* read file from one of the dirs in lib_dirs *)
let read_lib_file included_from filename =
    if not (String.ends_with ~suffix:".ab" filename) then
        (let loc = { filename; included_from; expanded_from = []; row = 0; col = 0 } in
        raise @@ Error (loc, "Aback source files should have '.ab' extension"));

    let rec open_file = function
        | [] -> raise_notrace @@ Failure (sprintf "cannot find file \"%s\"" filename)
        | dir :: rest -> 
                try
                    let f = open_in (dir ^ filename) in
                    let s = really_input_string f (in_channel_length f) in
                    close_in f; s
                with _ -> open_file rest
    in
    open_file lib_dirs

type typ = Int | Float | Char | Bool | Ptr | String | CStr
[@@deriving show { with_path = false }]

let string_of_typ = function
    | Int -> "int" | Float -> "float" | Char -> "char"
    | Bool -> "bool" | Ptr -> "ptr" | String -> "str" | CStr -> "cstr"

let string_of_typs =
    List.fold_left (fun acc typ -> acc ^ string_of_typ typ ^ " ") ""

type loc_typ = location * typ
[@@deriving show { with_path = false }]

type operator =
    | Eq | NEq | Lt | LEq | Gt | GEq

    | Add | FAdd
    | Sub | FSub
    | Mul | FMul
    | Div | FDiv
    | Mod

    | Itof | Ftoi

    | LAnd | LOr | LXor | Lsl | Lsr
    | And  | Or
    | Ref | Deref

    | Putc | Puts
[@@deriving show { with_path = false }]

(* token types *)
type word =
    | Include

    | Int of int
    | Float of float
    | Char of char
    | String of string
    | CStr of string
    | True | False

    | Type of typ

    | Sep | Return

    | Macro | Proc | Is | End
    | If | Then | Else
    | While | Do (* while ... end *)
    | Peek | Take | In (* peek ... end, take ... end *)
    | Mem | Var | Index | Assign

    | Dot_dot_dot

    | Op of operator

    | Word of string
[@@deriving show { with_path = false }]

type words = (location * word) list [@@deriving show { with_path = false }]

(* get token from word *)
let instr_of_word (loc, word) =
    let word =
        match word with
        | "include" -> Include
        | ";;" -> Sep | "->" -> Return
        | "macro" -> Macro | "proc" -> Proc | "is" -> Is
        | "if" -> If | "then" -> Then | "else" -> Else | "end" -> End
        | "while" -> While | "do" -> Do
        | "peek" -> Peek | "take" -> Take | "in" -> In
        | "mem" -> Mem | "var" -> Var | "[]" -> Index | ":=" -> Assign

        | "int" -> Type Int | "float" -> Type Float
        | "char" -> Type Char | "ptr" -> Type Ptr
        | "bool" -> Type Bool

        | "=" -> Op Eq | "/=" -> Op NEq
        | "<" -> Op Lt | "<=" -> Op LEq
        | ">" -> Op Gt | ">=" -> Op GEq

        | "+" -> Op Add | "+." -> Op FAdd
        | "-" -> Op Sub | "-." -> Op FSub
        | "*" -> Op Mul | "*." -> Op FMul
        | "/" -> Op Div | "/." -> Op FDiv
        | "%" -> Op Mod

        | "itof" -> Op Itof | "ftoi" -> Op Ftoi

        | "&"  -> Op LAnd | "|"  -> Op LOr | "^" -> Op LXor
        | "<<" -> Op Lsl  | ">>" -> Op Lsr
        | "&&" -> Op And  | "||" -> Op Or
        | "@"  -> Op Ref  | "."  -> Op Deref

        | "..." -> Dot_dot_dot

        | "putc" -> Op Putc | "puts" -> Op Puts

        | "true" -> True | "false" -> False

        (* chars, strings, numbers and other words *)
        | word ->
                if String.ends_with ~suffix:{|"|} word then
                    if String.starts_with ~prefix:{|"|} word then
                        let string = String.sub word 1 (String.length word - 2) in
                        String (Scanf.unescaped @@ string)
                    else if String.starts_with ~prefix:"c\"" word then
                        let string = String.sub word 2 (String.length word - 3) in
                        CStr (Scanf.unescaped @@ string)
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
                        | 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t'
                        | 'b' -> '\b' | '\\' -> '\\'
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

(* tokenize the input and keep track of locations *)
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
                    let next, next_loc = get_string (i + 1) { loc with col = loc.col + 2 } in
                    lex' ((loc, String.sub text i (next - i + 1)) :: acc) (next + 1, next_loc)
            | '\'' ->
                    let next, next_loc = get_char (i + 1) { loc with col = loc.col + 2 } in
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

let%test _ =
    let loc = {
        filename = "[test]";
        included_from = [];
        expanded_from = [];
        row = 1; col = 1
    } in
    test (lex "[test]" [] "+ 12 13 'c' 'cc' drop")
    ([
        { loc with col = 1  }, Op Add;
        { loc with col = 3  }, Int 12;
        { loc with col = 6  }, Int 13;
        { loc with col = 9  }, Char 'c';
        { loc with col = 13 }, Word "'cc'";
        { loc with col = 18 }, Word "drop"
    ])

let%test _ =
    let loc1 = {
        filename = "[test]";
        included_from = [];
        expanded_from = [];
        row = 1; col = 1
    } in
    let loc2 = { loc1 with row = 2 } in

    test (lex "[test]" [] "+ 12 13\n'c' 'cc' drop")
    ([
        { loc1 with col = 1 }, Op Add;
        { loc1 with col = 3 }, Int 12;
        { loc1 with col = 6 }, Int 13;

        { loc2 with col = 1  }, Char 'c';
        { loc2 with col = 5  }, Word "'cc'";
        { loc2 with col = 10 }, Word "drop"
    ])
