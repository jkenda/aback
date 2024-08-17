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

type loc_typ = location * type_tok
[@@deriving show { with_path = false }]

(* token types *)
type word =
    | Include

    | Literal of data_tok
    | Type of type_tok

    | Sep | Return

    | Macro | Proc | Is | End
    | If | Then | Else
    | While | Do (* while ... end *)
    | Peek | Take | In (* peek ... end, take ... end *)
    | Mem | Var | Index | Assign

    | Dot_dot

    | Op of operator

    | Word of string
    | EOF
[@@deriving show { with_path = false }]

let string_of_word = function
    | Include -> "include"
    | Literal a -> string_of_data_tok a
    | Type t -> string_of_type_tok t

    | Sep -> ";;" | Return -> "->"

    | Macro -> "macro" | Proc -> "proc" | Is -> "is"
    | If -> "if" | Then -> "then" | Else -> "else"
    | While -> "while" | Do -> "do"
    | Peek -> "peek" | Take -> "take" | In -> "in"
    | Mem -> "mem" | Var -> "var"
    | End -> "end"

    | Index -> "[]" | Assign -> ":="

    | Dot_dot -> ".."

    | Op Eq -> "=" | Op NEq -> "!=" | Op Lt -> "<" | Op LEq -> "<=" | Op Gt -> ">" | Op GEq -> ">="

    | Op Add -> "+"
    | Op Sub -> "-"
    | Op Mul -> "*"
    | Op Div -> "/"
    | Op Mod -> "%"

    | Op Itof -> "itof" | Op Ftoi -> "ftoi"

    | Op LAnd -> "&"  | Op LOr -> "|" | Op LXor -> "^" | Op Lsl -> "<<" | Op Lsr -> ">>"
    | Op And  -> "&&" | Op Or -> "||"
    | Op Ref -> "@"   | Op Deref -> "."

    | Word w -> w
    | EOF -> "EOF"

type words = (location * word) list [@@deriving show { with_path = false }]

let string_of_words =
    List.fold_left (fun s w -> s ^ " " ^ string_of_word w) ""

let string_of_something_words list =
    List.fold_left (fun s (_, w) -> s ^ " " ^ string_of_word w) "" list

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

        | "i8" -> Type I8 | "i16" -> Type I16 | "i32" -> Type I32 | "i64" -> Type I64
        | "u8" -> Type U8 | "u16" -> Type U16 | "u32" -> Type U32 | "u64" -> Type U64
        | "f32" -> Type F32 | "f64" -> Type F64
        | "bool" -> Type Bool
        | "ptr" -> Type Ptr
        | "str" -> Type String | "cstr" -> Type CStr

        | "=" -> Op Eq | "/=" -> Op NEq
        | "<" -> Op Lt | "<=" -> Op LEq
        | ">" -> Op Gt | ">=" -> Op GEq

        | "+" -> Op Add | "-" -> Op Sub
        | "*" -> Op Mul | "/" -> Op Div
        | "%" -> Op Mod

        | "itof" -> Op Itof | "ftoi" -> Op Ftoi

        | "&"  -> Op LAnd | "|"  -> Op LOr | "^" -> Op LXor
        | "<<" -> Op Lsl  | ">>" -> Op Lsr
        | "&&" -> Op And  | "||" -> Op Or
        | "@"  -> Op Ref  | "."  -> Op Deref

        | ".." -> Dot_dot

        | "true" -> Literal (Bool true) | "false" -> Literal (Bool false)

        (* chars, strings, numbers and other words *)
        | word ->
                if String.ends_with ~suffix:{|"|} word then
                    if String.starts_with ~prefix:{|"|} word then
                        let string = String.sub word 1 (String.length word - 2) in
                        Literal (String (Scanf.unescaped @@ string))
                    else if String.starts_with ~prefix:"c\"" word then
                        let string = String.sub word 2 (String.length word - 3) in
                        Literal (CStr (Scanf.unescaped @@ string))
                    else
                        Word word
                else
                    if String.length word = 3
                    && String.starts_with ~prefix:"'" word
                    && String.ends_with   ~suffix:"'" word then
                        Literal (Char word.[1])
                else
                    if String.length word = 4
                    && String.starts_with ~prefix:"'\\" word
                    && String.ends_with   ~suffix:"'" word then
                        Literal (Char (match word.[2] with
                        | 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t'
                        | 'b' -> '\b' | '\\' -> '\\'
                        | _ -> raise @@ Error (loc, "invalid escape character")))
                else
                    match int_of_string_opt word with
                    | Some i -> Literal (Integer i)
                    | None ->
                            match float_of_string_opt word with
                            | Some f -> Literal (Decimal f)
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
        { loc with col = 3  }, Literal (Integer 12);
        { loc with col = 6  }, Literal (Integer 13);
        { loc with col = 9  }, Literal (Char 'c');
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
        { loc1 with col = 3 }, Literal (Integer 12);
        { loc1 with col = 6 }, Literal (Integer 13);

        { loc2 with col = 1  }, Literal (Char 'c');
        { loc2 with col = 5  }, Word "'cc'";
        { loc2 with col = 10 }, Word "drop"
    ])
