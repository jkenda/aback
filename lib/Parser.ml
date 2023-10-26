open Lexer
open Preprocess
open Program
open Format

type func = {
    t_in : t list;
    t_out : t list;
    seq : ir list
}
[@@deriving show { with_path = false }]

let rec parse strings funcs macros words =
    let nstrings = ref (List.length !strings) in
    let add name words table =
        let rec extract_types input t_in t_out = function
            | Type t :: words ->
                    if input then extract_types input (t :: t_in) t_out words
                    else extract_types input t_in (t :: t_out) words
            | Return :: words -> extract_types false t_in t_out words
            | Is :: words -> words, List.rev t_in, List.rev t_out
            | word :: _ -> raise @@ Failure (sprintf "Expected type, got %s" (show_prep word))
            | [] -> raise @@ Failure "unreachable"
        and add' acc = function
            | [] -> raise @@ Failure "'end' expected"
            | (Macro | Func ) :: _ -> raise @@ Failure "Nesting macros and functions not allowed"

            | End_func :: words ->
                    List.rev @@ parse strings funcs macros acc, words
            | word :: words -> add' (word :: acc) words
        in
        let words, t_in, t_out = extract_types true [] [] words in
        let seq, rest = add' [] words in
        Hashtbl.add table name { seq; t_in; t_out };
        rest
    and add_string str =
        let addr = !nstrings in
        strings := str :: !strings;
        nstrings := !nstrings + 1;
        addr, String.length str
    in
    let data_of_operation = function
        | (Int i : Preprocess.data) -> Int i
        | Float f -> Float f
        | Char c -> Char c
        | _ -> raise @@ Failure "Invalid data"
    in
    let ir_of_word = function
        | Push push ->
                (match push with
                | String str ->
                        let addr, len = add_string str in
                        [PUSH (Int len); PUSH (Ptr addr)]
                | CStr str ->
                        let addr, _ = add_string str in
                        [PUSH (Ptr addr)]
                | _  as push ->
                        [PUSH (data_of_operation push)])

        | Eq -> [EQ] | NEq -> [NE] | Lt -> [LT] | LEq -> [LE] | Gt -> [GT] | GEq -> [GE]

        | Add -> [ADD] | FAdd -> [FADD]
        | Sub -> [SUB] | FSub -> [FSUB]
        | Mul -> [MUL] | FMul -> [FMUL]
        | Div -> [DIV] | FDiv -> [FDIV]
        | Mod -> [MOD] | FMod -> [FMOD]

        | Puti -> [PUTI] | Putf -> [PUTF]
        | Putc -> [PUTC] | Puts -> [PUTS]
        | Putb -> [PUTB]

        | prep -> raise @@ Not_implemented (sprintf "'%s'" @@ show_prep prep)
    in
    let rec parse' (top, rest) = function
        | [] -> top :: rest
        | Macro :: Word name :: tl -> parse' ([], top :: rest) @@ add name tl macros
        | Func  :: Word name :: tl -> parse' ([], top :: rest) @@ add name tl funcs
        | Rev :: tl -> parse' ([], top :: rest) tl
        
        | If id :: tl -> parse' ([], [IF id] :: top :: rest) tl
        | Then id :: tl -> parse' ([], [THEN id] :: top :: rest) tl
        | Else id :: tl -> parse' ([], [ELSE id] :: top :: rest) tl
        | End_if id :: tl -> parse' ([], [END_IF id] :: top :: rest) tl

        | Word name :: tl ->
                let macro =
                    try Hashtbl.find macros name with Not_found ->
                    try Hashtbl.find funcs name with Not_found ->
                        raise @@ Failure (
                            sprintf "Unknown word: '%s'.\n\tavailable macros: %s\n\tavailable functions: %s"
                            name
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) macros "")
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) funcs ""))
                in
                parse' (macro.seq @ top, rest) tl
        | word :: tl -> parse' (ir_of_word word @ top, rest) tl
    in
    parse' ([], []) words
    |> List.rev
    |> List.flatten

