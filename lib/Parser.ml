open Lexer
open Program
open Format

type strings = string list
[@@deriving show { with_path = false }]

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
            | Is :: words -> words, t_in, t_out
            | word :: _ -> raise @@ Failure (sprintf "Expected type, got %s" (show_word word))
            | [] -> raise @@ Failure "unreachable"
        and add' acc starts = function
            | [] -> raise @@ Failure "'end' expected"
            | _ when starts < 0   -> raise @@ Failure "Too many 'end's"
            | (Macro | Func) :: _ -> raise @@ Failure "Nesting macros and functions not allowed"

            | End :: words when starts = 0 ->
                    List.rev @@ parse strings funcs macros acc, words
            | word :: words -> add' (word :: acc) starts words
        in
        let words, t_in, t_out = extract_types true [] [] words in
        let seq, rest = add' [] 0 words in
        Hashtbl.add table name { seq; t_in; t_out };
        rest
    and add_string str =
        let addr = !nstrings in
        strings := str :: !strings;
        nstrings := !nstrings + 1;
        addr, String.length str
    in
    let data_of_operation = function
        | (Int i : word) -> Int i
        | Float f -> Float f
        | Char c -> Char c
        | _ -> raise @@ Failure "Invalid data"
    in
    let ir_of_word = function
        (Int _ : word) | Float _ | Char _ as push ->
            [Push (data_of_operation push)]

        | String str ->
                let addr, len = add_string str in
                [Push (Int len); Push (Ptr addr)]

        | Add -> [Add] | FAdd -> [FAdd]
        | Sub -> [Sub] | FSub -> [FSub]
        | Mul -> [Mul] | FMul -> [FMul]
        | Div -> [Div] | FDiv -> [FDiv]
        | Mod -> [Mod] | FMod -> [FMod]

        | Puti -> [Puti] | Putf -> [Putf]
        | Putc -> [Putc] | Puts -> [Puts]

        | word -> raise @@ Failure (sprintf "'%s' not implemeted" @@ show_word word)
    in
    let rec parse' (top, rest) = function
        | [] -> top :: rest
        | Macro :: Word name :: tl -> parse' ([], top :: rest) @@ add name tl macros
        | Func  :: Word name :: tl -> parse' ([], top :: rest) @@ add name tl funcs
        | Rev :: tl -> parse' ([], top :: rest) tl

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
