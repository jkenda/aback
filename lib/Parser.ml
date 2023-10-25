open Lexer
open Program

let rec parse strings words =
    let macros = Hashtbl.create 10
    and nstrings = ref (List.length !strings) in
    let add_macro name words =
        let rec skip_types = function
            | Is :: words -> words
            | _ :: words -> skip_types words
            | [] -> raise @@ Failure "Expected type"
        and add_macro' acc starts = function
            | End :: words when starts = 0 -> Hashtbl.add macros name @@ parse strings acc; words
            | (While | Peek | Take) as word :: words -> add_macro' (word :: acc) (starts + 1) words
            | (Macro | Func) :: _ -> raise @@ Failure "Nesting macros and functions not allowed"
            | [] -> raise @@ Failure "'end' expected"
            | _ when starts < 0 -> raise @@ Failure "Too many 'end's"
            | word :: words -> add_macro' (word :: acc) starts words
        in
        add_macro' [] 0 @@ skip_types words
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
    let rec parse' (top, rest) = function
        | [] -> top :: rest
        | (Int _ : word) | Float _ | Char _ as push :: tl ->
                parse' (Push (data_of_operation push) :: top, rest) tl
        | Macro :: Word name :: tl -> parse' ([], top :: rest) @@ add_macro name tl
        | Add  :: tl -> parse' (Add  :: top, rest) tl
        | Putc :: tl -> parse' (Putc :: top, rest) tl
        | Puti :: tl -> parse' (Puti :: top, rest) tl
        | Puts :: tl -> parse' (Puts :: top, rest) tl
        | String str :: tl ->
                let addr, len = add_string str in
                parse' (Push (Int len) :: Push (Ptr addr) :: top, rest) tl

        | Rev :: tl -> parse' ([], top :: rest) tl
        | Word name :: tl -> parse' (Hashtbl.find macros name @ top, rest) tl
        | word :: _ -> raise @@ Failure (Format.sprintf "'%s' not implemeted" @@ show_word word)
    in
    parse' ([], []) words
    |> List.rev
    |> List.flatten
