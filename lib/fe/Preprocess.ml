open Common
open Lexer

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

    let preprocess'' (acc, words) =
        match words with
        | [] -> acc, []

        | (loc, Include) :: (_, Literal String src) :: tl ->
                let included_from = loc.filename :: loc.included_from in
                include_file included_from src @ acc, tl
        | (_, Include) :: (loc, _) :: _
        | (loc, Include) :: _ -> raise @@ Error (loc, "expected string after include")

        | ((_, Word w) :: _) as words when String.starts_with ~prefix:"(" w ->
                acc, remove_comment words

        | (loc, word) :: tl ->
                (loc, match word with
                | Literal l -> Literal l

                | Macro -> Macro | Proc -> Proc | Is -> Is
                | If -> If | Then -> Then | Else -> Else
                | While -> While | Do -> Do
                | Peek -> Peek | Take -> Take | In -> In
                | Mem -> Mem | Var -> Var
                | End -> End

                | Sep -> Sep

                | Type t -> Type t

                | Dot_dot_dot -> Dot_dot_dot

                | Assign -> Assign | Index -> Index | Return -> Return

                | Op op -> Op op

                | Word w -> Word w
                | _ -> raise @@ Not_implemented (loc, show_word word)) :: acc, tl

    in
    let prepend_eof words =
        let loc =
            match words with
            | ((loc, _) :: _) ->
                    { loc with col = loc.col; row = 0 }
            | [] ->
                    { filename = ""; included_from = []; expanded_from = []; row = 1; col = 1 }
        in
        loc, EOF
    in
    let eof = prepend_eof words in
    let rec preprocess' ((acc, words) as data) =
        match words with
        | [] -> List.rev (eof :: acc), end_stack
        | _ -> preprocess' @@ preprocess'' data
    in
    let acc, end_stack = preprocess' ([], words) in
    if Stack.is_empty end_stack then acc
    else
        let loc, _ = Stack.pop end_stack in
        raise @@ Error (loc, "no matching 'end'")
