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

let _print_ir ir =
    let open Format in
    let str, _ = List.fold_left (fun (acc, i) ir -> acc ^ sprintf "%d: %s\n" i (show_ir ir), i + 1) ("", 0) ir in
    print_string str

let _print_funcs funcs =
    print_string 
    @@ Hashtbl.fold (fun name macro acc -> acc ^ sprintf "%s: %s\n" name (show_func macro)) funcs ""

let rec parse strings procs macros words =
    let nstrings = ref (List.length !strings) in
    let add_func name words table =
        let rec extract_types input t_in t_out = function
            | Type t :: words ->
                    if input then extract_types input (t :: t_in) t_out words
                    else extract_types input t_in (t :: t_out) words
            | Return :: words -> extract_types false t_in t_out words
            | Is :: words -> List.rev t_in, List.rev t_out, words
            | word :: _ -> raise @@ Failure (sprintf "Expected type, got %s" (show_prep word))
            | [] -> raise @@ Failure "expected 'is' after function declaration"
        and add' acc = function
            | [] -> raise @@ Failure "'end' expected"
            | (Macro | Proc) :: _ ->
                    raise @@ Failure "Nesting macros and procs is not allowed"
            | End_func :: words ->
                    parse strings procs macros @@ List.rev acc, words
            | word :: words -> add' (word :: acc) words
        in
        let t_in, t_out, words = extract_types true [] [] words in
        let seq, words = add' [] words in
        Hashtbl.add table name { t_in; t_out; seq };
        words
    and add_string str =
        let addr = !nstrings in
        strings := str :: !strings;
        nstrings := !nstrings + 1;
        addr, String.length str
    and parse_vars words =
        let rec parse' vars = function
            | In :: words -> List.rev vars, words
            | Word name :: words -> parse' (name :: vars) words
            | word :: _ -> raise @@ Failure (sprintf "Expected name or In, got %s" (show_prep word))
            | [] -> raise @@ Failure "expected 'is' after variable list"
        in
        parse' [] words
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

        | prep -> raise @@ Not_implemented (show_prep prep)
    in

    let vars = Hashtbl.create 10 in
    let rec parse' (top, rest) names = function
        | [] -> top :: rest
        | (Macro : prep) :: Word name :: tl ->
                parse' ([], top :: rest) names
                @@ add_func name tl macros
        | Macro :: _ -> raise @@ Failure "macro: expected name"
        | Proc  :: Word name :: tl ->
                parse' ([], top :: rest) names
                @@ add_func name tl procs
        | Proc :: _ -> raise @@ Failure "proc: expected name"
        | Rev :: tl -> parse' ([], top :: rest) names tl
        
        | (If _ | Then _ | Else _ | End_if _ | While _ | Do _ | End_while _) as word :: tl ->
                (parse' ([], (match word with
                | If id        -> [IF id]
                | Then id      -> [THEN id]
                | Else id      -> [ELSE id]
                | End_if id    -> [END_IF id]
                | While id     -> [WHILE id]
                | Do id        -> [DO id]
                | End_while id -> [END_WHILE id]
                | _ -> raise (Unreachable "")) :: top :: rest) names tl)

        | Peek | Take as word :: tl ->
                let n, tl = parse_vars tl in
                (* TODO: check if variable exists (shadowing?) *)
                List.iter (fun name -> Hashtbl.add vars name @@ Hashtbl.length vars) n;
                let irs =
                    List.mapi (fun depth name ->
                    if word = Peek
                    then PEEK (depth, (Hashtbl.find vars name))
                    else TAKE (Hashtbl.find vars name)) n
                in
                parse' ([], irs :: top :: rest) (n :: names) tl
        | End_peek :: tl ->
                (try List.hd names with Failure _ ->
                    raise @@ Unreachable "cannot end peek/take")
                |> List.iter @@ Hashtbl.remove vars;
                parse' ([], top :: rest) (List.tl names) tl

        | Word name :: tl ->
                (match Hashtbl.find_opt vars name with
                | Some var -> parse' (PUT var :: top, rest) names tl
                | None ->
                let macro =
                    try Hashtbl.find macros name with Not_found ->
                    try Hashtbl.find procs name with Not_found ->
                        raise @@ Failure (
                            sprintf "Unknown word: '%s'.\n\tavailable vars: %s\n\tavailable macros: %s\n\tavailable procs: %s"
                            name
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) vars "")
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) macros "")
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) procs ""))
                in
                parse' (macro.seq @ top, rest) names tl)
        | word :: tl -> parse' (ir_of_word word @ top, rest) names tl
    in
    parse' ([], []) [] words
    |> List.rev
    |> List.flatten

