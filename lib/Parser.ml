open Lexer
open Preprocess
open Program
open Format

type func = {
    loc : location;
    seq : (location * ir) list;
    types  : (loc_typ list * loc_typ list) option
}
[@@deriving show { with_path = false }]

let _print_ir ir =
    let open Format in
    let str, _ = List.fold_left (fun (acc, i) ir -> acc ^ sprintf "%d: %s\n" i (show_ir ir), i + 1) ("", 0) ir in
    print_string str

let _print_funcs funcs =
    print_string 
    @@ Hashtbl.fold (fun name macro acc -> acc ^ sprintf "%s: %s\n" name (show_func macro)) funcs ""

let rec parse strings procs macros max_addr words =
    let nstrings = ref (List.length !strings) in

    let add_func loc id name words table =
        let rec extract_types input t_in t_out = function
            | (loc, Type t) :: words ->
                    if input then extract_types input ((loc, t) :: t_in) t_out words
                    else extract_types input t_in ((loc, t) :: t_out) words
            | (_, Return) :: words -> extract_types false t_in t_out words
            | (_, Is) :: words -> (List.rev t_in, List.rev t_out), words
            | (loc, word) :: _ -> raise @@ Error (loc, sprintf "Expected 'is' or type, got %s" (show_prep word))
            | [] -> raise @@ Error (loc, "expected 'is' after function declaration")
        and add' acc = function
            | [] -> raise @@ Error (loc, "'end' expected")
            | (loc, Word _name) :: _ when name = _name ->
                    raise @@ Error (loc, sprintf "%s: recursive macros not supported" name)
            | (_, End_func _id) :: words when _id = id ->
                    parse strings procs macros max_addr @@ List.rev acc, words
            | word :: words -> add' (word :: acc) words
        in
        let types, words =
            match words with
            | (_, Is) :: tl -> None, tl
            | _ ->
                    let types, words = extract_types true [] [] words in
                    Some types, words
        in
        let seq, words = add' [] words in
        Hashtbl.replace table name { loc; types; seq };
        words
    and add_string str =
        let addr = !nstrings in
        strings := str :: !strings;
        nstrings := !nstrings + 1;
        addr, String.length str
    and parse_vars loc words =
        let rec parse' vars = function
            | (_, In) :: words -> List.rev vars, words
            | (loc, Word name) :: words -> parse' ((loc, name) :: vars) words
            | (loc, word) :: _ -> raise @@ Error (loc, sprintf "Expected name or In, got %s" (show_prep word))
            | [] -> raise @@ Error (loc, "expected 'is' after variable list")
        in
        parse' [] words
    in
    let data_of_operation loc = function
        | (Int i : Preprocess.data) -> Int i
        | Float f -> Float f
        | Char c -> Char c
        | Bool b -> Bool b
        | _ -> raise @@ Not_implemented (loc, "Invalid data")
    in
    let ir_of_word loc word =
        let ir_of_word' = function
            | Push push ->
                    (match push with
                    | String str ->
                            let addr, len = add_string str in
                            [PUSH (Int len); PUSH (Ptr addr)]
                    | CStr str ->
                            let addr, _ = add_string str in
                            [PUSH (Ptr addr)]
                    | _  as push ->
                            [PUSH (data_of_operation loc push)])

            | Eq -> [EQ] | NEq -> [NE] | Lt -> [LT] | LEq -> [LE] | Gt -> [GT] | GEq -> [GE]

            | Add -> [ADD] | FAdd -> [FADD]
            | Sub -> [SUB] | FSub -> [FSUB]
            | Mul -> [MUL] | FMul -> [FMUL]
            | Div -> [DIV] | FDiv -> [FDIV]
            | Mod -> [MOD] | FMod -> [FMOD]

            | And -> [AND] | Or -> [OR]
            | BAnd -> [BAND] | BOr -> [BOR] | BXor -> [BXOR]
            | Lsl -> [LSL] | Lsr -> [LSR]

            | Puti -> [PUTI] | Putf -> [PUTF]
            | Putc -> [PUTC] | Puts -> [PUTS]
            | Putb -> [PUTB]

            | prep -> raise @@ Not_implemented (loc, show_prep prep)
        in
        ir_of_word' word
        |> List.map (fun ir -> loc, ir)
    in

    let vars = Hashtbl.create 10
    and locs = Hashtbl.create 10 in
    let rec parse' (top, rest) names = function
        | [] -> top :: rest
        | (_, (Macro id : prep)) :: (loc, Word name) :: tl ->
                parse' ([], top :: rest) names
                @@ add_func loc id name tl macros
        | (loc, Macro _) :: _ -> raise @@ Error (loc, "macro: expected name")
        | (_, Proc id) :: (loc, Word name) :: tl ->
                parse' ([], top :: rest) names
                @@ add_func loc id name tl procs
        | (loc, Proc _) :: _ -> raise @@ Error (loc, "proc: expected name")
        | (_, Rev) :: tl -> parse' ([], top :: rest) names tl
        
        | (loc,  ((If _ | Then _ | Else _ | End_if _ | While _ | Do _ | End_while _) as word)) :: tl ->
                let instr =
                    match word with
                    | If id        -> IF id
                    | Then id      -> THEN id
                    | Else id      -> ELSE id
                    | End_if id    -> END_IF id
                    | While id     -> WHILE id
                    | Do id        -> DO id
                    | End_while id -> END_WHILE id
                    | _ -> raise (Unreachable "")
                in
                (parse' ([], [loc, instr] :: top :: rest) names tl)

        | (loc, ((Peek | Take) as word)) :: tl ->
                let n, tl = parse_vars loc tl in
                (* TODO: check if variable exists (shadowing?) *)
                List.iter (fun (loc, name) ->
                    let addr = Hashtbl.length vars in
                    max_addr := max !max_addr addr;
                    Hashtbl.replace vars name addr;
                    Hashtbl.replace locs loc  addr) n;
                let irs =
                    List.mapi (fun depth (loc, name) ->
                    if word = Peek
                    then loc, PEEK (depth, (Hashtbl.find vars name))
                    else loc, TAKE (Hashtbl.find vars name)) n
                in
                let _, n = List.split n in
                parse' ([], irs :: top :: rest) (n :: names) tl
        | (loc, End_peek) :: tl ->
                (try List.hd names with _ ->
                    raise @@ Error (loc, "cannot end peek/take"))
                |> List.iter @@ Hashtbl.remove vars;
                parse' ([], top :: rest) (List.tl names) tl

        | (loc, Word name) :: tl when Hashtbl.mem vars name ->
                let var = Hashtbl.find vars name in
                parse' ((loc, PUT var) :: top, rest) names tl

        | (loc, Word name) :: tl when Hashtbl.mem macros name ->
                let expand =
                    List.map (fun (l, ir) ->
                        { l with expanded_from = (loc, name) :: l.expanded_from }, ir)
                in
                let macro = Hashtbl.find macros name in
                parse' (expand macro.seq @ top, rest) names tl

        (*
        | (_, Word name) :: tl when Hashtbl.mem procs name ->
                let proc = Hashtbl.find procs name in ... *)

        | (loc, Word name) :: _ ->
                        raise @@ Error (loc, 
                            sprintf "Unknown word: '%s'.\n\tavailable vars: %s\n\tavailable macros: %s\n\tavailable procs: %s"
                            name
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) vars "")
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) macros "")
                            (Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) procs ""))

        | (loc, word) :: tl -> parse' (ir_of_word loc word @ top, rest) names tl
    in
    parse' ([], []) [] words
    |> List.rev
    |> List.flatten

