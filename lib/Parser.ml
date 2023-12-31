open Lexer
open Preprocess
open Program
open Format

type func_format =
    | Typed of (location * prep) list * (location * prep) list
    | Numbered of int * int
    | Untyped
[@@deriving show { with_path = false }]

type func = {
    loc : location;
    seq : (location * prep) list;
    types  : func_format
}
[@@deriving show { with_path = false }]

let _print_funcs funcs =
    print_string 
    @@ Hashtbl.fold (fun name macro acc -> acc ^ sprintf "%s: %s\n" name (show_func macro)) funcs ""

let strings = ref ""
let takes = Hashtbl.create 10
let mem = Hashtbl.create 10
let vars = Hashtbl.create 10
let names = ref []
let next_addr = ref 0
let max_addr = ref (-1)

(* parse the preprocessed words into intermediate representation *)
let rec parse ptr_size procs macros words =
    let add_func loc name words table =
        let rec extract_types input t_in t_out = function
            | (loc, (Type _ | Word _ as t)) :: words ->
                    if input then extract_types input ((loc, t) :: t_in) t_out words
                    else extract_types input t_in ((loc, t) :: t_out) words
            | (_, Return) :: words -> extract_types false t_in t_out words
            | (_, Is) :: words -> List.rev t_in, List.rev t_out, words
            | (loc, word) :: _ -> raise @@ Error (loc,
                sprintf "Expected 'is' or type, got %s" (print_prep word))
            | [] -> raise @@ Error (loc, "expected 'is' after function declaration")
        and add' acc = function
            | [] -> raise @@ Error (loc, "'end' expected")
            | (loc, Mem) :: _ -> raise @@ Error (loc, "cannot allocate global memory inside a function")
            | (loc, Word _name) :: _ when name = _name ->
                    raise @@ Error (loc, sprintf "%s: recursive macros not supported" name)
            | (_, End) :: words ->
                    List.rev acc, words
            | word :: words -> add' (word :: acc) words
        in
        let types, words =
            match words with
            | (_, Push Int n_in) :: (_, Return) :: (_, Push Int n_out) :: (_, Is) :: tl ->
                    Numbered (n_in, n_out), tl
            | (_, Is) :: tl -> Untyped, tl
            | _ ->
                    let t_in, t_out, words = extract_types true [] [] words in
                    Typed (t_in, t_out), words
        in
        let seq, words = add' [] words in
        Hashtbl.replace table name { loc; types; seq };
        words
    and add_string str =
        try
            let re = Str.regexp_string str in
            Str.search_forward re !strings 0, String.length str
        with Not_found ->
            let addr = String.length !strings in
            strings := !strings ^ str ^ "\x00";
            addr, String.length str
    and parse_vars loc words =
        let rec parse' vars = function
            | (_, In) :: words -> List.rev vars, words
            | (loc, Word name) :: words -> parse' ((loc, name) :: vars) words
            | (loc, word) :: _ -> raise @@ Error (loc,
                sprintf "Expected name or In, got %s" (print_prep word))
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
                            [PUSH (Int len); PUSH (Ptr ("strs", addr))]
                    | CStr str ->
                            let addr, _ = add_string str in
                            [PUSH (Ptr ("strs", addr))]
                    | _  as push ->
                            [PUSH (data_of_operation loc push)])

            | Eq -> [EQ] | NEq -> [NE] | Lt -> [LT] | LEq -> [LE] | Gt -> [GT] | GEq -> [GE]

            | Add -> [ADD] | FAdd -> [FADD]
            | Sub -> [SUB] | FSub -> [FSUB]
            | Mul -> [MUL] | FMul -> [FMUL]
            | Div -> [DIV] | FDiv -> [FDIV]
            | Mod -> [MOD]

            | Itof -> [ITOF] | Ftoi -> [FTOI]

            | And -> [AND] | Or -> [OR]
            | LAnd -> [LAND] | LOr -> [LOR] | LXor -> [LXOR]
            | Lsl -> [LSL] | Lsr -> [LSR]

            | Putc -> [PUTC] | Puts -> [PUTS]

            | prep -> raise @@ Not_implemented (loc, print_prep prep)
        in
        ir_of_word' word
        |> List.map (fun ir -> loc, ir)
    in

    let rec add_if if_loc words =
        let end_stack = ref 0 in
        let rec to_then acc = function
            | (loc, Then) :: tl when !end_stack = 0 -> List.rev acc, loc, tl
            | _, If as lw :: tl ->
                    end_stack := !end_stack + 1; to_then (lw :: acc) tl
            | _, End_if as lw :: tl ->
                    end_stack := !end_stack - 1; to_then (lw :: acc) tl
            | lw :: tl -> to_then (lw :: acc) tl
            | [] -> raise @@ Unreachable "already checked for end mismatch"
        and to_else acc = function
            | (loc, Else)   :: tl when !end_stack = 0 -> List.rev acc, loc, to_end [] tl
            | (loc, End_if) :: tl when !end_stack = 0 -> [], loc, (List.rev acc, loc, tl)
            | _, If as lw :: tl ->
                    end_stack := !end_stack + 1; to_else (lw :: acc) tl
            | _, End_if as lw :: tl ->
                    end_stack := !end_stack - 1; to_else (lw :: acc) tl
            | lw :: tl -> to_else (lw :: acc) tl
            | [] -> raise @@ Unreachable "already checked for end mismatch"
        and to_end acc = function
            | (loc, End_if) :: tl when !end_stack = 0 -> List.rev acc, loc, tl
            | _, If as lw :: tl ->
                    end_stack := !end_stack + 1; to_end (lw :: acc) tl
            | _, End_if as lw :: tl ->
                    end_stack := !end_stack - 1; to_end (lw :: acc) tl
            | lw :: tl -> to_end (lw :: acc) tl
            | [] -> raise @@ Unreachable "already checked for end mismatch"
        in
        let if_then, then_loc, words = to_then [] words in
        let then_else, else_loc, (else_end, end_loc, words) = to_else [] words in
        let parse' = parse' ([], []) in
        let seq =
            match then_else with
            | [] ->
                [if_loc, IF 0] ::
                    List.rev (parse' if_then) @
                [then_loc, THEN 0] ::
                    List.rev (parse' else_end) @
                [end_loc, END_IF 0] :: []
            | _ ->
                [if_loc, IF 0] ::
                    List.rev (parse' if_then) @
                [then_loc, THEN 0] ::
                    List.rev (parse' then_else) @
                [else_loc, ELSE 0] ::
                    List.rev (parse' else_end) @
                [end_loc, END_IF 0] :: []
        in
        seq |> List.flatten, words

    and add_while while_loc words =
        let end_stack = ref 0 in
        let rec to_do acc = function
            | (loc, Do) :: tl when !end_stack = 0 -> List.rev acc, loc, tl
            | _, While as lw :: tl ->
                    end_stack := !end_stack + 1; to_do (lw :: acc) tl
            | _, End_while as lw :: tl ->
                    end_stack := !end_stack - 1; to_do (lw :: acc) tl
            | lw :: tl -> to_do (lw :: acc) tl
            | [] -> raise @@ Unreachable "already checked for end mismatch"
        and to_end acc = function
            | (loc, End_while) :: tl when !end_stack = 0 -> List.rev acc, loc, tl
            | _, While as lw :: tl ->
                    end_stack := !end_stack + 1; to_end (lw :: acc) tl
            | _, End_while as lw :: tl ->
                    end_stack := !end_stack - 1; to_end (lw :: acc) tl
            | lw :: tl -> to_end (lw :: acc) tl
            | [] -> raise @@ Unreachable "already checked for end mismatch"
        in
        let while_do, do_loc, words = to_do [] words in
        let do_end, end_loc, words = to_end [] words in
        let parse' = parse' ([], []) in
        let seq =
                [while_loc, WHILE 0] ::
                    List.rev (parse' while_do) @
                [do_loc, DO 0] ::
                    List.rev (parse' do_end) @
                [end_loc, END_WHILE 0] :: []
        in
        seq |> List.flatten, words

    and parse' (top, rest) = function
        | [] -> top :: rest
        | (_, (Macro : prep)) :: (loc, Word name) :: tl ->
                parse' ([], top :: rest)
                @@ add_func loc name tl macros
        | (loc, Macro) :: _ -> raise @@ Error (loc, "macro: expected name")
        | (_, Proc ) :: (loc, Word name) :: tl ->
                parse' ([], top :: rest)
                @@ add_func loc name tl procs
        | (loc, Proc) :: _ -> raise @@ Error (loc, "proc: expected name")

        | (_, Var) :: (_, Word name) :: (_, Type t) :: (_, End) :: tl ->
                Hashtbl.add vars name t;
                parse' (top, rest) tl
        | (_, Mem) :: (_, Word name) :: (_, Type t) :: (_, Push Int size) :: (_, End) :: tl ->
                Hashtbl.add mem name (t, size);
                parse' (top, rest) tl
        | (_, Mem) :: (_, Word name) :: (_, Type t) :: (loc, Word size) :: (_, End) :: tl
            when Hashtbl.mem macros size ->
                let size =
                    match Hashtbl.find_opt macros size with
                    | Some { seq = [_, Push Int size]; _ } -> size
                    | _ -> raise @@ Error (loc, "size has to be of constant value")
                in
                Hashtbl.add mem name (t, size);
                parse' (top, rest) tl
        | (loc, Mem) :: _ ->
                raise @@ Error (loc, sprintf "usage: mem <name> <type> <size> end")
        | (loc, Var) :: _ ->
                raise @@ Error (loc, sprintf "usage: var <name> <type> end")

        | (li, Index) :: (ln, Word name) :: tl when Hashtbl.mem mem name ->
                let t, _size = Hashtbl.find mem name in
                let size =
                    match t with
                    | Char -> 1
                    | Ptr
                    | _ -> ptr_size
                in
                let instrs = [
                   li, PUSH (Int size);
                   li, MUL;
                   ln, PUSH (Ptr ("mem_" ^ name, 0));
                   li, ADD;
                   li, LOAD t
                ] in
                parse' (instrs @ top, rest) tl
        | (la, Assign) :: (li, Index) :: (ln, Word name) :: tl when Hashtbl.mem mem name ->
                let t, _size = Hashtbl.find mem name in
                let size =
                    match t with
                    | Char -> 1
                    | Ptr
                    | _ -> ptr_size
                in
                let instrs = [
                   li, PUSH (Int size);
                   li, MUL;
                   ln, PUSH (Ptr ("mem_" ^ name, 0));
                   li, ADD;
                   la, STORE t
                ] in
                parse' (instrs @ top, rest) tl
        | (la, Assign) :: (ln, Word name) :: tl when Hashtbl.mem vars name ->
                let t = Hashtbl.find vars name in
                let instrs = [
                   ln, PUSH (Ptr ("var_" ^ name, 0));
                   la, STORE t
                ] in
                parse' (instrs @ top, rest) tl
        | (_, Index) :: (ln, Word name) :: _ ->
                let mem = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) mem "" in
                raise @@ Error (ln, sprintf "unknown mem: %s. available: %s" name mem)
        | (_, Assign) :: (_, Index) :: (ln, Word name) :: _ ->
                let mem = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) mem "" in
                raise @@ Error (ln, sprintf "unknown mem: %s. available: %s" name mem)
        | (loc, Index) :: _ ->
                raise @@ Error (loc, "expected [] <mem> <index>")
        | (loc, Assign) :: _ ->
                raise @@ Error (loc, "expected := [] <mem> <index>")

        | (_, Rev) :: tl -> parse' ([], top :: rest) tl
        
        | (loc, If) :: tl ->
                let parsed, tl = add_if loc tl in
                (parse' (parsed @ top, rest) tl)

        | (loc, While) :: tl ->
                let parsed, tl = add_while loc tl in
                (parse' ([], parsed :: top :: rest) tl)

        | (loc, ((Peek | Take) as word)) :: tl ->
                let n, tl = parse_vars loc tl
                and top_takes = ref [] in
                let irs =
                    n |>
                    List.mapi (fun depth (loc, name) ->
                        let addr = !next_addr in
                        if not @@ String.starts_with ~prefix:"_" name then
                            (max_addr := max !max_addr addr;
                            next_addr := !next_addr + 1;
                            top_takes := name :: !top_takes;
                            Hashtbl.add takes name addr);
                        if word = Peek
                        then loc, PEEK (depth, addr)
                        else loc, TAKE addr)
                in
                names := !top_takes :: !names;
                parse' ([], irs :: top :: rest) tl
        | (loc, End_peek) :: tl ->
                let top_vars =
                    try List.hd !names
                    with _ -> raise @@ Error (loc, "cannot end peek/take")
                in
                next_addr := !next_addr - List.length top_vars;
                List.iter (Hashtbl.remove takes) top_vars;
                names := List.tl !names;
                parse' ([], top :: rest) tl

        | (loc, Syscall) :: (_, Push Int i) :: tl ->
            parse' ((loc, SYSCALL i) :: top, rest) tl
        | (loc, Syscall) :: _ -> raise @@ Error (loc, "syscall: expected int")

        | (loc, Word name) :: tl when Hashtbl.mem takes name ->
                let var = Hashtbl.find takes name in
                parse' ((loc, PUT var) :: top, rest) tl

        | (loc, Word name) :: tl when Hashtbl.mem vars name ->
                let t = Hashtbl.find vars name in
                let instrs = [
                    loc, PUSH (Ptr ("var_" ^ name, 0));
                    loc, LOAD t
                ] in
                parse' (instrs @ top, rest) tl

        | (loc, Word name) :: tl when Hashtbl.mem mem name ->
                parse' ((loc, PUSH (Ptr ("mem_" ^ name, 0))) :: top, rest) tl

        | (loc, Word name) :: tl when Hashtbl.mem macros name ->
                let expand prep =
                    parse ptr_size procs macros prep
                    |> List.map (fun (l, prep) ->
                        { l with expanded_from = (loc, name) :: l.expanded_from }, prep)
                in
                let macro = Hashtbl.find macros name in
                parse' ((loc, FN name) :: expand macro.seq @ (loc, FN_END) :: top, rest) tl

        | (loc, Word name) :: _tl when Hashtbl.mem procs name ->
                 let _proc = Hashtbl.find procs name in
                raise @@ Not_implemented (loc, "Procs aren't implemented yet!")

        | (loc, Word name) :: _ ->
                let vars = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) takes ""
                and mem  = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) mem ""
                and procs = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) procs ""
                and macros = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) macros "" in
                raise @@ Error (loc, 
                    sprintf "Unknown word: '%s'.\n" name ^
                    sprintf "\tavailable vars: %s\n" vars ^
                    sprintf "\tavailable mem: %s\n" mem ^
                    sprintf "\tavailable macros: %s\n" macros ^
                    sprintf "\tavailable procs: %s" procs)

        | (loc, word) :: tl -> parse' (ir_of_word loc word @ top, rest) tl
    in
    let set_ids instrs =
        let end_stack = Stack.create () in
        let push_end data = Stack.push data end_stack
        and pop_end () = ignore @@ Stack.pop end_stack in
        let set' (acc, next_id) (loc, inst) =
            let instr, next_id =
                match inst, Stack.top_opt end_stack with
                | IF        _, _ -> push_end @@ IF    next_id; IF    next_id, next_id + 1
                | WHILE     _, _ -> push_end @@ WHILE next_id; WHILE next_id, next_id + 1
                | THEN      _, Some IF    id -> THEN id, next_id
                | ELSE      _, Some IF    id -> ELSE id, next_id
                | DO        _, Some WHILE id -> DO   id, next_id
                | END_IF    _, Some IF    id -> pop_end (); END_IF    id, next_id
                | END_WHILE _, Some WHILE id -> pop_end (); END_WHILE id, next_id
                | THEN      _, None -> raise @@ Error (loc, "unmatched 'then'")
                | ELSE      _, None -> raise @@ Error (loc, "unmatched 'else'")
                | DO        _, None -> raise @@ Error (loc, "unmatched 'do'")
                | END_IF    _, None
                | END_WHILE _, None -> raise @@ Error (loc, "unmatched 'end'")
                | _ -> inst, next_id
            in (loc, instr) :: acc, next_id
        in
        fst @@ List.fold_left set' ([], 0) instrs
        |> List.rev
    in

    parse' ([], []) words
    |> List.rev
    |> List.flatten
    |> set_ids
