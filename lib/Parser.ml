open Lexer
open Preprocess
open Format

type func_format =
    | Typed of (location * prep) list * (location * prep) list
    | Numbered of int * int
[@@deriving show { with_path = false }]

type name = string  [@@deriving show { with_path = false }]
type addr = int     [@@deriving show { with_path = false }]
type size = int     [@@deriving show { with_path = false }]
type id   = int     [@@deriving show { with_path = false }]

type node =
    | Empty

    | Var of name * typ
    | Mem of name * (typ * size)
    | Take of name list
    | Peek of name list
    | Push_take of name
    | Push_literal of data

    | Proc of name * proc * node list
    | Macro of name * macro * node list
    | Syscall of id * node list

    | If of node * node * node
    | While of node * node

    | Index_into of name * node
    | Assign_to_mem of name * node * node
    | Assign_to_var of name * node
    | If_statement of node * node list * node list
    | While_statement of node * node list

    | Literal of data
    | Op of operator * node * node
[@@deriving show { with_path = false }]

and proc = {
    p_loc : location;
    p_seq : node list;
    p_types   : func_format;
    recursive : bool;
}
[@@deriving show { with_path = false }]

and macro = {
    m_loc : location;
    m_seq : (location * prep) list;
    m_types  : func_format;
}
[@@deriving show { with_path = false }]

let nargs = function
    | Typed (args, _) -> List.length args
    | Numbered (nargs, _) -> nargs

let _print_macros funcs =
    print_string 
    @@ Hashtbl.fold (fun name macro acc -> acc ^ sprintf "%s: %s\n" name (show_macro macro)) funcs ""


type parser_output = {
    procs : (name, proc) Hashtbl.t;
    macros : (name, macro) Hashtbl.t;
    vars : (name, typ) Hashtbl.t;
    mems : (name, (typ * size)) Hashtbl.t;
    strings : string ref;
}

(*
    Parse the preprocessed words into an AST.

    NOTE:
    Output doesn't directly contain any nodes,
    they are all inside functions since all that's allowed
    on the toplevel are global memory declarations and functions.
*)
let parse words =
    let output = {
        procs = Hashtbl.create 10;
        macros = Hashtbl.create 10;
        vars = Hashtbl.create 10;
        mems = Hashtbl.create 10;
        strings = ref "";
    } in

    let takes = Hashtbl.create 10 in

    let _add_string str =
        let strings = output.strings in
        try
            let re = Str.regexp_string str in
            Str.search_forward re !strings 0, String.length str
        with Not_found ->
            let addr = String.length !strings in
            strings := !strings ^ str ^ "\x00";
            addr, String.length str

    and add_var name typ =
        Hashtbl.add output.vars name typ

    and add_mem loc name typ size =
        let size =
            match size with
            | Word size -> (
                    let macro =
                        try Hashtbl.find output.macros size
                        with _ -> raise @@ Error (loc, "Unknown value")
                    in
                    match macro with
                    | { m_seq = [_, Literal Int size]; _ } -> size
                    | _ -> raise @@ Error (loc, "size has to be of constant value"))
            | Literal Int size -> size
            | _ -> raise @@ Error (loc, "usage: mem <name> <type> <size> end")
        in
        Hashtbl.add output.mems name (typ, size);
    in

    (* parse Polish notation starting from the root *)
    let rec parse_polish = function
        | [] -> Empty, []
        | (loc, word : location * prep) :: words ->
                match word with
                | Op op ->
                        let node1, words = parse_polish words in
                        let node2, words = parse_polish words in
                        Op (op, node1, node2), words
                | Literal p ->
                        Push_literal p, words
                | Word name when Hashtbl.mem output.macros name ->
                        let macro = Hashtbl.find output.macros name in
                        let nargs = (nargs macro.m_types) in
                        let args, words = parse_args loc name nargs words in
                        Macro (name, macro, args), words
                | Word name when Hashtbl.mem output.procs name ->
                        let proc = Hashtbl.find output.procs name in
                        let nargs = nargs proc.p_types in
                        let args, words = parse_args loc name nargs words in
                        Proc (name, proc, args), words
                | _ -> raise @@ Error (loc, "expected expression")

    (* parse function arguments *)
    and parse_args loc name nargs words =
        let rec parse' n acc = function
            (* all arguments parsed *)
            | words when n = 0 -> List.rev acc, words

            (* invalid arguments *)
            | [] -> raise @@ Error (loc, "Not enough arguments for function " ^ name)
            | (loc, Sep) :: _ -> raise @@ Error (loc, "Expected argument, got " ^ print_prep Sep)

            (* parse next argument *)
            | words ->
                    let node, words = parse_polish words in
                    parse' (n - 1) (node :: acc) words
        in
        parse' nargs [] words
    in

    (* parse function call *)
    let parse_proc_call loc name words =
        let proc = Hashtbl.find output.procs name in
        let nargs = nargs proc.p_types in
        let args, tl = parse_args loc name nargs words in
        Proc (name, proc, args), tl
    and parse_macro_call loc name words =
        let macro = Hashtbl.find output.macros name in
        let nargs = nargs macro.m_types in
        let args, tl = parse_args loc name nargs words in
        Macro (name, macro, args), tl
    in

    let extract_types loc words =
        let rec extract' input t_in t_out = function
            | (_, Return) :: words -> extract' false t_in t_out words
            | (_, Is) :: words -> List.rev t_in, List.rev t_out, words
            | (loc, (Type _ | Word _ as t)) :: words ->
                    if input then extract' input ((loc, t) :: t_in) t_out words
                    else extract' input t_in ((loc, t) :: t_out) words
            | (loc, word) :: _ -> raise @@ Error (loc,
                sprintf "Expected 'is' or type, got %s" (print_prep word))
            | [] -> raise @@ Error (loc, "expected 'is' after function declaration")
        in
        match (words : (location * prep) list) with
        | (_, Literal Int n_in) :: (_, Return) :: (_, Literal Int n_out) :: (_, Is) :: tl ->
                Numbered (n_in, n_out), tl
        | _ ->
                let t_in, t_out, words = extract' true [] [] words in
                Typed (t_in, t_out), words
    in

    (* add a macro to the table of macros *)
    let rec add_macro m_loc name words =
        let rec add' acc = function
            | [] -> raise @@ Error (m_loc, "'end' expected")
            | (loc, (Mem : prep)) :: _ -> raise @@ Error (loc, "cannot allocate global memory inside a function")
            | (_, End) :: words ->
                    List.rev acc, words
            | (_, Word _name) :: _ when name = _name ->
                    raise @@ Error (m_loc, "macros cannot be recursive")
            | word :: words ->
                    add' (word :: acc) words
        in
        let m_types, words = extract_types m_loc words in
        let m_seq, words = add' [] words in
        Hashtbl.replace output.macros name { m_loc; m_types; m_seq };
        words

    (* add a proc to the table of procs *)
    and add_proc p_loc name words =
        let rec check_recursion = function
            | [] ->
                    raise @@ Error (p_loc, "'end' expected")
            | (_, End) :: _ ->
                    false
            | (_, Word _name) :: _ when name = _name ->
                    true
            | _ :: words ->
                    check_recursion words
        in
        let p_types, words = extract_types p_loc words in
        let recursive = check_recursion words in
        let _, p_seq, words = parse_sequence [End] parse_next words in
        Hashtbl.replace output.procs name { p_loc; p_types; p_seq; recursive };
        words

    and parse_take loc words =
        let rec parse' acc = function
            | [] -> raise @@ Error (loc, "expected 'end'")
            | (_, End) :: words -> List.rev acc, words
            | (_, Word w) :: tl -> parse' (w :: acc) tl
            | (_, word) :: _ -> raise @@ Error (loc,
                "Expected name or 'end', got " ^ print_prep word)
        in
        parse' [] words

    and parse_sequence terminators f words =
        let rec parse' acc = function
            | [] when terminators = [] -> End, List.rev acc, []
            | (_, t') :: tl when List.mem t' terminators ->
                    t', List.rev acc, tl
            | words ->
                    let node, words = f words in
                    match words with
                            | (_, Sep) :: words -> parse' (node :: acc) words
                            | (loc, _) :: _ -> raise @@ Error (loc, "expected ';'")
                            | [] -> End, List.rev acc, []
        in
        parse' [] words

    and parse_indexing name words =
        let index, words = parse_polish words in
        Index_into (name, index), words

    and parse_assign_to_mem name words =
        let index, words = parse_polish words in
        let value, words = parse_polish words in
        Assign_to_mem (name, index, value), words

    and parse_assign_to_var name words =
        let value, words = parse_polish words in
        Assign_to_var (name, value), words

    and parse_if loc words =
        let condition, words = parse_polish words in
        let words =
            match words with
            | (_, Then) :: words -> words
            | _ -> raise @@ Error (loc, "expected 'then'")
        in
        let t, then_branch, words = parse_sequence [Else; End] parse_next words in
        let _, else_branch, words =
            match t with
            | End_if -> End, [], words
            | Else -> parse_sequence [End] parse_next words
            | _ -> raise @@ Error (loc, "expected 'else' or 'end'")
        in
        If_statement (condition, then_branch, else_branch), words

    and parse_while loc words =
        let condition, words = parse_polish words in
        let words =
            match words with
            | (_, Do) :: words -> words
            | _ -> raise @@ Error (loc, "expected 'then'")
        in
        let _, body, words = parse_sequence [] parse_next words in
        While_statement (condition, body), words

    and parse_syscall loc nargs id words =
        let args, words =
            parse_args loc (sprintf "syscall %d %d" nargs id) nargs words
        in
        Syscall (id, args), words

    and parse_op op words =
        let left, words = parse_polish words in
        let right, words = parse_polish words in
        Op (op, left, right), words

    and parse_next = function
        | [] -> Empty, []

        (* parse variable pushes *)
        | (_, Word name) :: tl when Hashtbl.mem takes name ->
                Push_take name, tl
        | (_, Word name) :: tl when Hashtbl.mem output.vars name ->
                Var (name, Hashtbl.find output.vars name), tl
        | (_, Word name) :: tl when Hashtbl.mem output.mems name ->
                Mem (name, Hashtbl.find output.mems name), tl

        (* parse function/macro calls *)
        | (loc, Word name) :: tl when Hashtbl.mem output.macros name ->
                parse_macro_call loc name tl
        | (loc, Word name) :: tl when Hashtbl.mem output.procs name ->
                parse_proc_call loc name tl

        (* ERROR -- unknown word *)
        | (loc, Word name) :: _ ->
                let vars = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) takes ""
                and mem  = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.mems ""
                and procs = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.procs ""
                and macros = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.macros "" in
                raise @@ Error (loc, 
                    sprintf "Unknown word: '%s'.\n" name ^
                    sprintf "\tavailable vars: %s\n" vars ^
                    sprintf "\tavailable mem: %s\n" mem ^
                    sprintf "\tavailable macros: %s\n" macros ^
                    sprintf "\tavailable procs: %s" procs)

        (* ERROR -- function definitions only allowed in toplevel *)
        | (loc, Macro) :: _ -> raise @@ Error (loc, "macro: expected name")
        | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc: expected name")

        (* ERROR -- global memory definitions only allowed in toplevel *)
        | (loc, Mem) :: _ -> raise @@ Error (loc, sprintf "usage: mem <name> is <type> <size> end")
        | (loc, Var) :: _ -> raise @@ Error (loc, sprintf "usage: var <name> is <type> end")


        | (loc, Take) :: tl ->
                let seq, tl = parse_take loc tl in
                Take seq, tl
        | (loc, Peek) :: tl ->
                let seq, tl = parse_take loc tl in
                Peek seq, tl

        (* ERROR -- unexpected separator ; *)
        | (loc, Sep) :: _ ->
                raise @@ Error (loc, "unexpected ';'")

        (* index into arrays to get the value or assign to it *)
        | (_, Index) :: (_, Word name) :: tl when Hashtbl.mem output.mems name ->
                parse_indexing name tl
        | (_, Assign) :: (_, Index) :: (_, Word name) :: tl when Hashtbl.mem output.mems name ->
                parse_assign_to_mem name tl
        | (_, Assign) :: (_, Word name) :: tl when Hashtbl.mem output.vars name ->
                parse_assign_to_var name tl

        (* ERROR -- unknown memory *)
        | (_, Index) :: (ln, Word name) :: _ ->
                let mem = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.mems "" in
                raise @@ Error (ln, sprintf "unknown mem: %s. available: %s" name mem)
        | (_, Assign) :: (_, Index) :: (ln, Word name) :: _ ->
                let mem = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.mems "" in
                raise @@ Error (ln, sprintf "unknown mem: %s. available: %s" name mem)

        (* ERROR -- unknown format *)
        | (loc, Index) :: _ ->
                raise @@ Error (loc, "expected [] <mem> <index>")
        | (loc, Assign) :: _ ->
                raise @@ Error (loc, "expected := <mem> <index> or := [] <mem> <index>")


        (* parse if statement *)
        | (loc, If) :: tl ->
                parse_if loc tl

        (* parse while statement *)
        | (loc, While) :: tl ->
                parse_while loc tl

        (* parse syscall *)
        | (loc, Syscall) :: (_, Literal Int nargs) :: (_, Literal Int id) :: tl ->
                parse_syscall loc nargs id tl

        (* ERROR -- syscall without nargs, id *)
        | (loc, Syscall) :: _ -> raise @@ Error (loc, "usage: syscall <nargs> <id>")

        (* parse operator *)
        | (_, Op op) :: tl ->
                parse_op op tl

        | (_, Literal l) :: tl ->
                Push_literal l, tl

        | (loc, word) :: _ ->
                raise @@ Error (loc, "unexpected word" ^ print_prep word)
    in

    let parse_toplevel words =
        let parse_tl' (words : (location * prep) list) =
            match words with
            (* parse vars and arrays, don't add anything to the AST *)
            | (_, Var) :: (_, Word name) :: (_, Is) :: (_, Type t) :: (_, End) :: tl ->
                    add_var name t;
                Empty, tl
            | (_, Mem) :: (_, Word name) :: (_, Is) :: (_, Type t) :: (loc, size) :: (_, End) :: tl ->
                    add_mem loc name t size;
                Empty, tl

            (* ERROR -- invalid var/mem format *)
            | (loc, Mem) :: _ -> raise @@ Error (loc, sprintf "usage: mem <name> is <type> <size> end")
            | (loc, Var) :: _ -> raise @@ Error (loc, sprintf "usage: var <name> is <type> end")


            (* parse functions -- macros ans procs, don't add anyting to the AST *)
            | (_, (Macro : prep)) :: (loc, Word name) :: tl ->
                    Empty, add_macro loc name tl
            | (_, Proc ) :: (loc, Word name) :: tl ->
                    Empty, add_proc loc name tl

            (* ERROR -- missing the name of the function *)
            | (loc, Macro) :: _ -> raise @@ Error (loc, "macro: expected name")
            | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc: expected name")

            | (_, word) :: _ -> failwith (show_prep word ^ "not yet implemented")
            | _ -> failwith "empty list"
        in
        parse_sequence [] parse_tl' words
        |> ignore
    in

    parse_toplevel words;
    output
