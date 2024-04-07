open Format

open Lexer
open Preprocess

open Parser_types

(**
    Parse the preprocessed words into an AST.

    NOTE:
    Output doesn't directly contain any nodes,
    they are all inside functions since all that's allowed
    on the toplevel are global memory declarations and functions.
*)
let parse words =
    let output = {
        procs   = Hashtbl.create 10;
        macros  = Hashtbl.create 10;
        vars    = Hashtbl.create 10;
        mems    = Hashtbl.create 10;
        typs    = primitives
    } in

    (* take care of the scope of takes *)
    let takes = Hashtbl.create 10
    and scopes_of_takes = ref [] in
    let scope_entry () =
        scopes_of_takes := Hashtbl.create 10 :: !scopes_of_takes
    and scope_exit () =
        Hashtbl.iter
            (fun take () -> Hashtbl.remove takes take)
            (List.hd !scopes_of_takes);
        scopes_of_takes := List.tl !scopes_of_takes
    and add_take take =
        Hashtbl.replace takes take ();
        Hashtbl.replace (List.hd !scopes_of_takes) take ()
    in

    (** parse multiple subsequent words into a list of types *)
    let parse_typs loc terminator words =
        let rec parse_typ' acc = function
            | (_, w) :: tl when w = terminator -> List.rev acc, tl

            | (loc, Type Ptr) :: tl ->
                    (try parse_typ' (Ptr (List.hd acc) :: List.tl acc) tl
                    with _ -> raise @@ Error (loc, "trying to create a ptr to unknown type"))
            | (_, Type t) :: tl -> parse_typ' (Primitive t :: acc) tl
            | (_, Word w) :: tl when Hashtbl.mem output.typs w ->
                    parse_typ' (Hashtbl.find output.typs w :: acc) tl
            | (_, Word w) :: _ -> raise @@ Error (loc, "unknown type: " ^ w)

            | (loc, _) :: _ -> raise @@ Error (loc, "unexpected word")
            | [] -> raise @@ Error (loc, "unexpected EOF")
        in
        parse_typ' [] words
    in

    (** parse a list of words into one single type *)
    let parse_typ loc words =
        let typs, words = parse_typs loc End words in
        match typs with
            | [t] -> t, words
            | [] -> raise @@ Error (loc, "usage: var <name> is <type> end")
            | _ -> raise @@ Error (loc, "only one type allowed")
    in

    (** add a variable to the table *)
    let add_var loc name words =
        let typ, words = parse_typ loc words in
        Hashtbl.add output.vars name typ;
        Empty, words

    (** add array to the table *)
    and add_mem loc name size words =
        let typ, words = parse_typ loc words in
        let size =
            match size with
            | Word size -> (
                    let macro =
                        try Hashtbl.find output.macros size
                        with _ -> raise @@ Error (loc, "Unknown value")
                    in
                    match macro with
                    | { seq = [Push_literal Int size]; _ } -> size
                    | _ -> raise @@ Error (loc, "size has to be of constant value"))
            | Literal Int size -> size
            | _ -> raise @@ Error (loc, "usage: mem <name> <type> <size> end")
        in
        Hashtbl.add output.mems name (typ, size);
        Empty, words
    in

    (** parse Polish notation starting from the root *)
    let rec parse_polish words =
        match words with
        | [] -> Empty, []
        | (loc, word : location * prep) :: rest ->
                match word with
                | Literal p ->
                        Push_literal p, rest
                | Op op ->
                        let node1, rest = parse_polish rest in
                        let node2, rest = parse_polish rest in
                        Op (op, node1, node2), rest
                | Word name when Hashtbl.mem output.macros name ->
                        let macro = Hashtbl.find output.macros name in
                        let nargs = nargs In macro.types in
                        let args, tl = parse_args loc name nargs rest in
                        Macro (name, macro, args), tl
                | Word name when Hashtbl.mem output.procs name ->
                        let proc = Hashtbl.find output.procs name in
                        let nargs = nargs In proc.types in
                        let args, tl = parse_args loc name nargs rest in
                        Proc (name, proc, args), tl
                | End ->
                        Empty, words

                | _ -> raise @@ Error (loc, "expected expression")

    (** parse function arguments *)
    and parse_args loc name n words =
        let rec parse' n acc = function
            (* all arguments parsed *)
            | words when n = 0 -> List.rev acc, words

            (* invalid arguments *)
            | _ when n < 0 -> raise @@ Error (loc, "Too many arguments for function " ^ name)
            | [] -> raise @@ Error (loc, "Not enough arguments for function " ^ name)
            | (loc, Sep) :: _ -> raise @@ Error (loc, "Expected argument, got " ^ print_prep Sep)

            (* parse next argument *)
            | words ->
                    let node, rest = parse_polish words in
                    match node with
                    | Empty -> List.rev (Unknown_sequence n :: acc), rest
                    | Proc (_, func, _) | Macro (_, func, _) ->
                            let nargs = nargs In func.types in
                            parse' (n - nargs) (node :: acc) rest
                    | _ ->
                            parse' (n - 1) (node :: acc) rest
        in
        parse' n [] words
    in

    (** parse function call *)
    let parse_proc_call loc name words =
        let proc = Hashtbl.find output.procs name in
        let nargs = nargs In proc.types in
        let args, rest = parse_args loc name nargs words in
        Proc (name, proc, args), rest
    and parse_macro_call loc name words =
        let macro = Hashtbl.find output.macros name in
        let nargs = nargs In macro.types in
        let args, rest = parse_args loc name nargs words in
        Macro (name, macro, args), rest
    in

    (** get input and output types of function *)
    let extract_types loc words =
        match (words : (location * prep) list) with
        | (_, Literal Int n_in) :: (_, Return) :: (_, Literal Int n_out) :: (_, Is) :: tl ->
                Numbered (n_in, n_out), tl
        | _ ->
                let t_in , rest = parse_typs loc Return words in
                let t_out, rest = parse_typs loc Is rest in
                Typed (t_in, t_out), rest
    in

    (** add a proc to the table of procs *)
    let rec add_func loc table name words =
        let types, words = extract_types loc words in
        let recursive = check_recursion loc name words in
        let _, seq, words = parse_sequence [|End|] parse_next words in
        Hashtbl.replace table name { loc; types; seq; recursive; ncalls = ref 0 };
        words

    (** parse sequence of statements *)
    and parse_sequence terminators f words =
        scope_entry ();

        let rec parse' acc = function
            | [] when terminators = [||] -> End, List.rev acc, []
            | (_, t') :: tl when Array.mem t' terminators ->
                    t', List.rev acc, tl
            | words ->
                    let node, rest = f words in

                    (* make sure sequence is delimited by ';' *)
                    match rest with
                            | [] -> End, List.rev acc, []
                            | (_, Sep) :: tl -> parse' (node :: acc) tl
                            | (loc, _) :: _ -> raise @@ Error (loc, "expected ';'")
        in

        let ret = parse' [] words in

        scope_exit ();
        ret

    (** parse array indexing *)
    and parse_indexing name words =
        let index, words = parse_polish words in
        Index_into (name, index), words

    (** parse array element assignment *)
    and parse_assign_to_mem name words =
        let index, words = parse_polish words in
        let value, words = parse_polish words in
        Assign_to_mem (name, index, value), words

    (** parse variable assignment *)
    and parse_assign_to_var name words =
        let value, words = parse_polish words in
        Assign_to_var (name, value), words

    (** parse syscall *)
    and parse_syscall loc nargs id words =
        let args, words =
            parse_args loc (sprintf "syscall %d %d" nargs id) nargs words
        in
        Syscall (id, args), words

    (** parse operator *)
    and parse_op op words =
        let l, words = parse_polish words in
        let r, words = parse_polish words in
        Op (op, l, r), words

    (** parse next statement/expression *)
    and parse_next words =

        (** parse if statement *)
        let parse_if loc words =
            let condition, rest = parse_polish words in
            let rest =
                match rest with
                | (_, Then) :: tl -> tl
                | _ -> raise @@ Error (loc, "expected 'then'")
            in
            let t, then_branch, rest = parse_sequence [|Else; End|] parse_next rest in
            let _, else_branch, rest =
                match t with
                | End -> End, [], rest
                | Else -> parse_sequence [|End|] parse_next rest
                | _ -> raise @@ Error (loc, "expected 'else' or 'end'")
            in
            If_statement (condition, then_branch, else_branch), rest

        (** parse while statement *)
        and parse_while loc words =
            let condition, rest = parse_polish words in
            let rest =
                match rest with
                | (_, Do) :: words -> words
                | _ -> raise @@ Error (loc, "expected 'then'")
            in
            let _, body, rest = parse_sequence [||] parse_next rest in
            While_statement (condition, body), rest

        (** parse 'take' and 'peek' *)
        and parse_take loc words =
            let rec parse' acc = function
                | (_, End) :: tl -> List.rev acc, tl
                | (loc, Word w) :: tl -> parse' ((loc, w) :: acc) tl
                | (_, word) :: _ -> raise @@ Error (loc,
                    "Expected name or 'end', got " ^ print_prep word)
                | [] -> raise @@ Error (loc, "expected 'end'")
            in
            let names, rest = parse' [] words in
            List.iter (fun (_, t) -> add_take t) names;
            List.map snd names, rest

        in

        match words with
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

    (** parse top-level program constructs -- global memory and functions *)
    let parse_toplevel words =
        let parse_tl' (words : (location * prep) list) =
            match words with

            (* parse vars and arrays, don't add anything to the AST *)
            | (_, Var) :: (_, Word name) :: (loc, Is) :: tl ->
                    add_var loc name tl
            | (_, Mem) :: (_, Word name) :: (_, Is) :: (loc, (Literal Int _ | Word _ as size)) :: tl ->
                    add_mem loc name size tl

            (* ERROR -- invalid var/mem format *)
            | (loc, Mem) :: _ -> raise @@ Error (loc, sprintf "usage: mem <name> is <type> <size> end")
            | (loc, Var) :: _ -> raise @@ Error (loc, sprintf "usage: var <name> is <type> end")


            (* parse functions -- macros ans procs, don't add anyting to the AST *)
            | (_, (Macro : prep)) :: (loc, Word name) :: tl ->
                    Empty, add_func loc output.macros name tl
            | (_, Proc ) :: (loc, Word name) :: tl ->
                    Empty, add_func loc output.procs name tl

            (* ERROR -- missing the name of the function *)
            | (loc, Macro) :: _ -> raise @@ Error (loc, "macro: expected name")
            | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc: expected name")

            | (loc, word) :: _ -> raise @@ Error (loc, show_prep word ^ " not allowed in the toplevel")
            | _ -> raise @@ Unreachable "empty list in parse_toplevel"
        in
        parse_sequence [||] parse_tl' words
        |> ignore
    in

    parse_toplevel words;
    output


(*
   TEST
*)

let test input expected =
    let parsed = parse input in
    let matches = parsed = expected in
    if matches then
        print_endline "OK"
    else
        print_endline (Format.asprintf "%s\n!=\n%s" (show_parser_output expected) (show_parser_output parsed));
    matches

let hashtbl_of_list l =
    Hashtbl.of_seq @@ List.to_seq l

let test_loc = {
    filename = "[test]";
    included_from = [];
    expanded_from = [];
    row = 1; col = 1
}

let%test _ =
    let input : prep list =
        [Var; Word "x"; Is; Type Int; End]
    in
    let input = List.map (fun prep -> (test_loc, prep)) input 
    and expected = {
        procs   = hashtbl_of_list [];
        macros  = hashtbl_of_list [];
        mems    = hashtbl_of_list [];
        vars    = hashtbl_of_list [
            "x", (Primitive Int : typ)
        ];
        typs    = primitives
    } in
    test input expected

let%expect_test _ =
    [(Mem : prep); Word "x"; Is; Type Int; End]
    |> List.map (fun prep -> (test_loc, prep))
    |> parse
    |> ignore;
    [%expect {||}]

