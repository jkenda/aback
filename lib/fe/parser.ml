open Common
open Lexer
open Parser_types
open Format

let show_parser_output = Parser_types.show_parser_output

let raise_unexpected_word loc expected actual =
    let expected_string =
        match expected with
        | [] -> raise @@ Unreachable "why are you calling this"
        | [word] -> sprintf "'%s'" (string_of_word word)
        | _ -> List.fold_left
            (fun acc word -> sprintf "%s or '%s'" acc (string_of_word word))
            (sprintf "'%s'" (string_of_word @@ List.hd expected))
            (List.tl expected)
    in
    raise @@ Error (loc, sprintf "expected %s, got '%s'" expected_string (string_of_word actual))

let raise_unreachable_eof = function
    | Some loc -> raise @@ Error (loc, "sequence not ended in EOF. this shouldn't happen")
    | None -> raise @@ Unreachable "sequence not ended in EOF. this shouldn't happen"

let raise_unexpected_eof loc =
    raise @@ Error (loc, "unexpected EOF")

(**
    Parse the preprocessed words into an AST.

    NOTE:
    Output doesn't directly contain any nodes,
    they are all inside functions since all that's allowed
    on the toplevel are global memory declarations and functions.
*)
let parse loc words =
    let current_proc = ref ""
    and func_callers = Hashtbl.create 10 in

    let output = {
        procs   = Hashtbl.create 10;
        macros  = Hashtbl.create 10;
        vars    = Hashtbl.create 10;
        mems    = Hashtbl.create 10;
        strings = "";
        typs    = primitives
    } in

    Hashtbl.add output.typs "str" Str;
    Hashtbl.add output.typs "cstr" CStr;

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
    and has_take take =
        Hashtbl.mem takes take
    in

    let raise_unknown_word loc word =
        let vars   = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) takes ""
        and mem    = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.mems ""
        and procs  = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.procs ""
        and macros = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.macros "" in
        raise @@ Error (loc, 
            sprintf "Unknown word: '%s'.\n"     word ^
            sprintf "\tavailable vars: %s\n"    vars ^
            sprintf "\tavailable mem: %s\n"     mem ^
            sprintf "\tavailable macros: %s\n"  macros ^
            sprintf "\tavailable procs: %s"     procs)
    in

    let add_func table loc name types seq =
        let is_signature = false and is_unused = false in
        Hashtbl.replace table name { loc; name; types; seq; is_signature; is_unused };
    and add_signature table loc name types =
        let seq = [] and is_signature = true and is_unused = false in
        Hashtbl.replace table name { loc; name; types; seq; is_signature; is_unused };
    in

    let make_proc_call loc func args =
        Hashtbl.add func_callers func.name !current_proc;
        let node = make_node loc @@ Proc_call { func; args } in
        node.t <- Some func.types.t_out;
        node
    in


    (* add string literal to strings and return its offset and length *)
    let parse_literal loc (data : data_tok) =
        let data =
            match data with
            | String str ->
                    let off = String.length output.strings
                    and len = String.length str in
                    output.strings <- output.strings ^ str;
                    Const_str (str, off, len)
            | CString str ->
                    let off = String.length output.strings in
                    output.strings <- output.strings ^ str ^ "\x00";
                    Const_cstr (str, off)
            | _ ->
                    data_lit_of_data_tok data

        in
        let (data : data_hl) = Literal data in
        let node = make_node loc @@ Push_data { data } in
        node.t <- Some [type_of_data_hl data];
        node
    in

    (** parse multiple subsequent words into a list of types *)
    let parse_types loc terminators words =
        let rec parse_typ' (acc : type_hl list) = function
            | [] -> raise_unreachable_eof @@ Some loc
            | (loc, EOF) :: _ -> raise_unexpected_eof loc

            | (_, w) :: tl when Array.mem w terminators -> w, List.rev acc, tl

            | (loc, Type Ptr) :: tl ->
                    (try parse_typ' (Ptr (List.hd acc) :: List.tl acc) tl
                    with _ -> raise @@ Error (loc, "trying to create a ptr to unknown type"))
            | (_, Type t) :: tl -> parse_typ' ((type_hl_of_type_tok t) :: acc) tl
            | (_, Word w) :: tl when Hashtbl.mem output.typs w ->
                    parse_typ' (Hashtbl.find output.typs w :: acc) tl
            | (_, Word w) :: tl ->
                    parse_typ' ((Generic w) :: acc) tl

            | (loc, word) :: _ ->
                    let expected_words = Array.to_list terminators in
                    raise_unexpected_word loc expected_words word
        in
        parse_typ' [] words
    in

    (** parse a list of words into one single type *)
    let parse_type terminators loc words =
        let _, types, words = parse_types loc terminators words in
        match types with
            | [t] -> t, words
            | [] -> raise @@ Error (loc, "usage: var <name> is <type> end")
            | _ -> raise @@ Error (loc, sprintf "expected one type, got %s" (string_of_types_hl types))
    in

    (** add a variable to the table *)
    let add_var loc name words =
        let typ, words = parse_type [|End|] loc words in
        Hashtbl.add output.vars name typ;
        make_node loc @@ Empty, words

    (** add array to the table *)
    and add_mem loc name words =
        let size, words =
            match words with
            | (_, Word size) :: tl -> (
                    let macro =
                        try Hashtbl.find output.macros size
                        with _ -> raise @@ Error (loc, "Unknown value")
                    in
                    match macro with
                    | { seq = [{ n = Push_data { data = Literal Integer size; _ }; _ }]; _ } -> size, tl
                    | _ -> raise @@ Error (loc, "size has to be of constant value"))
            | (_, Literal Integer size) :: tl -> size, tl
            | (_, w) :: _ -> raise @@ Error (loc, sprintf "expected size, got '%s'\nusage: mem <name> <type> <size> end" (string_of_word w))
            | _ -> raise @@ Error (loc, "usage: mem <name> <type> <size> end")
        in
        let typ, words = parse_type [|End|] loc words in
        Hashtbl.add output.mems name (typ, size);
        make_node loc @@ Empty, words
    in

    (** parse Polish notation starting from the root *)
    let rec parse_polish words =
        match words with
        | [] -> make_node loc @@ Empty, []
        | (loc, word : location * word) :: rest ->
                match word with
                | Literal data ->
                        parse_literal loc data, rest
                | Word name when Hashtbl.mem takes name ->
                        Hashtbl.find takes name;
                        make_node loc @@ Push_take { name; type_hl = None }, rest
                | Op op ->
                        let left , rest = parse_polish rest in
                        let right, rest = parse_polish rest in
                        make_node loc @@ Op { op; left; right }, rest
                | Word name when Hashtbl.mem output.macros name ->
                        let func = Hashtbl.find output.macros name in
                        let args, tl = parse_args func.types.t_in rest in
                        make_macro_call loc func args, tl
                | Word name when Hashtbl.mem output.procs name ->
                        let proc = Hashtbl.find output.procs name in
                        let args, tl = parse_args proc.types.t_in rest in
                        make_proc_call loc proc args, tl
                | Word name when Hashtbl.mem output.vars name ->
                        let type_hl = Hashtbl.find output.vars name in
                        let node = make_node loc @@ Push_var { name } in
                        node.t <- Some [type_hl];
                        node, rest
                | Word name when Hashtbl.mem output.mems name ->
                        let type_hl, _ = Hashtbl.find output.mems name in
                        let node = make_node loc @@ Push_mem { name } in
                        node.t <- Some [type_hl];
                        node, rest
                | Word word ->
                        raise_unknown_word loc word
                | End | Sep ->
                        make_node loc @@ Empty, words

                | word -> raise @@ Error (loc, sprintf "expected expression, got '%s'" (string_of_word word))

    (** parse function arguments *)
    and parse_args types words =
        let rec parse' acc = function
            (* all arguments parsed *)
            | [], words -> List.rev acc, words

            (* invalid arguments *)
            | types, ((loc, Sep) :: _ as words)
            | types, (loc, Dot_dot) :: words ->
                    let node = make_node loc @@ Unknown_sequence { types } in
                    List.rev (node :: acc), words

            (* parse next argument *)
            | _t_hl :: types, words ->
                    let node, rest = parse_polish words in
                    parse' (node :: acc) (types, rest)
        in
        parse' [] (types, words)
    in

    (** parse function call *)
    let parse_func_call f table loc name words =
        let func = Hashtbl.find table name in
        let args, rest = parse_args func.types.t_in words in
        f loc func args, rest
    in

    (** get input and output types of function *)
    let extract_types loc words =
        let _, t_in , rest = parse_types loc [|Return|] words in
        let term, t_out, rest = parse_types loc [|Is; End|] rest in
        term, { t_in; t_out }, rest
    in

    (** add a proc to the table of procs *)
    let rec parse_func loc table name words =
        let term, types, words = extract_types loc words in
        if term = End then
            (add_signature table loc name types;
            words)
        else
            (current_proc := name;
            let _, seq, words = parse_scope [|End|] [|Sep|] parse_next words in
            add_func table loc name types seq;
            words)

    (** parse sequence of statements *)
    and parse_scope terminators separators f words =
        scope_entry ();

        let rec parse' (top, rest) = function
            | (_, word) :: tl when Array.mem word terminators ->
                    word, top :: rest, tl

            | [] -> raise_unreachable_eof None
            | [loc, EOF] -> raise_unexpected_eof loc

            | words ->
                    let node, words = f words in

                    match words with
                    | _ when separators = [||] ->
                            parse' (top, rest) words

                    | [] -> raise_unreachable_eof None
                    | [loc, EOF] -> raise_unexpected_eof loc

                    | (_, word) :: tl when Array.mem word separators ->
                            parse' ([], (node :: top) :: rest) tl
                    | _ ->
                            parse' (node :: top, rest) words

        in

        let terminator, nodes, rest = parse' ([], []) words in

        scope_exit ();

        let nodes =
            nodes
            |> List.rev
            |> List.flatten
        in
        terminator, nodes, rest

    (** parse array indexing *)
    and parse_indexing loc name words =
        let index, rest = parse_polish words in
        make_node loc @@ Push_member { name; index }, rest

    (** parse array element assignment *)
    and parse_assign_to_mem loc name words =
        let index, rest = parse_polish words in
        let value, rest = parse_polish rest in
        make_node loc @@ Assign_to_mem { name; index; value }, rest

    (** parse variable assignment *)
    and parse_assign_to_var loc name words =
        let value, rest = parse_polish words in
        make_node loc @@ Assign_to_var { name; value }, rest

    (** parse next statement/expression *)
    and parse_next words =

        (** parse if statement *)
        let parse_if loc words =
            let cond, rest = parse_polish words in
            let rest =
                match rest with
                | [] -> raise_unreachable_eof None
                | [loc, EOF] -> raise_unexpected_eof loc

                | (_, Then) :: tl -> tl
                | (loc, word) :: _ -> raise_unexpected_word loc [Then] word
            in
            let t, true_branch, rest = parse_scope [|Else; End|] [|Sep|] parse_next rest in
            let _, false_branch, rest =
                match t with
                | End -> End, [], rest
                | Else -> parse_scope [|End|] [|Sep|] parse_next rest
                | word -> raise_unexpected_word loc [Else; End] word
            in
            make_node loc @@ If_statement { cond; true_branch; false_branch }, rest

        (** parse while statement *)
        and parse_while loc words =
            let cond, rest = parse_polish words in
            let rest =
                match rest with
                | [] -> raise_unreachable_eof @@ Some loc
                | [loc, EOF] -> raise_unexpected_eof loc

                | (_, Do) :: words -> words
                | (loc, word) :: _ -> raise_unexpected_word loc [Do] word
            in
            let _, body, rest = parse_scope [|End|] [|Sep|] parse_next rest in
            make_node loc @@ While_statement { cond; body }, rest

        (** parse 'take' and 'peek' *)
        and parse_take (word : word) loc words =
            let rec parse' acc = function
                | [] -> raise_unreachable_eof @@ Some loc
                | [loc, EOF] -> raise_unexpected_eof loc

                | (_, In) :: tl ->
                        let vars =
                            acc
                            |> List.rev
                            |> List.map snd
                        in

                        scope_entry ();
                        List.iter add_take vars;
                        let _, body, rest = parse_scope [|End|] [|Sep|] parse_next tl in
                        scope_exit ();

                        if word = Take then
                            make_node loc @@ Scoped_take {vars; body }, rest
                        else
                            make_node loc @@ Scoped_peek {vars; body }, rest
                | (_, End) :: tl ->
                        let vars =
                            acc
                            |> List.rev
                            |> List.map snd
                        in
                        List.iter add_take vars;

                        if word = Take then
                            make_node loc @@ Take { vars }, tl
                        else
                            make_node loc @@ Peek { vars }, tl

                | (loc, Word w) :: tl -> parse' ((loc, w) :: acc) tl
                | (loc, word) :: _ -> raise_unexpected_word loc [Is; In; End] word
            in
            parse' [] words
        in

        match words with
        | [] -> make_node loc @@ Empty, []

        (* parse variable pushes *)
        | (loc, Word name) :: tl when has_take name ->
                make_node loc @@ Push_take { name; type_hl = None }, tl
        | (loc, Word name) :: tl when Hashtbl.mem output.vars name ->
                let typ = Hashtbl.find output.vars name in
                make_node loc @@ Var { name; typ }, tl
        | (loc, Word name) :: tl when Hashtbl.mem output.mems name ->
                let typ, size = Hashtbl.find output.mems name in
                make_node loc @@ Mem { name; typ; size }, tl

        (* parse function/macro call *)
        | (loc, Word name) :: tl when Hashtbl.mem output.macros name ->
                parse_func_call make_macro_call output.macros loc name tl
        | (loc, Word name) :: tl when Hashtbl.mem output.procs name ->
                parse_func_call make_proc_call output.procs loc name tl

        (* parse operator *)
        | (_, Op _) :: _ ->
                parse_polish words

        (* ERROR -- unknown word *)
        | (loc, Word word) :: _ ->
                raise_unknown_word loc word

        (* ERROR -- function definitions only allowed in toplevel *)
        | (loc, Macro) :: _ -> raise @@ Error (loc, "macro definitions only allowed in toplevel")
        | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc definitions only allowed in toplevel")

        (* ERROR -- global memory definitions only allowed in toplevel *)
        | (loc, Mem) :: _ -> raise @@ Error (loc, sprintf "global memory definitions only allowed in toplevel")
        | (loc, Var) :: _ -> raise @@ Error (loc, sprintf "global variable definitions only allowed in toplevel")


        | (loc, Take) :: tl ->
                parse_take Take loc tl
        | (loc, Peek) :: tl ->
                parse_take Peek loc tl

        (* ERROR -- unexpected separator ; *)
        | (loc, Sep) :: _ ->
                raise @@ Error (loc, sprintf "unexpected '%s'" (string_of_word Sep))

        (* index into arrays to get the value or assign to it *)
        | (loc, Index) :: (_, Word name) :: tl when Hashtbl.mem output.mems name ->
                parse_indexing loc name tl
        | (loc, Assign) :: (_, Index) :: (_, Word name) :: tl when Hashtbl.mem output.mems name ->
                parse_assign_to_mem loc name tl
        | (loc, Assign) :: (_, Word name) :: tl when Hashtbl.mem output.vars name ->
                parse_assign_to_var loc name tl

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

        (* parse literal *)
        | (loc, Literal data) :: tl ->
                parse_literal loc data, tl

        | (loc, word) :: _ ->
                raise @@ Error (loc, sprintf "%s: word not allowed at the toplevel" (string_of_word word))
    in

    (** parse top-level program constructs -- global memory and functions *)
    let parse_toplevel words =
        let parse_tl' (words : (location * word) list) =
            match words with

            (* parse vars and arrays, don't add anything to the AST *)
            | (loc, Var) :: (_, Word name) :: (_, Is) :: tl ->
                    add_var loc name tl
            | (loc, Mem) :: (_, Word name) :: (_, Is) :: tl ->
                    add_mem loc name tl

            (* ERROR -- invalid var/mem format *)
            | (loc, Var) :: _ -> raise @@ Error (loc, "usage: var <name> is <type> end")
            | (loc, Mem) :: _ -> raise @@ Error (loc, "usage: mem <name> is <type> <size> end")


            (* parse functions -- macros ans procs, don't add anyting to the AST *)
            | (_, (Macro : word)) :: (loc, Word name) :: tl ->
                    make_node loc @@ Empty, parse_func loc output.macros name tl
            | (_, Proc ) :: (loc, Word name) :: tl ->
                    make_node loc @@ Empty, parse_func loc output.procs name tl

            (* ERROR -- missing the name of the function *)
            | (loc, Macro) :: _ -> raise @@ Error (loc, "macro: expected name")
            | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc: expected name")

            | (loc, word) :: _ -> raise @@ Error (loc, string_of_word word ^ " not allowed in the toplevel")
            | _ -> raise @@ Unreachable "empty list in parse_toplevel"
        in
        parse_scope [|EOF|] [||] parse_tl' words
        |> ignore
    in

    let mark_unused _ func =
        let rec is_called_from_main func_name =
            let callers = Hashtbl.find_all func_callers func_name in
            if List.mem "main" callers then
                true
            else
                List.exists is_called_from_main callers
        in
        if (not @@ is_called_from_main func.name) && func.name <> "main" then
            func.is_unused <- true
    in

    parse_toplevel words;
    Hashtbl.iter mark_unused output.procs;

    output


(*
   TEST
*)

let null_loc = {
    filename = "[test]";
    included_from = [];
    expanded_from = [];
    row = 1;
    col = 1
}

let test_vars input expected =
    let parsed = parse null_loc input in
    let matches = parsed.vars = expected.vars in
    if not matches then
        begin
            print_endline "expected:";
            Hashtbl.iter (fun key data ->
                printf "%s: %s\n" key (show_type_hl data)) expected.vars;

            print_endline "\nactual";
            Hashtbl.iter (fun key data ->
                printf "%s: %s\n" key (show_type_hl data)) parsed.vars;
        end;
    matches

let hashtbl_of_list l =
    Hashtbl.of_seq @@ List.to_seq l

let test_loc = {
    filename = "[test]";
    included_from = [];
    expanded_from = [];
    row = 1; col = 1
}

let%test "toplevel" =
    let input : word list =
        [
            Var; Word "a"; Is; Type I8; End;
            Var; Word "b"; Is; Type I16; End;
            Var; Word "c"; Is; Type I32; End;
            Var; Word "d"; Is; Type I64; End;
            Var; Word "e"; Is; Type U8; End;
            Var; Word "f"; Is; Type U16; End;
            Var; Word "g"; Is; Type U32; End;
            Var; Word "h"; Is; Type U64; End;
            Var; Word "i"; Is; Type F32; End;
            Var; Word "j"; Is; Type F64; End;
            Var; Word "k"; Is; Type Bool; End;
            EOF
        ]
    in
    let input = List.map (fun prep -> (test_loc, prep)) input 
    and expected = {
        procs   = hashtbl_of_list [];
        macros  = hashtbl_of_list [];
        mems    = hashtbl_of_list [];
        vars    = hashtbl_of_list [
            "a", (Primitive I8 : type_hl);
            "b", (Primitive I16 : type_hl);
            "c", (Primitive I32 : type_hl);
            "d", (Primitive I64 : type_hl);
            "e", (Primitive U8 : type_hl);
            "f", (Primitive U16 : type_hl);
            "g", (Primitive U32 : type_hl);
            "h", (Primitive U64 : type_hl);
            "i", (Primitive F32 : type_hl);
            "j", (Primitive F64 : type_hl);
            "k", (Primitive Bool : type_hl);
        ];
        strings = "";
        typs    = primitives
    } in
    test_vars input expected

let%expect_test "mem" =
    [(Mem : word); Word "x"; Is; Type I32; End; EOF]
    |> List.map (fun prep -> (test_loc, prep))
    |> parse null_loc
    |> ignore;
    [%expect {||}]

