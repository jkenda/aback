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
    let output = {
        procs   = Hashtbl.create 10;
        macros  = Hashtbl.create 10;
        vars    = Hashtbl.create 10;
        mems    = Hashtbl.create 10;
        strings = "";
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
    and has_take take =
        Hashtbl.mem takes take
    in

    (* add string literal to strings and return its offset and length *)
    let parse_literal l (data : data_tok) =
        let data =
            match data with
            | String str | CStr str ->
                    let addr = String.length output.strings in
                    output.strings <- output.strings ^ str ^ "\x00";
                     Const_str addr
            | _ ->
                    data_lit_of_data_tok data

        in
        let (data : data_hl) = Literal data in
        { l; t = Some (type_of_data_hl data); n = Push_data { data }}
    in

    (** parse multiple subsequent words into a list of types *)
    let parse_typs loc terminator words =
        let rec parse_typ' (acc : type_hl list) = function
            | (_, w) :: tl when w = terminator -> List.rev acc, tl

            | (loc, Type Ptr) :: tl ->
                    (try parse_typ' (Ptr (List.hd acc) :: List.tl acc) tl
                    with _ -> raise @@ Error (loc, "trying to create a ptr to unknown type"))
            | (_, Type t) :: tl -> parse_typ' ((type_hl_of_type_tok t) :: acc) tl
            | (_, Word w) :: tl when Hashtbl.mem output.typs w ->
                    parse_typ' (Hashtbl.find output.typs w :: acc) tl
            | (_, Word w) :: tl ->
                    parse_typ' ((Generic w) :: acc) tl

            | (loc, word) :: _ -> raise @@ Error (loc, sprintf "unexpected word: %s. expected %s" (string_of_word word) (string_of_word terminator))
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
        make_node loc @@ Empty, words

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
                    | { seq = [{ n = Push_data { data = Literal Integer size; _ }; _ }]; _ } -> size
                    | _ -> raise @@ Error (loc, "size has to be of constant value"))
            | Literal Integer size -> size
            | _ -> raise @@ Error (loc, "usage: mem <name> <type> <size> end")
        in
        Hashtbl.add output.mems name (typ, size);
        make_node loc @@ Empty, words
    in

    (** parse Polish notation starting from the root *)
    let rec parse_polish words =
        match words with
        | [] -> { l = loc; t = None; n = Empty }, []
        | (loc, word : location * word) :: rest ->
                match word with
                | Literal data ->
                        parse_literal loc data, rest
                | Op op ->
                        let left , rest = parse_polish rest in
                        let right, rest = parse_polish rest in
                        make_node loc @@ Op { op; left; right }, rest
                | Word name when Hashtbl.mem output.macros name ->
                        let func = Hashtbl.find output.macros name in
                        let nargs = List.length func.types.t_in in
                        let args, tl = parse_args loc name nargs rest in
                        make_node loc @@ Macro_call { func; args }, tl
                | Word name when Hashtbl.mem output.procs name ->
                        let func = Hashtbl.find output.procs name in
                        let nargs = List.length func.types.t_in in
                        let args, tl = parse_args loc name nargs rest in
                        make_node loc @@ Proc_call { func; args }, tl
                | End | Sep ->
                        make_node loc @@ Empty, words

                | _ -> raise @@ Error (loc, "expected expression")

    (** parse function arguments *)
    and parse_args loc name n words =
        let rec parse' n acc = function
            (* all arguments parsed *)
            | words when n = 0 -> List.rev acc, words

            (* invalid arguments *)
            | _ when n < 0 -> raise @@ Error (loc, "Too many arguments for function " ^ name)
            | [] -> raise @@ Error (loc, "Not enough arguments for function " ^ name)
            | (loc, Sep) :: _ -> raise @@ Error (loc, "Expected argument, got " ^ string_of_word Sep)

            (* parse next argument *)
            | (l, _) :: _ as words ->
                    let node, rest = parse_polish words in
                    match node.n with
                    | Empty -> List.rev ({ l; t = None; n = Unknown_sequence { length = n }} :: acc), rest
                    | Proc_call { func; _ } | Macro_call { func; _ } ->
                            let nargs = List.length func.types.t_in in
                            parse' (n - nargs) (node :: acc) rest
                    | _ ->
                            parse' (n - 1) (node :: acc) rest
        in
        parse' n [] words
    in

    (** parse function call *)
    let parse_func_call f loc name words =
        let proc = Hashtbl.find output.procs name in
        let nargs = List.length proc.types.t_in in
        let args, rest = parse_args loc name nargs words in
        f (loc, proc, args), rest
    in

    (** get input and output types of function *)
    let extract_types loc words =
        let t_in , rest = parse_typs loc Return words in
        let t_out, rest = parse_typs loc Is rest in
        { t_in; t_out }, rest
    in

    (** add a proc to the table of procs *)
    let rec add_func loc table name words =
        let types, words = extract_types loc words in
        let _, seq, rest = parse_scope [|End|] [|Sep|] parse_next words in
        Hashtbl.replace table name { loc; name; types; seq; ncalls = ref 0 };
        rest

    (** parse sequence of statements *)
    and parse_scope terminators separators f words =
        scope_entry ();

        let rec parse' acc = function
            | [] -> raise_unreachable_eof None
            | [loc, EOF] -> raise_unexpected_eof loc

            | (_, word) :: tl when Array.mem word terminators ->
                    word, List.rev acc, tl

            | words ->
                    let node, words = f words in

                    match words with
                    | [] -> raise_unreachable_eof None
                    | [loc, EOF] -> raise_unexpected_eof loc

                    | (_, word) :: _ when separators = [||] || Array.mem word terminators ->
                            parse' (node :: acc) words
                    | (_, word) :: tl when Array.mem word separators ->
                            parse' (node :: acc) tl
                    | (loc, word) :: _ ->
                            let terminators = Array.to_list terminators in
                            raise_unexpected_word loc terminators word

        in

        let ret = parse' [] words in

        scope_exit ();
        ret

    (** parse array indexing *)
    and parse_indexing loc name words =
        let index, rest = parse_polish words in
        make_node loc @@ Index_into { name; index }, rest

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
        and parse_take loc words =
            let rec parse' acc = function
                | [] -> raise_unreachable_eof @@ Some loc
                | [loc, EOF] -> raise_unexpected_eof loc

                | (_, In) :: _ -> raise @@ Not_implemented (loc, "scoped 'take' not yet implemented")
                | (_, End) :: tl -> List.rev acc, tl
                | (loc, Word w) :: tl -> parse' ((loc, w) :: acc) tl
                | (loc, word) :: _ -> raise_unexpected_word loc [End] word
            in
            let names, rest = parse' [] words in
            List.iter (fun (_, t) -> add_take t) names;
            List.map snd names, rest

        in

        match words with
        | [] -> make_node loc @@ Empty, []

        (* parse variable pushes *)
        | (loc, Word name) :: tl when has_take name ->
                make_node loc @@ Push_take { name }, tl
        | (loc, Word name) :: tl when Hashtbl.mem output.vars name ->
                let typ = Hashtbl.find output.vars name in
                make_node loc @@ Var { name; typ }, tl
        | (loc, Word name) :: tl when Hashtbl.mem output.mems name ->
                let typ, size = Hashtbl.find output.mems name in
                make_node loc @@ Mem { name; typ; size }, tl

        (* parse function/macro call *)
        | (loc, Word name) :: tl when Hashtbl.mem output.macros name ->
                parse_func_call make_macro loc name tl
        | (loc, Word name) :: tl when Hashtbl.mem output.procs name ->
                parse_func_call make_proc loc name tl

        (* parse operator *)
        | (_, Op _) :: _ ->
                parse_polish words

        (* ERROR -- unknown word *)
        | (loc, Word name) :: _ ->
                let vars   = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) takes ""
                and mem    = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.mems ""
                and procs  = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.procs ""
                and macros = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) output.macros "" in
                raise @@ Error (loc, 
                    sprintf "Unknown word: '%s'.\n"     name ^
                    sprintf "\tavailable vars: %s\n"    vars ^
                    sprintf "\tavailable mem: %s\n"     mem ^
                    sprintf "\tavailable macros: %s\n"  macros ^
                    sprintf "\tavailable procs: %s"     procs)

        (* ERROR -- function definitions only allowed in toplevel *)
        | (loc, Macro) :: _ -> raise @@ Error (loc, "macro definitions only allowed in toplevel")
        | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc definitions only allowed in toplevel")

        (* ERROR -- global memory definitions only allowed in toplevel *)
        | (loc, Mem) :: _ -> raise @@ Error (loc, sprintf "global memory definitions only allowed in toplevel")
        | (loc, Var) :: _ -> raise @@ Error (loc, sprintf "global variable definitions only allowed in toplevel")


        | (loc, Take) :: tl ->
                let vars, tl = parse_take loc tl in
                make_node loc @@ Take { vars }, tl
        | (loc, Peek) :: tl ->
                let vars, tl = parse_take loc tl in
                make_node loc @@ Peek { vars }, tl

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
                let (data : data_hl) = Literal (data_lit_of_data_tok data) in
                let l = loc
                and t = Some (type_of_data_hl data)
                and n = Push_data { data } in
                { l; t; n }, tl

        | (loc, word) :: _ ->
                raise @@ Error (loc, string_of_word word ^ ": word not allowed at the toplevel")
    in

    (** parse top-level program constructs -- global memory and functions *)
    let parse_toplevel words =
        let parse_tl' (words : (location * word) list) =
            match words with

            (* parse vars and arrays, don't add anything to the AST *)
            | (_, Var) :: (_, Word name) :: (loc, Is) :: tl ->
                    add_var loc name tl
            | (_, Mem) :: (_, Word name) :: (_, Is) :: (loc, (Literal Integer _ | Word _ as size)) :: tl ->
                    add_mem loc name size tl

            (* ERROR -- invalid var/mem format *)
            | (loc, Mem) :: _ -> raise @@ Error (loc, sprintf "usage: mem <name> is <type> <size> end")
            | (loc, Var) :: _ -> raise @@ Error (loc, sprintf "usage: var <name> is <type> end")


            (* parse functions -- macros ans procs, don't add anyting to the AST *)
            | (_, (Macro : word)) :: (loc, Word name) :: tl ->
                    make_node loc @@ Empty, add_func loc output.macros name tl
            | (_, Proc ) :: (loc, Word name) :: tl ->
                    make_node loc @@ Empty, add_func loc output.procs name tl

            (* ERROR -- missing the name of the function *)
            | (loc, Macro) :: _ -> raise @@ Error (loc, "macro: expected name")
            | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc: expected name")

            | (loc, word) :: _ -> raise @@ Error (loc, string_of_word word ^ " not allowed in the toplevel")
            | _ -> raise @@ Unreachable "empty list in parse_toplevel"
        in
        parse_scope [|EOF|] [||] parse_tl' words
        |> ignore
    in

    parse_toplevel words;
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
    if matches then
        print_endline "OK"
    else
        failwith (Format.asprintf "%s\n!=\n%s" (show_parser_output expected) (show_parser_output parsed));
    matches

let hashtbl_of_list l =
    Hashtbl.of_seq @@ List.to_seq l

let test_loc = {
    filename = "[test]";
    included_from = [];
    expanded_from = [];
    row = 1; col = 1
}

let%test "types" =
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

let%expect_test _ =
    [(Mem : word); Word "x"; Is; Type I32; End; EOF]
    |> List.map (fun prep -> (test_loc, prep))
    |> parse null_loc
    |> ignore;
    [%expect {||}]

