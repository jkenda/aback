open Lexer
open Preprocess
open Program
open Format

type func_format =
    | Typed of (location * prep) list * (location * prep) list
    | Numbered of int * int
[@@deriving show { with_path = false }]

type func = {
    loc : location;
    seq : (location * prep) list;
    types  : func_format;
    recursive : bool;
}
[@@deriving show { with_path = false }]

let nargs = function
    | Typed (args, _) -> List.length args
    | Numbered (nargs, _) -> nargs

let _print_funcs funcs =
    print_string 
    @@ Hashtbl.fold (fun name macro acc -> acc ^ sprintf "%s: %s\n" name (show_func macro)) funcs ""

type name = string
type addr = int
type size = int

type node =
    | Empty

    | Proc of name * func * node list
    | Macro of name * func * node list
    | Var of name * typ
    | Mem of name * (typ * size)
    | Take of name

    | If of node * node * node
    | While of node * node

    | Push of Preprocess.data
    | Op of operator * node * node

let strings = ref ""
let takes = Hashtbl.create 10
let vars = Hashtbl.create 10
let mem = Hashtbl.create 10
let names = ref []
let next_addr = ref 0
let max_addr = ref (-1)

(* parse the preprocessed words into intermediate representation *)
let parse procs macros words =

    (* add a function to the table of macros or procs *)
    let add_func loc name words table =
        let recursive = ref false in

        let rec extract_types input t_in t_out = function
            | (_, Return) :: words -> extract_types false t_in t_out words
            | (_, Is) :: words -> List.rev t_in, List.rev t_out, words
            | (loc, (Type _ | Word _ as t)) :: words ->
                    if input then extract_types input ((loc, t) :: t_in) t_out words
                    else extract_types input t_in ((loc, t) :: t_out) words
            | (loc, word) :: _ -> raise @@ Error (loc,
                sprintf "Expected 'is' or type, got %s" (print_prep word))
            | [] -> raise @@ Error (loc, "expected 'is' after function declaration")
        and add' acc = function
            | [] -> raise @@ Error (loc, "'end' expected")
            | (loc, (Mem : prep)) :: _ -> raise @@ Error (loc, "cannot allocate global memory inside a function")
            | (_, End) :: words ->
                    List.rev acc, words
            | (_, Word _name) as word :: words when name = _name ->
                    recursive := true;
                    add' (word :: acc) words
            | word :: words ->
                    add' (word :: acc) words
        in
        let types, words =
            match (words : (location * prep) list) with
            | (_, Push Int n_in) :: (_, Return) :: (_, Push Int n_out) :: (_, Is) :: tl ->
                    Numbered (n_in, n_out), tl
            | _ ->
                    let t_in, t_out, words = extract_types true [] [] words in
                    Typed (t_in, t_out), words
        in
        let seq, words = add' [] words in
        let recursive = !recursive in
        Hashtbl.replace table name { loc; types; seq ; recursive };
        words
    and add_string str =
        try
            let re = Str.regexp_string str in
            Str.search_forward re !strings 0, String.length str
        with Not_found ->
            let addr = String.length !strings in
            strings := !strings ^ str ^ "\x00";
            addr, String.length str
    and add_var name typ =
        Hashtbl.add vars name typ
    and add_mem loc name typ size =
        let size =
            match size with
            | Word size -> (
                    let macro =
                        try Hashtbl.find macros size
                        with _ -> raise @@ Error (loc, "Unknown value")
                    in
                    match macro with
                    | { seq = [_, Push Int size]; _ } -> size
                    | _ -> raise @@ Error (loc, "size has to be of constant value"))
            | Push Int size -> size
            | _ -> raise @@ Error (loc, "usage: mem <name> <type> <size> end")
        in
        Hashtbl.add mem name (typ, size);
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

    (* parse Polish notation starting from the root *)
    let rec parse_polish = function
        | [] -> Empty, []
        | (loc, word : location * prep) :: words ->
                match word with
                | Op op ->
                        let node1, words = parse_polish words in
                        let node2, words = parse_polish words in
                        Op (op, node1, node2), words
                | Push p -> Push p, words
                | Word name when Hashtbl.mem macros name ->
                        let func = Hashtbl.find macros name in
                        let nargs = nargs func.types in
                        let args, words = parse_args loc name nargs words in
                        Macro (name, func, args), words
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
    let parse_func_call loc name words func =
        let nargs = nargs func.types in
        let args, tl = parse_args loc name nargs words in
        func, args, tl
    in
    let parse_proc_call loc name words =
        let func,  args, tl =
            Hashtbl.find macros name
            |> parse_func_call loc name words
        in
        Macro (name, func, args), tl
    and parse_macro_call loc name words =
        let func,  args, tl =
            Hashtbl.find macros name
            |> parse_func_call loc name words
        in
        Proc (name, func, args), tl
    in

    let parse_head = function
        | [] -> Empty, []

        (* parse functions -- macros ans procs, don't add anyting to the AST *)
        | (_, (Macro : prep)) :: (loc, Word name) :: tl ->
                Empty, add_func loc name tl macros
        | (_, Proc ) :: (loc, Word name) :: tl ->
                Empty, add_func loc name tl procs

        (* missing the name of the function *)
        | (loc, Macro) :: _ -> raise @@ Error (loc, "macro: expected name")
        | (loc, Proc)  :: _ -> raise @@ Error (loc, "proc: expected name")

        (* parse vars and arrays, don't add anything to the AST *)
        | (_, Var) :: (_, Word name) :: (_, Is) :: (_, Type t) :: (_, End) :: tl ->
                add_var name t;
                Empty, tl
        | (_, Mem) :: (_, Word name) :: (_, Is) :: (_, Type t) :: (loc, size) :: (_, End) :: tl ->
                add_mem loc name t size;
                Empty, tl
        | (loc, Mem) :: _ -> raise @@ Error (loc, sprintf "usage: mem <name> is <type> <size> end")
        | (loc, Var) :: _ -> raise @@ Error (loc, sprintf "usage: var <name> is <type> end")


        (* parse variable pushes *)
        | (_, Word name) :: tl when Hashtbl.mem takes name ->
                Take name, tl
        | (_, Word name) :: tl when Hashtbl.mem vars name ->
                Var (name, Hashtbl.find vars name), tl
        | (_, Word name) :: tl when Hashtbl.mem mem name ->
                Mem (name, Hashtbl.find mem name), tl

        (* parse function/macro calls *)
        | (loc, Word name) :: tl when Hashtbl.mem macros name ->
                parse_macro_call loc name tl
        | (loc, Word name) :: tl when Hashtbl.mem procs name ->
                parse_proc_call loc name tl

        (* unknown word -- ERROR *)
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

        (*
           TODO
         *)

        (* index into arrays to get the value or assign to it *)
        | (li, Index) :: (ln, Word name) :: tl when Hashtbl.mem mem name ->
                index_into_array li ln name tl
        | (la, Assign) :: (li, Index) :: (ln, Word name) :: tl when Hashtbl.mem mem name ->
                assign_to_array la li ln name tl
        | (la, Assign) :: (ln, Word name) :: tl when Hashtbl.mem vars name ->
                assign_to_var la ln name tl
        | (_, Index) :: (ln, Word name) :: _ ->
                let mem = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) mem "" in
                raise @@ Error (ln, sprintf "unknown mem: %s. available: %s" name mem)
        | (_, Assign) :: (_, Index) :: (ln, Word name) :: _ ->
                let mem = Hashtbl.fold (fun acc _ v -> acc ^ sprintf " %s" v) mem "" in
                raise @@ Error (ln, sprintf "unknown mem: %s. available: %s" name mem)
        | (loc, Index) :: _ ->
                raise @@ Error (loc, "expected [] <mem> <index>")
        | (loc, Assign) :: _ ->
                raise @@ Error (loc, "expected := <mem> <index> or := [] <mem> <index>")

        (* separator ; *)
        | (_, Sep) :: tl ->
                None, tl
        
        | (loc, If) :: tl ->
                parse_if loc tl

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

        | (loc, word) :: tl -> parse' (ir_of_word loc word @ top, rest) tl
    in

    let rec parse' acc = function
        | [] -> List.rev acc
        | words ->
                let node, words = parse_head words in
                parse' (node :: acc) words
    in
    parse' [] words
