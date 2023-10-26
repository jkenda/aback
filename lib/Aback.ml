open Lexer
open Preprocess
open Parser
open Postprocess
open Program

let _print_ir ir =
    let open Format in
    let str, _ = List.fold_left (fun (acc, i) ir -> acc ^ sprintf "%d: %s\n" i (show_ir ir), i + 1) ("", 0) ir in
    print_string str

let compile text =
    let strings = ref [] in
    let funcs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    let ir =
        try text
        |> lex
        |> preprocess
        |> parse strings funcs macros
        |> postprocess
        with Failure msg ->
                print_endline msg;
                exit 1
    and strings =
        !strings
        |> List.rev
        |> Array.of_list
    in
    (* print_string @@ Hashtbl.fold (fun name macro acc -> acc ^ sprintf "%s: %s\n" name (show_func macro)) macros ""; *)
    _print_ir ir;

    { ir; strings }

let simulate =
    Program.exec
