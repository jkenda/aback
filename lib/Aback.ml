open Lexer
open Preprocess
open Parser
open Check
open Postprocess
open Program

let compile filename =
    let strings = ref [] in
    let funcs = Hashtbl.create 10
    and macros = Hashtbl.create 10
    and max_addr = ref 0 in
    let parse =
        parse strings funcs macros max_addr
    in
    let loc, ir =
        try
            read_src_file filename
            |> lex filename []
            |> preprocess
            |> parse
            |> check
            |> postprocess
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 1
    and strings =
        !strings
        |> List.rev
        |> Array.of_list
    in

    { ir; loc; strings; storage = Array.make (!max_addr + 1) (Int 0) }

let simulate program =
    try Program.exec program
    with Error (loc, msg) ->
        print_error (loc, msg);
        exit 1
