open Lexer
open Preprocess
open Parser
open Check
open Postprocess
open Program

let compile filename =
    let strings = ref [] in
    let funcs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    let loc, ir =
        try
            read_src_file filename
            |> lex filename []
            |> preprocess
            |> parse strings funcs macros
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

    { ir; loc; strings }

let simulate program =
    try Program.exec program
    with Error (loc, msg) ->
        print_error (loc, msg);
        exit 1
