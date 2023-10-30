open Lexer
open Preprocess
open Parser
open Check
open Postprocess
open Program

let compile filename =
    let strings = ref [] in
    let procs = Hashtbl.create 10
    and macros = Hashtbl.create 10
    and max_addr = ref 0 in
    let parse =
        parse strings procs macros max_addr
    and check =
        check procs macros
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
    and storage_size = !max_addr + 1
    in

    { ir; loc; strings; storage_size }

let simulate program =
    try Program.exec program
    with Error (loc, msg) ->
        print_error (loc, msg);
        exit 1
