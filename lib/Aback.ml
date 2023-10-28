open Format
open Lexer
open Preprocess
open Parser
open Postprocess
open Program

let compile filename =
    let strings = ref [] in
    let funcs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    let ir =
        try read_src_file filename
        |> lex filename
        |> preprocess
        |> parse strings funcs macros
        |> postprocess
        with Error (loc, msg) ->
            printf "%s: %s\n" (print_location loc) msg;
            exit 1
    and strings =
        !strings
        |> List.rev
        |> Array.of_list
    in

    { ir; strings }

let simulate =
    Program.exec
