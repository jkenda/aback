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

    { ir; strings }

let simulate =
    Program.exec
