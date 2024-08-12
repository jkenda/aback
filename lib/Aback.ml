open Format

open Common
open Lexer
open Preprocess
open Parser
open Check
open Compile
(*open Postprocess*)
(*open Program*)

type mode =
    | Interpret
    | Compile
    | Check
    | Print
[@@deriving show { with_path = false }]


let exec mode path _run =
    let src =
        try
            read_src_file path
        with Error (loc, msg) ->
            print_error loc msg;
            exit 2
    and null_loc = {
        filename = path;
        included_from = [];
        expanded_from = [];
        row = 1;
        col = 1 }
    in

    let lex = lex path []
    and parse = parse null_loc in
    (* compile the program *)
    try
        let parsed =
                src
                |> lex
                |> preprocess 
                |> parse
        in
        match mode with
        | Check ->
                check parsed |> ignore;
                print_endline "OK."
        | Print ->
                show_parser_output parsed
                |> print_string
        | Compile ->
                parsed 
                |> check
                |> compile
        | _ ->
                failwith @@ show_mode mode ^ " not implemented"
    with ex ->
        match ex with Error (loc, msg) ->
            print_error loc msg;
            printf "%s\n" @@ Printexc.to_string ex;
            printf "%s\n" @@ Printexc.get_backtrace ();
            exit 1
        | _ -> ();

(*
    let write_whole_file path bytes =
        let ch = open_out_bin path in
        output_bytes ch bytes;
        close_out ch
    and filename =
        let rec last = function
            | [el; _] -> el
            | _ :: t -> last t
            | _ -> raise @@ Unreachable "empty path"
        in
        path
        |> Str.split (Str.regexp "[/.]")
        |> last
    in

    (* define "global" variables *)
    let procs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    (* specialize functions *)
    let lex = lex path [] in
    (* compile the program *)
    let loc, ir =
        try
            src
            |> lex
            |> preprocess
            |> parse
            |> check
            |> postprocess
            |> compile
            |> output_binary filename
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 6
    in

    if run then
        Sys.command ("./" ^ filename)
        |> exit
*)
