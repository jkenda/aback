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
        begin
            match ex with
            | Error (loc, msg) ->
                print_error loc msg;
            | Not_implemented (loc, msg) ->
                print_in_color Yellow "(NOT IMPLEMENTED)\n\n";
                print_error loc msg;
            | _ -> ()
        end;

        printf "\n";
        printf "%s\n" @@ Printexc.to_string ex;
        printf "%s\n" @@ Printexc.get_backtrace ();
        printf "%!";
        exit 1
