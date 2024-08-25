open Format

open Common
open Lexer
open Preprocess
open Parser
open Check
open Compile
(*open Postprocess*)
(*open Program*)


let print_usage msg =
    printf "%s\n" msg;
    printf "usage: %s [mode] <options> <path> [-- <run-args>]\n\n" Sys.argv.(0);
    printf "mode: int com print check\n\n";
    printf "options:\n";
    printf "\t-r: run program after compilation\n";
    printf "\t-i: print intermediate language\n";
    printf "\t-a: print assembly\n";
    printf "\t-o <path>: write output to <path>\n\n";
    printf "run-args: arguments to be passed to the compiler program\n\n";
    exit 1

let parse_args args =
    let options = { mode = Interpret; flags = []; path_in = None; path_out = None; run_args = "" } in

    let add_flag flag =
        options.flags <- flag :: options.flags
    in

    let parse_flag = function
        | 'r' -> add_flag Run
        | 'i' -> add_flag Stdout_il
        | 'a' -> add_flag Stdout_asm
        | c -> raise @@ Invalid_argument (sprintf "unknown flag: '%c'" c)
    in
    let rec parse_args' args =
        match args with
        | [] -> ()
        | [path] -> options.path_in <- Some path
        | path :: "--" :: tl ->
                if not @@ List.mem Run options.flags then
                    raise @@ Invalid_argument "passing run args without -r flag";

                options.path_in <- Some path;
                options.run_args <- (List.fold_left (fun acc arg -> acc ^ " " ^ arg) "" tl)
        | "-o" :: path :: tl ->
                options.path_out <- Some path;
                parse_args' tl

        | arg :: tl ->
            begin
                match arg with
                | "int" -> options.mode <- Interpret
                | "com" -> options.mode <- Compile
                | "check" -> options.mode <- Check
                | "print" -> options.mode <- Print
                | arg when String.starts_with ~prefix:"-" arg && not @@ String.starts_with ~prefix:"--" arg ->
                        String.iter parse_flag @@ String.sub arg 1 (String.length arg - 1)
                | word ->
                        raise @@ Invalid_argument (sprintf "invalid argument: '%s'" word)
            end;
            parse_args' tl;
    in
    try
        parse_args' args;

        begin
            if options.path_in = None then
                raise @@ Invalid_argument "no input file";
            if options.path_out = None then
            begin
                match options.path_in with
                | Some path -> options.path_out <- Some (Str.replace_first (Str.regexp ".ab$") "" path)
                | None -> ()
            end;

            if List.mem Run options.flags && (List.mem Stdout_il options.flags || List.mem Stdout_asm options.flags) then
                raise @@ Invalid_argument "-r flag specified but no executable will be produced";
            if List.mem Stdout_il options.flags && List.mem Stdout_asm options.flags then
                raise @@ Invalid_argument "-i and -a flags are mutually exclusive";
        end;

        options
    with Invalid_argument msg ->
        print_usage msg


let exec options =
    let path_in =
        match options.path_in with
        | None -> "[STDIN]"
        | Some path -> path
    in
    print_endline @@ show_options options;

    let src =
        match options.path_in with
        | None -> read_whole_stream stdin
        | Some path ->
            try read_src_file path
            with Error (loc, msg) ->
                print_error loc msg;
                exit 2
    and null_loc = {
        filename = path_in;
        included_from = [];
        expanded_from = [];
        row = 1;
        col = 1 }
    in

    let lex = lex path_in []
    and parse = parse null_loc in
    (* compile the program *)
    try
        let parsed =
                src
                |> lex
                |> preprocess 
                |> (fun l -> l @ [null_loc, EOF])
                |> parse
        in
        match options.mode with
        | Check ->
                parsed
                |> check
                |> ignore;
                print_endline "OK."
        | Print ->
                parsed
                |> show_parser_output
                |> print_string
        | Compile ->
                parsed 
                |> check
                |> compile options;

                if List.mem Run options.flags then
                    let path_exe = Option.get options.path_out in
                    Sys.command @@ sprintf "./%s %s" path_exe options.run_args
                    |> exit
        | _ ->
                failwith @@ show_mode options.mode ^ " not implemented"
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
