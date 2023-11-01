open Format
open Lexer
open Preprocess
open Parser
open Check
open Postprocess
open Program

let interpret filename =
    (* define "global" variables *)
    let strings = ref ""
    and procs = Hashtbl.create 10
    and macros = Hashtbl.create 10
    and max_addr = ref (-1) in
    (* specialize functions *)
    let lex = lex filename []
    and parse = parse strings procs macros max_addr
    and check = check procs macros in
    (* compile the program *)
    let loc, ir =
        try
            read_src_file filename
            |> lex
            |> preprocess
            |> parse
            |> check
            |> postprocess
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 2
    and strings = !strings
    and storage_size = !max_addr + 1
    in

    try
        Program.interpret
        @@ { ir; loc; strings; storage_size }
    with Error (loc, msg) ->
        print_error (loc, msg);
        exit 3

let print filename =
    let print_ir ir =
        let len =
            Array.length ir - 1
            |> Int.to_float
            |> Float.log10
            |> Float.ceil
            |> Float.to_int
        in
        Array.iteri
        (fun i ir ->
            let l = 
                if i = 0 then 1 else
                Int.to_float (i + 1)
                |> Float.log10
                |> Float.ceil
                |> Float.to_int
            in
            for _ = 1 to len - l do printf " " done;
            printf "%d| %s\n" i (show_ir ir)) ir
    in

    (* define "global" variables *)
    let strings = ref ""
    and procs = Hashtbl.create 10
    and macros = Hashtbl.create 10
    and max_addr = ref (-1) in
    (* specialize functions *)
    let lex = lex filename []
    and parse = parse strings procs macros max_addr
    and check = check procs macros in
    (* compile the program *)
    let _, ir =
        try
            read_src_file filename
            |> lex
            |> preprocess
            |> parse
            |> check
            |> postprocess
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 4
    and strings = !strings
    and storage_size = !max_addr + 1 in

    printf "program:\n";
    print_ir ir;
    printf "\nstrings: |%s" @@ String.escaped strings;
    printf "|\nstorage size: %d\n" storage_size

let check filename =
    (* define "global" variables *)
    let strings = ref ""
    and procs = Hashtbl.create 10
    and macros = Hashtbl.create 10
    and max_addr = ref (-1) in
    (* specialize functions *)
    let lex = lex filename []
    and parse = parse strings procs macros max_addr
    and check = check procs macros in
    (* compile the program *)
    let () =
        try
            read_src_file filename
            |> lex
            |> preprocess
            |> parse
            |> check
            |> ignore
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 5
    in

    print_endline "OK."
