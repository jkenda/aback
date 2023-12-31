open Format
open Lexer
open Preprocess
open Parser
open Check
open Postprocess
open Program
open Compile

let read path =
    try
        read_src_file path
    with Error (loc, msg) ->
        print_error (loc, msg);
        exit 2

let interpret path src =
    (* define "global" variables *)
    let procs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    (* specialize functions *)
    let lex = lex path []
    and parse = parse 1 procs macros
    and check = check 1 procs macros in
    (* compile the program *)
    let loc, ir =
        try
            src
            |> lex
            |> preprocess
            |> parse
            |> check
            |> postprocess
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 3
    and strings = !strings
    and storage_size = !max_addr + 1
    in

    try
        Program.interpret
        @@ { ir; loc; strings; mem; vars; storage_size }
    with Error (loc, msg) ->
        print_error (loc, msg);
        exit 4

let compile run path src =
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
    let lex = lex path []
    and parse = parse 8 procs macros
    and check = check 1 procs macros in
    (* compile the program *)
    let loc, ir =
        try
            src
            |> lex
            |> preprocess
            |> parse
            |> check
            |> Array.of_list
            |> Array.split
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 6
    and strings = !strings
    and storage_size = !max_addr + 1
    in

    { ir; loc; strings; mem; vars; storage_size }
    |> to_fasm_x64_linux
    |> write_whole_file (filename ^ ".asm");

    let exit_code = Sys.command ("fasm " ^ filename ^ ".asm") in
    if run then
        if exit_code <> 0 then exit exit_code
        else
            exit
            @@ Sys.command ("./" ^ filename)
    else
        exit exit_code

let print path src =
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
    let procs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    (* specialize functions *)
    let lex = lex path []
    and parse = parse 1 procs macros in
    (* compile the program *)
    let _, ir =
        try
            src
            |> lex
            |> preprocess
            |> parse
            |> postprocess
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 6
    and strings = !strings
    and storage_size = !max_addr + 1 in

    printf "program:\n";
    print_ir ir;
    printf "\nstrings: |%s" @@ String.escaped strings;
    printf "|\nstorage size: %d\n" storage_size

let check path src =
    (* define "global" variables *)
    let procs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    (* specialize functions *)
    let lex = lex path []
    and parse = parse 1 procs macros
    and check = check 1 procs macros in
    (* compile the program *)
    let () =
        try
            src
            |> lex
            |> preprocess
            |> parse
            |> check
            |> ignore
        with Error (loc, msg) ->
            print_error (loc, msg);
            exit 7
    in

    print_endline "OK."


(* testing *)

let%expect_test _ =
    interpret "[test]" "puti + 1 2";
    [%expect {| 3 |}]

let%expect_test _ =
    interpret "[test]" {|
        macro put is
            puti ; putc ' '
        end
        put + 1  2 ; put - 7 13 ;
        put * 7  3 ; put / 7  3 ;
        put % 7  3
    |};
    [%expect {| 3 -6 21 2 1 |}]

let%expect_test _ =
    interpret "[test]" {|
        include "core/io.ab"

        macro put. is
            putf ; putc ' '
        end
        put. +. 1.  2. ; put. -. 7. 13. ;
        put. *. 7.  3. ; put. /. 7.  3.
    |};
    [%expect {| 3. -6. 21. 2.33333333333 |}]

let%expect_test _ =
    interpret "[test]" {| puts "Hello," ; puts " world!\n" |};
    [%expect {| Hello, world! |}]

let%expect_test _ =
    interpret "[test]" {|
        macro drop 1 -> 0 is
            take _ in
            end
        end

        macro over a' b' -> b' a' b' is
            peek _ b in
                b
            end
        end

        macro 2drop 2 -> 0 is
            drop drop
        end

        macro LIMIT is 20 end

        (output a Fibonacci sequence up to LIMIT)
        1 0 while < over LIMIT do
            take a b in
                puti a ;
                + a b a
            end
            putc ' '
        end 2drop ;
        puts "\n"
    |};
    [%expect {| 1 1 2 3 5 8 13 |}]

let%expect_test _ =
    interpret "[test]" {|
        include "core/io.ab"

        macro min
            a' a' -> a'
        is
        take a b in
            if <= a b then a
            else b end
        end end

        puti min 7  13   ; puts " " ;
        putf min 7. 3.14 ; puts " "
    |};
    [%expect {| 7 3.14 |}]

