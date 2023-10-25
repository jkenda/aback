open Lexer
open Parser
open Program

let compile text =
    let strings = ref [] in
    let funcs = Hashtbl.create 10
    and macros = Hashtbl.create 10 in
    let ir =
        try text
        |> lex
        |> parse strings funcs macros with Failure msg ->
                print_endline msg;
                exit 1
    and strings =
        !strings
        |> List.rev
        |> Array.of_list
    in
    (* let open Format in
    print_string @@ Hashtbl.fold (fun name macro acc -> acc ^ sprintf "%s: %s\n" name (show_func macro)) macros "";
    print_string @@ List.fold_left (fun acc str -> acc ^ str ^ "\n") "" (List.map show_ir ir); *)

    { ir; strings }

let simulate =
    Program.exec
