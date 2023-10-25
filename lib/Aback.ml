open Lexer
open Parser
open Program

let compile text =
    let strings = ref [] in
    let ir =
        let funcs = Hashtbl.create 10
        and macros = Hashtbl.create 10 in
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
    (* print_string @@ List.fold_left (fun acc str -> acc ^ str ^ "\n") "" (List.map show_ir ir); *)

    { ir; strings }

let simulate =
    Program.exec
