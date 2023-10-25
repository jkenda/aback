open Lexer
open Parser
open Program

let compile text =
    let strings = ref [] in
    let ir =
        text
        |> lex
        |> parse strings
    and strings =
        !strings
        |> List.rev
        |> Array.of_list
    in
    { ir; strings }

let simulate =
    Program.exec
