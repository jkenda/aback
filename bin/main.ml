open Format

let print_usage msg =
    printf "%s\n" msg;
    printf "usage: %s [mode] <path>\n" Sys.argv.(0);
    printf "mode: int com print check";
    exit 1

type mode =
    | Interpret
    | Compile
    | Check
    | Print
[@@deriving show { with_path = false }]

let () =
    let mode =
        match Sys.argv.(1) with
        | "int" -> Interpret
        | "com" -> Compile
        | "check" -> Check
        | "print" -> Print
        | word -> print_usage @@ sprintf "unknown mode: '%s'" word
        | exception _ -> print_usage "not enough arguments"
    in

    let path =
        try Sys.argv.(2)
        with _ -> print_usage "not enough arguments"
    in

    (* TODO: implement compilation mode (x86_64) *)
    Aback.read path |>
    match mode with
    | Interpret -> Aback.interpret path
    | Compile -> raise @@ Failure (sprintf "%s not implemented" (show_mode mode))
    | Check -> Aback.check path
    | Print -> Aback.print path



