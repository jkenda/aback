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
    and run =
        match Sys.argv.(3) with
        | "-r" -> true
        | _ | exception _ -> false
    in


    (* TODO: implement compilation mode (x86_64) *)
    Aback.read path |>
    match mode with
    | Interpret -> Aback.interpret path
    | Compile -> Aback.compile run path
    | Check -> Aback.check path
    | Print -> Aback.print path



