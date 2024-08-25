open Aback

let () =
    let args =
        Sys.argv
        |> Array.to_list
        |> List.tl
    in
    let options = parse_args args in
    Aback.exec options
