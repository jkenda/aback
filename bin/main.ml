let print_usage () =
    Printf.printf "usage: %s <path>\n" Sys.argv.(0)

let () =
    if Array.length Sys.argv != 2 then (
        print_usage ();
        exit 1);

    Sys.argv.(1)
    |> Aback.compile
    |> Aback.simulate
    |> fun stack -> assert (stack = [])

