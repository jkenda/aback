let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let print_usage () =
    Printf.printf "usage: %s <path>\n" Sys.argv.(0)

let () =
    if Array.length Sys.argv != 2 then (
        print_usage ();
        exit 1);

    read_whole_file Sys.argv.(1)
    |> Aback.compile
    |> Aback.simulate
    |> fun stack -> assert (stack = [])

