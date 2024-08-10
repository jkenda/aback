open Format

type location = {
    filename : string;
    included_from : string list;
    expanded_from : (location * string) list;
    row : int;
    col : int;
}
[@@deriving show { with_path = false }]
let string_of_location loc = sprintf "'%s':%d:%d" loc.filename loc.row loc.col

exception Error of location * string
let print_error (loc, msg) =
    List.iter (fun (loc, name) -> printf "expanded from %s (%s)\n" (string_of_location loc) name) loc.expanded_from;
    printf "%s:\n" (string_of_location loc);
    printf "\t%s\n" msg;
    if List.length loc.included_from > 0 then printf "\n";
    List.iter (fun filename -> printf "included from '%s'\n" filename) loc.included_from;

exception Not_implemented of location * string
exception Unreachable of string

(* read file from the current dir *)
let read_src_file filename =
    if not (String.ends_with ~suffix:".ab" filename) then
        (let loc = { filename; included_from = []; expanded_from = []; row = 0; col = 0 } in
        raise @@ Error (loc, "Aback source files should have '.ab' extension"));

    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

