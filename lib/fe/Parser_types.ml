open Common

type func_format = {
    t_in  : type_hl list;
    t_out : type_hl list;
}
[@@deriving show { with_path = false }]

type direction =
    | In
    | Out

type func = {
    loc    : location;
    name   : string;
    seq    : node list;
    types  : func_format;
    ncalls : int ref
}
and node = {
    l         : location;
    mutable t : type_hl option;
    n         : node_hl
}
and node_hl =
    | Empty
    | Var of { name : string; typ : type_hl }
    | Mem of { name : string; typ : type_hl; size : int }

    | Take of { vars : string list }
    | Peek of { vars : string list }
    | Push_take of { name : string }
    | Push_data of { data : data_hl }

    | Proc_call of { func : func; args : node list }
    | Macro_call of { func : func; args : node list}

    | Index_into    of { name : string; index : node }
    | Assign_to_mem of { name : string; index : node; value : node }
    | Assign_to_var of { name : string; value : node }
    | If_statement  of { cond : node; true_branch : node list; false_branch : node list }
    | While_statement of { cond : node; body : node list}

    | Op of { op : operator; left : node; right : node }

    | Unknown_sequence of { length : int }
[@@deriving show { with_path = false }]

let make_node loc node_hl =
    { l = loc; t = None; n = node_hl }

let string_of_node =
    let rec str_of_node' ind node =
        let tabs =
            (Seq.init ind (fun _ -> "\t")
            |> Seq.fold_left (^) "")
        in
        let node_str =
            match node.n with
            | Empty -> ""
            | Push_data { data; _ } ->
                    show_data_hl data
            | Op { op; left; right } ->
                    show_operator op ^ "\n"
                    ^ str_of_node' (ind + 1) left
                    ^ str_of_node' (ind + 1) right ^ ";;"
            | _ ->
                    show_node node
        in
        tabs ^ node_str ^ ": " ^ (match node.t with Some t -> string_of_type_hl t | None -> "Unknown") ^ "\n"
    in
    str_of_node' 1

let string_of_func func =
    (Format.sprintf "\nfunc %s %s -> %s is\n" func.name (string_of_types_hl func.types.t_in) (string_of_types_hl func.types.t_out)) ^
    (List.fold_left (fun acc node -> acc ^ string_of_node node) "" func.seq) ^
    "end\n"

let primitives =
    let add_type str =
        str, type_ll_of_string str
    and make_str_type_hl (str, type_ll) =
        str, type_hl_of_type_ll type_ll
    in

    [
        "i8"; "i16"; "i32"; "i64";
        "u8"; "u16"; "u32"; "u64";
        "f32"; "f64";
        "bool"; "str"; "cstr"
    ]
    |> List.map add_type
    |> List.map make_str_type_hl
    |> List.to_seq
    |> Hashtbl.of_seq

module Funcs = struct
    type t = (string, func) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (string_of_func data))
end

module Vars = struct
    type t = (string, type_hl) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (show_type_hl data))
end

module Mems = struct
    type t = (string, type_hl * int) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (Format.sprintf "%s %d" (show_type_hl (fst data)) (snd data)))
end

module Typs = struct
    type t = (string, type_hl) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (show_type_hl data))
end

module Strings = struct
    type t = string

    let pp ppf =
        Format.fprintf ppf "@[<1>%s@]@."
end

type parser_output = {
    procs : Funcs.t;
    macros : Funcs.t;
    vars : Vars.t;
    mems : Mems.t;
    typs : Typs.t;
    mutable strings : Strings.t;
}
[@@deriving show { with_path = false }]


let make_proc (loc, func, args) =
    { t = None; l = loc; n = Proc_call { func; args } }

let make_macro (loc, func, args) =
    { t = None; l = loc; n = Macro_call { func; args } }
