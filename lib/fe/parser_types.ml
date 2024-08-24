open Format

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
    is_signature : bool;
    mutable is_unused : bool
}
and node = {
    l          : location;
    n          : node_hl;
    mutable t  : type_hl list option;
    mutable id : int option
}
and node_hl =
    | Empty
    | Var of { name : string; typ : type_hl }
    | Mem of { name : string; typ : type_hl; size : int }

    | Take of { vars : string list }
    | Peek of { vars : string list }
    | Scoped_take of { vars : string list; body : node list }
    | Scoped_peek of { vars : string list; body : node list }

    | Push_take   of { name : string }
    | Push_data   of { data : data_hl }
    | Push_var    of { name : string }
    | Push_mem    of { name : string }
    | Push_member of { name : string; index : node }

    | Proc_call of { func : func; args : node list }
    | Macro_call of { func : func; args : node list }

    | Assign_to_mem of { name : string; index : node; value : node }
    | Assign_to_var of { name : string; value : node }
    | If_statement  of { cond : node; true_branch : node list; false_branch : node list }
    | While_statement of { cond : node; body : node list}

    | Op of { op : operator; left : node; right : node }

    | Unknown_sequence of { length : int }
[@@deriving show { with_path = false }]

let make_node loc node_hl =
    { l = loc; t = None; n = node_hl; id = None }

let make_macro_call loc func args =
    make_node loc @@ Macro_call { func; args }

let string_of_node node =
    let rec string_of_nodes' ind nodes =
        List.fold_left (fun acc node -> acc ^ string_of_node' ind node) "" nodes
    and string_of_node' ind node =
        let tabs =
            (Seq.init ind (fun _ -> "\t")
            |> Seq.fold_left (^) "")
        and var_id =
            sprintf "[%s] " (try string_of_int @@ Option.get node.id with _ -> "??")
        in
        let node_str =
            match node.n with
            | Empty -> ""
            | Push_data { data; _ } ->
                    show_data_hl data
            | Op { op; left; right } ->
                    show_operator op ^ "\n"
                    ^ string_of_node' (ind + 1) left
                    ^ string_of_node' (ind + 1) right ^ ";;"
            | Scoped_take { vars; body }
            | Scoped_peek { vars; body } ->
                    sprintf "%s %s in\n%s%send\n"
                        (match node.n with Scoped_take _ -> "take" | _ -> "peek")
                        (List.fold_left (sprintf "%s %s") "" vars)
                        (string_of_nodes' (ind + 1) body)
                        tabs
            | Push_take { name } -> name ^ "\n"

            | Proc_call  { func; args }
            | Macro_call { func; args } ->
                    func.name ^ "\n"
                    ^ string_of_nodes' (ind + 1) args

            | _ ->
                    show_node node
        in
        if node.n <> Empty then
            tabs ^ var_id ^ node_str ^ ";;\n"
        else
            ""
    in
    string_of_node' 2 node

let string_of_nodes =
    List.fold_left (fun acc node -> acc ^ string_of_node node) ""

let string_of_func func =
    let unused = if func.is_unused then "(unused) " else "" in
    let t_in = string_of_types_hl func.types.t_in
    and t_out = string_of_types_hl func.types.t_out in
    if func.is_signature then
        Format.sprintf "\n\t%s(prototype) func %s %s -> %s end\n" unused func.name t_in t_out
    else
        Format.sprintf "\n\t%sfunc %s %s -> %s is\n" unused func.name t_in t_out
        ^ string_of_nodes func.seq
        ^ "\tend\n"

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
        "bool"
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
