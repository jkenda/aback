open Lexer
open Preprocess
open Common

type func_format = {
    t_in  : typ list;
    t_out : typ list;
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

and node =
    | Empty

    | Var of { loc : location; name : string; typ : typ }
    | Mem of { loc : location; name : string; typ : typ; size : int }

    | Take of { loc : location; vars : string list }
    | Peek of { loc : location; vars : string list }
    | Push_take of { loc : location; name : string }
    | Push_literal of { loc : location; data : data }

    | Proc_call of { loc : location; func : func; args : node list }
    | Macro_call of { loc : location; func : func; args : node list}

    | Index_into of { loc : location; name : string; index : node }
    | Assign_to_mem of { loc : location; name : string; index : node; value : node }
    | Assign_to_var of { loc : location; name : string; value : node }
    | If_statement of { loc : location; cond : node; true_branch : node list; false_branch : node list }
    | While_statement of { loc : location; cond : node; body : node list}

    | Op of { loc : location; op : operator; left : node; right : node }

    | Unknown_sequence of { loc : location; length : int }
[@@deriving show { with_path = false }]

let string_of_node =
    let rec str_of_node' ind node =
        if node = Empty then ""
        else
        let tabs =
            (Seq.init ind (fun _ -> "\t")
            |> Seq.fold_left (^) "")
        in
        let node_str =
            match node with
            | Empty ->
                    ""
            | Push_literal { data; _ } ->
                    show_data data ^ "\n"
            | Op { op; left; right; _ } ->
                    show_operator op ^ "\n"
                    ^ str_of_node' (ind + 1) left
                    ^ str_of_node' (ind + 1) right ^ tabs ^ ";;\n"
            | _ -> show_node node
        in
        tabs ^ node_str
    in
    str_of_node' 1

let string_of_func func =
    (Format.sprintf "\nfunc %s %s -> %s is\n" func.name (string_of_typs func.types.t_in) (string_of_typs func.types.t_out)) ^
    (List.fold_left (fun acc node -> acc ^ string_of_node node) "" func.seq) ^
    "end\n"

let primitives =
    Hashtbl.of_seq
    @@ List.to_seq
    [
        "int"   , Primitive Int;
        "float" , Primitive Float;
        "char"  , Primitive Char;
        "bool"  , Primitive Bool;
        "string", Primitive String;
        "cstr"  , Primitive CStr;
    ]

module Funcs = struct
    type t = (string, func) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (string_of_func data))
end

module Vars = struct
    type t = (string, typ) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (show_typ data))
end

module Mems = struct
    type t = (string, typ * int) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (Format.sprintf "%s %d" (show_typ (fst data)) (snd data)))
end

module Typs = struct
    type t = (string, typ) Hashtbl.t

    let pp ppf =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (show_typ data))
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
    Proc_call { loc; func; args }

let make_macro (loc, func, args) =
    Macro_call { loc; func; args }
