open Lexer
open Preprocess
open Common

type typ =
    | Primitive of Lexer.typ
    | Struc of (string * typ) list
    | Union of (string * typ) list
    | Ptr of typ
[@@deriving show { with_path = false }]

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
    | Syscall of { loc : location; number : int; args : node list}

    | Index_into of { loc : location; name : string; index : node }
    | Assign_to_mem of { loc : location; name : string; index : node; value : node }
    | Assign_to_var of { loc : location; name : string; value : node }
    | If_statement of { loc : location; cond : node; true_branch : node list; false_branch : node list }
    | While_statement of { loc : location; cond : node; body : node list}

    | Op of { loc : location; op : operator; left : node; right : node}

    | Unknown_sequence of { loc : location; length : int }
[@@deriving show { with_path = false }]

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

    let pp ppf values =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (show_func data))
        values 
end

module Vars = struct
    type t = (string, typ) Hashtbl.t

    let pp ppf values =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (show_typ data))
        values 
end

module Mems = struct
    type t = (string, typ * int) Hashtbl.t

    let pp ppf values =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (Format.sprintf "%s %d" (show_typ (fst data)) (snd data)))
        values 
end

module Typs = struct
    type t = (string, typ) Hashtbl.t

    let pp ppf values =
        Hashtbl.iter (fun key data ->
            Format.fprintf ppf "@[<1>%s: %s@]@." key (show_typ data))
        values 
end

type parser_output = {
    procs : Funcs.t;
    macros : Funcs.t;
    vars : Vars.t;
    mems : Mems.t;
    typs : Typs.t
}
[@@deriving show { with_path = false }]


let make_proc (loc, func, args) =
    Proc_call { loc; func; args }

let make_macro (loc, func, args) =
    Macro_call { loc; func; args }
