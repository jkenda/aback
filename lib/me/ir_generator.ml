open Printf
open Common
open Parser_types

let (qbe_string_of_type_extty : type_ll -> char) = function
    | I8 | U8 | Char | Bool -> 'b'
    | I16 | U16 -> 'h'
    | I32 | U32 -> 'w'
    | I64 | U64 | Ptr _ -> 'l'
    | F32 -> 's'
    | F64 -> 'd'

let (qbe_string_of_type_basety : type_ll -> char) = function
    | I8 | U8 | Char | Bool | I16 | U16 | I32 | U32 -> 'w'
    | I64 | U64 | Ptr _ -> 'l'
    | F32 -> 's'
    | F64 -> 'd'

let (qbe_string_of_type_signed : type_ll -> string) = function
    | I8 -> "sb" | U8 | Bool | Char -> "ub"
    | I16 -> "sh" | U16 -> "uh"
    | I32 -> "sw" | U32 -> "uw"
    | I64 | U64 | Ptr _ -> "l"
    | F32 -> "s"
    | F64 -> "d"

let (qbe_string_of_type_hl : type_hl -> string option) = function
    | Str -> Some "{ l, l }"
    | CStr -> Some "{ l }"
    | _ -> None


let qbe_string_of_operator node op =
    let t_in =
        match node.t with
        | Some [t_hl] -> t_hl
        | _ -> raise @@ Not_implemented (node.l, "multiple return types not supported on operators")
    in
    let signed =
        match t_in with
        | Primitive (I8 | I16 | I32 | I64 | F32 | F64) -> true
        | Primitive (U8 | U16 | U32 | U64| Bool| Ptr _)
        | Ptr _ -> false
        | t -> raise @@ Unreachable (sprintf "%s not directly comparable" (show_type_hl t))
    and (_type_width : type_hl -> int) = function
        | Primitive (I8 | U8 | Bool) -> 1
        | Primitive (I16 | U16) -> 2
        | Primitive (I32 | U32 | F32) -> 4
        | Primitive (I64 | U64 | F64) -> 8
        | Ptr _ -> 8
        | t -> raise @@ Unreachable (sprintf "%s not convertible" (show_type_hl t))
    in

    let s = if signed then "s" else "u"
    and t =
        (* TODO: high-level types *)
        t_in
        |> type_ll_of_type_hl 
        |> qbe_string_of_type_basety
    in

    let expr =
        match op with
        | Eq -> "c" ^ s ^ "eq" ^ Char.escaped t | NEq -> "c" ^ s ^ "ne" ^ Char.escaped t
        | Lt -> "c" ^ s ^ "lt" ^ Char.escaped t | LEq -> "c" ^ s ^ "le" ^ Char.escaped t
        | Gt -> "c" ^ s ^ "gt" ^ Char.escaped t | GEq -> "c" ^ s ^ "ge" ^ Char.escaped t

        | Add -> "add" | Sub -> "sub"
        | Mul -> "mul" | Div -> "div"
        | Mod -> if signed then "mod" else "umod"

        | Cast_to _

        | LAnd | LOr | LXor | Lsl | Lsr
        | And  | Or
        | Ref | Deref ->
                raise @@ Not_implemented (node.l, sprintf "operator %s not implemented" (show_operator op))
    in

    expr, t_in

let qbe_string_of_params =
    let add_qbe_string_of_type_extty s t =
        sprintf "%s, %c" s (qbe_string_of_type_extty t)
    in
    List.fold_left add_qbe_string_of_type_extty ""

let qbe_string_of_args var_id t_in =
    let _, args =
        let type_arg (t : type_hl) arg_n =
            match t with
            | Str  -> sprintf "l %%v%d.off, l %%v%d.len" arg_n arg_n
            | CStr -> sprintf "l %%v%d.off" arg_n
            | _ ->
                    let t = type_ll_of_type_hl t in
                    sprintf "%c %%v%d.arg%d" (qbe_string_of_type_extty t) var_id arg_n
        in
        match t_in with
        | [] -> 0, ""
        | hd :: tl -> List.fold_left (fun (i, acc) t -> i + 1, sprintf "%s, %s" acc (type_arg t i)) (1, type_arg hd 0) tl
    in
    args

let get_node_id node =
    try Option.get node.id
    with _ -> raise @@ Not_implemented (node.l, sprintf "node has no stack offset: %s" (show_node_hl node.n))

let get_node_types node =
    try Option.get node.t
    with _ -> raise @@ Not_implemented (node.l, sprintf "node has no concrete type: %s" (show_node_hl node.n))

let generate_qbe_ir f path_in { procs; strings; _ } =
    let eliminate_unused = Hashtbl.mem procs "main" in
    let branch_counter = ref 0 in

    let get_branch_name b =
        branch_counter := !branch_counter + 1;
        if b then sprintf "true_branch_%d" !branch_counter
        else sprintf "false_branch_%d" !branch_counter
    in

    (* auxiliary functions for outputting different node types *)
    let output_loc loc =
        fprintf f ".loc %d, %d\n" loc.row loc.col
    and output_label name =
        fprintf f "\n@%s\n" name
    and output_ret loc = function
        | 0 -> fprintf f "\tret\n"
        | 1 -> fprintf f "\tret %%v0\n"
        | _ -> raise @@ Not_implemented (loc, "returning multiple args not yet implemented")
    in
    let rec output_data node (data : data_hl) =
        output_loc node.l;
        let var_id = get_node_id node in

        (* TODO: high-level types *)
        match data with
        | Literal Const_str (_, off, len)
        | Str (_, off, len) ->
                fprintf f "\t%%v%d.off =l add $strs, %d\n" var_id off;
                fprintf f "\t%%v%d.len =l copy %d\n" var_id len;

        | Literal Const_cstr (_, off)
        | CStr (_, off) ->
                fprintf f "\t%%v%d.off =l add $strs, %d\n" var_id off;

        | Literal l ->
                let type_hl = get_node_types node |> List.hd in
                let data_hl = data_hl_of_data_lit node.l type_hl l
                and char_type = qbe_string_of_type_basety @@ type_ll_of_type_hl type_hl in
                fprintf f "\t%%v%d =%c copy %s\n" var_id char_type (string_of_data_hl data_hl)

        | Ptr (t_hl, var, off) ->
                output_data node @@ Primitive (Ptr (type_ll_of_type_hl t_hl, var, off))
        | Primitive d_ll -> (
                match d_ll with
                | Ptr (t_ll, var, off) ->
                    (* TODO: high-level types *)
                    let t_q = qbe_string_of_type_basety t_ll
                    and t_s = qbe_string_of_type_signed t_ll in
                    fprintf f "\t%%i%d =l add $%s, %d\n" var_id var off;
                    fprintf f "\t%%v%d =%c load%s %%i%d\n" var_id t_q t_s var_id
                | _ ->
                    let type_ll = type_of_data_ll d_ll in
                    let preamble = sprintf "%%v%d =%c " var_id (qbe_string_of_type_basety type_ll) in
                    (* TODO: high-level data *)
                    let data_qbe =
                        data
                        |> data_ll_of_data_hl
                        |> string_of_data_ll
                    in
                    fprintf f "\t%s copy %s\n" preamble data_qbe)

        | d -> raise @@ Not_implemented (node.l, sprintf "IR generation not implemented for %s" (show_data_hl d))

    in
    let output_take_peek node vars =
        let start_id = Option.get node.id in
        let output_take_peek' i (type_hl, var) =
            let t =
                try qbe_string_of_type_basety @@ type_ll_of_type_hl type_hl
                with _ -> raise @@ Not_implemented (node.l, "peeking complex types not yet implemented")
            in
            fprintf f "\t%%t_%s =%c %%v%d\n" var t (start_id - i - 1)
        in
        let types =
            try Option.get node.t
            with _ -> raise @@ Unreachable (sprintf "node has no types: %s" (show_node node))
        in
        List.iteri output_take_peek' @@ List.combine types vars;
    and output_push_take node name =
        let var_id = get_node_id node in
        let t =
            let type_hl = List.hd @@ get_node_types node in
            try qbe_string_of_type_basety @@ type_ll_of_type_hl type_hl
            with Invalid_argument str | Failure str -> raise @@ Not_implemented (node.l, str)
        in
        fprintf f "\t%%v%d =%c %%t_%s\n" var_id t name
    and output_operator node op =
        let var_id = get_node_id node
        and op, t_hl = qbe_string_of_operator node op in
        let t_ll = type_ll_of_type_hl t_hl in
        output_loc node.l;
        fprintf f "\t%%v%d =%c %s\n" var_id (qbe_string_of_type_basety t_ll) op

    and output_proc_call node func =
        output_loc node.l;

        let var_id = get_node_id node
        and types_hl =
            try Option.get node.t
            with _ -> raise @@ Unreachable ("node has no type: " ^ show_node_hl node.n)
        in
        let args = qbe_string_of_args var_id func.types.t_in in

        match types_hl with
        | [] -> fprintf f "\tcall $%s(%s)\n" func.name args
        | [Primitive t] -> fprintf f "\t%%v%d =%c call $%s(%s)\n" var_id (qbe_string_of_type_basety t) func.name args

        | [_] -> raise @@ Not_implemented (node.l, "procs with complex return types not yet implemented")
        | l -> raise @@ Not_implemented (node.l, sprintf "procs with return types not yet implemented: %s" (show_types_hl l))
    in

    let rec output_if_statement node cond true_branch false_branch =
        let var_id = get_node_id node in
        output_node cond;
        fprintf f "\tjnz %%v%d, @%s, @%s\n" var_id (get_branch_name true) (get_branch_name false);
        output_label @@ get_branch_name true;
        output_seq true_branch;
        output_label @@ get_branch_name false;
        output_seq false_branch;
        fprintf f "\n"

    and output_node node =
        if node.n = Empty then
            ()
        else
            match node.n with
            | Take { vars }
            | Peek { vars } ->
                    output_take_peek node vars
            | Push_take name ->
                    output_push_take node name
            | Push_data data ->
                    output_data node data
            | Proc_call { func; args } ->
                    List.iter output_node args;
                    output_proc_call node func
            | Macro_call { func; args } ->
                    List.iter output_node args;
                    output_seq func.seq
            | If_statement { cond; true_branch; false_branch } ->
                    output_if_statement node cond true_branch false_branch
            | Op { op; left; right; _ } ->
                begin
                    output_node left;
                    output_node right;
                    output_operator node op
                end
            | Unknown_sequence _ -> ()

            | Empty -> raise @@ Unreachable ""
            | n -> raise @@ Not_implemented (node.l, sprintf "IR generation of %s not yet supported" (show_node_hl n))
    and output_seq seq =
        List.iter output_node seq
    in
    let output_proc _ { loc; name; types; seq; is_signature; is_unused } = 
        if is_signature || (eliminate_unused && is_unused) then
            ()
        else
            (* TODO: multiple return values, high-level types *)
            let t_in =
                try List.map type_ll_of_type_hl types.t_in
                with _ -> 
                    raise @@ Not_implemented (loc, "functions with complex return values not yet implemented")
            and t_out =
                match types.t_out with
                | [] -> ""
                | [t] ->
                        (try String.make 1 @@ qbe_string_of_type_extty @@ type_ll_of_type_hl t
                        with _ ->
                            raise @@ Not_implemented (loc, "functions with complex returns not yet implemented"))
                | _ -> raise @@ Not_implemented (loc, "functions with multiple returns not yet implemented")
            in

            output_loc loc;
            fprintf f "export function %s $%s(%s) {\n" (t_out) name (qbe_string_of_params t_in);
            fprintf f "@start\n";

            output_seq seq;
            output_ret loc (List.length types.t_out);

            fprintf f "}\n\n";
    in

    fprintf f "dbgfile \"%s\"\n\n" path_in;
    Hashtbl.iter output_proc procs;

    let strings =
        (* replace OCaml's representation of null bytes with a more standard representation *)
        String.escaped strings
        |> Str.global_replace (Str.regexp {|\\000|}) {|\\0|}
    in
    fprintf f "data $strs = { b \"%s\" }\n" strings
