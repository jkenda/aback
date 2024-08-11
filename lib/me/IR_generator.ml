open Format
open Common
open Parser_types

let (qbe_string_of_type_extty : type_ll -> string) = function
    | I8 | U8 | Bool -> "b"
    | I16 | U16 -> "h"
    | I32 | U32 -> "w"
    | I64 | U64 | Ptr _ -> "l"
    | F32 -> "s"
    | F64 -> "d"

let (qbe_string_of_type_basety : type_ll -> string) = function
    | I8 | U8 | Bool | I16 | U16 | I32 | U32 -> "w"
    | I64 | U64 | Ptr _ -> "l"
    | F32 -> "s"
    | F64 -> "d"

let (qbe_string_of_type_signed : type_ll -> string) = function
    | I8 -> "sb" | U8 | Bool -> "ub"
    | I16 -> "sh" | U16 -> "uh"
    | I32 -> "sw" | U32 -> "uw"
    | I64 | U64 | Ptr _ -> "l"
    | F32 -> "s"
    | F64 -> "d"

let qbe_string_of_operator t_in (t_out : type_hl) op =
    let signed =
        match t_out with
        | Primitive (I8 | I16 | I32 | I64 | F32 | F64) -> true
        | Primitive (U8 | U16 | U32 | U64| Bool| Ptr _)
        | Ptr _ -> false
        | _ -> failwith @@ sprintf "%s not directly comparable" (show_type_hl t_out)
    in

    let s = if signed then "s" else "u" in

    match op with
    | Eq -> "eq" | NEq -> "ne"
    | Lt -> s ^ "lt" | LEq -> s ^ "le"
    | Gt -> s ^ "gt" | GEq -> s ^ "ge"

    | Add | FAdd -> "add"
    | Sub | FSub -> "sub"
    | Mul | FMul -> "mul"
    | Div | FDiv -> "div"
    | Mod -> if signed then "mod" else "umod"

    | Itof
    | Ftoi

    | LAnd | LOr | LXor | Lsl | Lsr
    | And  | Or
    | Ref | Deref

    | Putc | Puts

let qbe_string_of_args =
    List.fold_left (fun s t -> s ^ ", " ^ qbe_string_of_type_extty t) ""

let generate_qbe_ir f { procs; _ } =
    let output_loc loc =
        fprintf f ".loc %d, %d\n" loc.col loc.row
    and output_data var_num (data : data_ll) =
        let type_ll = type_of_data_ll data in
        let preamble = sprintf "%%v%d =%s " var_num (qbe_string_of_type_basety type_ll) in

        match data with
        | Ptr (t, var, off) ->
                fprintf f "%%i%d =w add $%s, %d" var_num var off;
                fprintf f "%s load%s %%i%d" preamble (qbe_string_of_type_signed t) var_num
        | _ ->
                fprintf f "%s %s" preamble (string_of_data_ll data)
    in

    let rec output_node i = function
        | Empty -> ()
        | Push_literal { loc; data } -> (
            output_loc loc;
            output_data i data)
        | Op { loc; op; left; right } -> (
            output_node i left;
            output_node i right;
            left.t
        )
        | _ -> failwith "not implemented"
    in
    let output_seq base_i =
        List.iteri (fun i node -> output_node (base_i + i) node)
    in

    let output_proc _ { loc; name; types; seq; _ } = (
        let varn = ref 0

        (* TODO: multiple return values *)
        and t_out  = type_ll_of_type_hl (List.hd types.t_out)
        and t_in   = List.map type_ll_of_type_hl types.t_in in

        output_loc loc;
        fprintf f "export function %s $%s(%s) {\n" (qbe_string_of_type_extty t_out) name (qbe_string_of_args t_in);
            output_seq !varn seq;
        fprintf f "@start\n";
        fprintf f "}\n";

        varn := !varn + List.length seq)

    in

    Hashtbl.iter output_proc procs;
    ()
