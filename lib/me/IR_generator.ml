open Printf
open Common
open Parser_types

let (qbe_string_of_type_extty : type_ll -> char) = function
    | I8 | U8 | Bool -> 'b'
    | I16 | U16 -> 'h'
    | I32 | U32 -> 'w'
    | I64 | U64 | Ptr _ -> 'l'
    | F32 -> 's'
    | F64 -> 'd'

let (qbe_string_of_type_basety : type_ll -> char) = function
    | I8 | U8 | Bool | I16 | U16 | I32 | U32 -> 'w'
    | I64 | U64 | Ptr _ -> 'l'
    | F32 -> 's'
    | F64 -> 'd'

let (qbe_string_of_type_signed : type_ll -> string) = function
    | I8 -> "sb" | U8 | Bool -> "ub"
    | I16 -> "sh" | U16 -> "uh"
    | I32 -> "sw" | U32 -> "uw"
    | I64 | U64 | Ptr _ -> "l"
    | F32 -> "s"
    | F64 -> "d"

let qbe_string_of_operator op (t_in : type_hl) =
    let signed =
        match t_in with
        | Primitive (I8 | I16 | I32 | I64 | F32 | F64) -> true
        | Primitive (U8 | U16 | U32 | U64| Bool| Ptr _)
        | Ptr _ -> false
        | t -> failwith @@ sprintf "%s not directly comparable" (show_type_hl t)
    and (_type_width : type_hl -> int) = function
        | Primitive (I8 | U8 | Bool) -> 1
        | Primitive (I16 | U16) -> 2
        | Primitive (I32 | U32 | F32) -> 4
        | Primitive (I64 | U64 | F64) -> 8
        | Ptr _ -> 8
        | t -> failwith @@ sprintf "%s not convertible" (show_type_hl t)
    in

    let s = if signed then "s" else "u"
    and t =
        qbe_string_of_type_basety
        @@ type_ll_of_type_hl t_in
    in

    match op with
    | Eq -> "c" ^ s ^ "eq" ^ Char.escaped t | NEq -> "c" ^ s ^ "ne" ^ Char.escaped t
    | Lt -> "c" ^ s ^ "lt" ^ Char.escaped t | LEq -> "c" ^ s ^ "le" ^ Char.escaped t
    | Gt -> "c" ^ s ^ "gt" ^ Char.escaped t | GEq -> "c" ^ s ^ "ge" ^ Char.escaped t

    | Add | FAdd -> "add"
    | Sub | FSub -> "sub"
    | Mul | FMul -> "mul"
    | Div | FDiv -> "div"
    | Mod -> if signed then "mod" else "umod"

    | Putc -> "call $puts\n"
    | Puts -> "call $putc\n"

    | Itof | Ftoi

    | LAnd | LOr | LXor | Lsl | Lsr
    | And  | Or
    | Ref | Deref ->
            failwith @@ sprintf "operator %s not implemented" (show_operator op)

let qbe_string_of_args =
    let add_qbe_string_of_type_extty s t =
        sprintf "%s, %c" s (qbe_string_of_type_extty t)
    in
    List.fold_left add_qbe_string_of_type_extty ""

let generate_qbe_ir f { procs; _ } =
    (* auxiliary functions for outputting different node types *)
    let output_loc loc =
        fprintf f ".loc %d, %d\n" loc.col loc.row
    and output_data var_num (data : data_ll) =
        let type_ll = type_of_data_ll data in
        let preamble = sprintf "%%v%d =%c " var_num (qbe_string_of_type_basety type_ll) in

        match data with
        | Ptr (t, var, off) ->
                fprintf f "\t%%i%d =w add $%s, %d\n" var_num var off;
                fprintf f "\t%s load%s %%i%d\n" preamble (qbe_string_of_type_signed t) var_num
        | _ ->
                fprintf f "\t%s %s\n" preamble (string_of_data_ll data)
    and output_operator var_num op (t_hl : type_hl) =
        let t_ll = type_ll_of_type_hl t_hl in
        fprintf f "\t%%v%d =%c %s\n" var_num (qbe_string_of_type_basety t_ll) (qbe_string_of_operator op t_hl)
    in

    let rec output_node i node =
        if node.t = None then
            raise @@ Unreachable ("node has no type: " ^ show_node node)
        else
            let typ = Option.get node.t in

            match node.n with
            | Empty -> ()
            | Push_literal { data } -> (
                output_loc node.l;
                output_data i data)
            | Op { op; left; right; _ } -> (
                output_node i left;
                output_node i right;
                output_operator i op typ
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
        fprintf f "export function %c $%s(%s) {\n" (qbe_string_of_type_extty t_out) name (qbe_string_of_args t_in);
        fprintf f "@start\n";

        output_seq !varn seq;

        fprintf f "}\n";

        varn := !varn + List.length seq)
    in

    Hashtbl.iter output_proc procs;

    (* flush the channel *)
    fprintf f "%!";
