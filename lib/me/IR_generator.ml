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
        (* TODO: high-level types *)
        t_in
        |> type_ll_of_type_hl 
        |> qbe_string_of_type_basety
    in

    match op with
    | Eq -> "c" ^ s ^ "eq" ^ Char.escaped t | NEq -> "c" ^ s ^ "ne" ^ Char.escaped t
    | Lt -> "c" ^ s ^ "lt" ^ Char.escaped t | LEq -> "c" ^ s ^ "le" ^ Char.escaped t
    | Gt -> "c" ^ s ^ "gt" ^ Char.escaped t | GEq -> "c" ^ s ^ "ge" ^ Char.escaped t

    | Add -> "add" | Sub -> "sub"
    | Mul -> "mul" | Div -> "div"
    | Mod -> if signed then "mod" else "umod"

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

let generate_qbe_ir f path { procs; _ } =
    (* auxiliary functions for outputting different node types *)
    let output_loc loc =
        fprintf f ".loc %d, %d\n" loc.row loc.col
    in
    let rec output_data loc var_num (data : data_hl) =
        output_loc loc;

        (* TODO: high-level types *)
        match data with
        | Literal Const_str (off, len)
        | String (_, off, len) ->
                fprintf f "\t%%str_addr =w add $strs, %d\n" off;
                fprintf f "\t%%str_len  =w %d\n" len;

        | Literal Const_cstr off
        | CStr (_, off) ->
                fprintf f "\t%%str_addr =w add $strs, %d\n" off;

        | Literal Integer d ->
                fprintf f "\t%%v%d =w %d\n" var_num d;

        | Ptr (t_hl, var, off) ->
                output_data loc var_num @@ Primitive (Ptr (type_ll_of_type_hl t_hl, var, off))
        | Primitive d_ll -> (
                match d_ll with
                | Ptr (t_ll, var, off) ->
                    (* TODO: high-level types *)
                    let t_q = qbe_string_of_type_basety t_ll
                    and t_s = qbe_string_of_type_signed t_ll in
                    fprintf f "\t%%i%d =w add $%s, %d\n" var_num var off;
                    fprintf f "\t%%v%d =%c load%s %%i%d\n" var_num t_q t_s var_num
                | _ ->
                    let type_ll = type_of_data_ll d_ll in
                    let preamble = sprintf "%%v%d =%c " var_num (qbe_string_of_type_basety type_ll) in
                    (* TODO: high-level data *)
                    let data_qbe =
                        data
                        |> data_ll_of_data_hl
                        |> string_of_data_ll
                    in
                    fprintf f "\t%s %s\n" preamble data_qbe)

        | d -> raise @@ Not_implemented (loc, sprintf "IR generation not implemented for %s" (show_data_hl d))

    and output_operator loc var_num op (t_hl : type_hl list) =
        let op, t_hl =
            match t_hl with
            | [t_hl] -> qbe_string_of_operator op t_hl, t_hl
            | _ -> failwith @@ "multiple return types not supported on operators"
        in
        let t_ll = type_ll_of_type_hl t_hl in
        output_loc loc;
        fprintf f "\t%%v%d =%c %s\n" var_num (qbe_string_of_type_basety t_ll) op

    and output_proc_call loc types func =
        let _, args =
            let type_arg t i =
                let t = type_ll_of_type_hl t in
                sprintf "%c $arg%d" (qbe_string_of_type_extty t) i
            in
            match types with
            | [] -> 0, ""
            | hd :: tl -> List.fold_left (fun (i, acc) t -> i + 1, sprintf "%s, %s" acc (type_arg t i)) (1, type_arg hd 0) tl
        in

        output_loc loc;
        match types with
        | [] -> fprintf f "\tcall $%s(%s)\n" func.name args
        | _ -> raise @@ Not_implemented (loc, "procs with return types not yet implemented")
    in

    let rec output_node base_i node =
        if node.t = None then
            raise @@ Unreachable ("node has no type: " ^ show_node node)
        else
            if node.n = Empty then
                ()
            else
                let types_hl = Option.get node.t in
                match node.n with
                | Empty -> ()
                | Push_data { data } ->
                        output_data node.l base_i data
                | Op { op; left; right; _ } ->
                    begin
                        output_node base_i left;
                        output_node base_i right;
                        output_operator node.l base_i op types_hl
                    end
                | Proc_call { func; args } ->
                        let output_node i arg =
                            output_node (base_i + i) arg
                        in
                        List.iteri output_node args;
                        output_proc_call node.l types_hl func

                | n -> failwith @@ sprintf "IR generation of %s not yet supported" (show_node_hl n)
    in
    let output_seq base_i =
        List.iteri (fun i node -> output_node (base_i + i) node)
    in
    let output_proc _ { loc; name; types; seq; is_prototype; is_unused } = 
        if is_prototype || is_unused then
            ()
        else
            let varn = ref 0

            (* TODO: multiple return values, high-level types *)
            and t_out  = type_ll_of_type_hl (List.hd types.t_out)
            and t_in   = List.map type_ll_of_type_hl types.t_in in

            output_loc loc;
            fprintf f "export function %c $%s(%s) {\n" (qbe_string_of_type_extty t_out) name (qbe_string_of_args t_in);
            fprintf f "@start\n";

            output_seq !varn seq;

            fprintf f "}\n";

            varn := !varn + List.length seq
    in

    fprintf f "dbgfile \"%s\"\n\n" path;
    Hashtbl.iter output_proc procs;

    (* flush the channel *)
    fprintf f "%!";
