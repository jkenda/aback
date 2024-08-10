open Format
open Common
open Parser_types

let qbe_typ_op_typ = function
    | Primitive Int    -> "w"
    | Primitive Float  -> "s"
    | Primitive Char   -> "b"
    | Primitive Bool   -> "b"
    | Primitive Ptr    -> "l"
    | Primitive String -> "l"
    | Primitive CStr   -> "l"
    | Struc _          -> "l"
    | Union _          -> "l"
    | Ptr _            -> "l"

let qbe_string_of_args =
    List.fold_left (fun s t -> s ^ ", " ^ qbe_typ_op_typ t) ""

let generate_qbe_ir formatter { procs; _ } =
    let output_proc _ { loc; name; types; _ } = (
        fprintf formatter ".loc %d, %d\n" loc.col loc.row;
        fprintf formatter "export function %s $%s(%s) {\n" (qbe_typ_op_typ (List.hd types.t_out)) name (qbe_string_of_args types.t_in);
        fprintf formatter "@start\n";
        fprintf formatter "}\n")
    in

    Hashtbl.iter output_proc procs;
    ()
