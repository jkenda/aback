open Format

open Common
open Parser_types


(** check whether the function is recursive or not *)
let is_recursive { name; seq; _ } =
    let rec is_recursive' = function
        | [] -> false
        | Macro_call { func; _ } :: _
        | Proc_call { func; _ } :: _
            when func.name = name ->
                true
        | Op { left; right; _ } :: _ ->
                is_recursive' [left] || is_recursive' [right]
        | If_statement { cond; true_branch; false_branch; _ } :: _ ->
                is_recursive' [cond] || is_recursive' true_branch || is_recursive' false_branch
        | While_statement { cond; body; _ } :: _ ->
                is_recursive' [cond] || is_recursive' body
        | _ :: tl -> is_recursive' tl
    in
    is_recursive' seq

(** refuse recursive macros *)
let check_rec_macro ({ loc; _ } as func : func) =
    if is_recursive func then
        raise @@ Error (loc, "recursive macro")

let rec check_seq input stack takes seq =
    let stack = Stack.of_seq @@ List.to_seq stack in

    (* pop types from the stack and add them to takes *)
    let pop_to_takes loc names =
        let pop_to_takes' name =
            match Stack.pop_opt stack with
            | None -> raise @@ Error (loc, "not enough elements on the stack")
            | Some typ -> Hashtbl.add takes name typ
        in
        List.iter pop_to_takes' names

    (* peek types from the stack and add them to takes *)
    and peek_to_takes loc names =
        let pop_to_takes' stack name =
            match Seq.uncons stack with
            | None -> raise @@ Error (loc, "not enough elements on the stack")
            | Some (typ, tl) -> Hashtbl.add takes name typ; tl
        in
        let stack = Stack.to_seq stack in
        List.fold_left pop_to_takes' stack names ()
        |> ignore

    (* push a type from takes to stack *)
    and push_take_to_stack loc name =
        try
            Stack.push (Hashtbl.find takes name) stack
        with Not_found ->
            raise @@ Error (loc, sprintf "take %s not found" name)

    (* handle a proc call *)
    and handle_proc_call loc func args =
        let t_out_actual = check_seq input func.types.t_in takes seq in
        List.iter (fun typ -> Stack.push typ stack) t_out_actual

    (* handle a macro call *)
    and handle_macro_call loc func args =
        let t_out_actual = check_seq input func.types.t_in takes seq in
        List.iter (fun typ -> Stack.push typ stack) t_out_actual

    and handle_syscall loc number args =
        let pop_int _ =
            match Stack.pop_opt stack with
            | Some Primitive Int -> ()
            | Some _ -> raise @@ Error (loc, "expected int")
            | None -> raise @@ Error (loc, "not enough elements on the stack")
        in
        Seq.iter pop_int (Seq.ints number)
    in

    let check' = function
        | Take { loc; vars } ->
                pop_to_takes loc vars
        | Peek { loc; vars } ->
                peek_to_takes loc vars
        | Push_take { loc; name } ->
                push_take_to_stack loc name
        | Proc_call { loc; func; args } ->
                handle_proc_call loc func args
        | Macro_call { loc; func; args } ->
                handle_macro_call loc func args
        | Syscall { loc; number; args } ->
                handle_syscall loc number args

        | node ->
                failwith ("checking " ^ show_node node ^ " not yet implemented")
    in
    List.iter check' seq;
    Stack.to_seq stack
    |> List.of_seq


let check input =

    let check_func { loc; seq; types; ncalls = _; _ } =
        let takes = Hashtbl.create 0 in
        let t_out_actual = check_seq input types.t_in takes seq in

        if t_out_actual <> types.t_out then
            raise @@ Error (loc, "type mismatch")
    in

    Hashtbl.iter (fun _ f -> check_rec_macro f) input.macros;
    Hashtbl.iter (fun _ f -> check_func f) input.macros;
    Hashtbl.iter (fun _ f -> check_func f) input.procs
