open Format

open Common
open Lexer
open Parser_types

let typ_of_data = Preprocess.typ_of_data

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
    and push_take loc name =
        try
            Stack.push (Hashtbl.find takes name) stack
        with Not_found ->
            raise @@ Error (loc, sprintf "take %s not found" name)

    and push_literal data =
        Stack.push (Primitive (typ_of_data data)) stack

    (* handle a proc call *)
    and handle_proc_call _loc func _args =
        (* TODO: handle arguments *)
        let t_out_actual = check_seq input func.types.t_in takes seq in
        List.iter (fun typ -> Stack.push typ stack) t_out_actual

    (* handle a macro call *)
    and handle_macro_call _loc func _args =
        (* TODO: handle arguments *)
        let t_out_actual = check_seq input func.types.t_in takes seq in
        List.iter (fun typ -> Stack.push typ stack) t_out_actual

    (* check an if statement *)
    and check_if_statement loc cond true_branch false_branch =
        let t_stack_before = List.of_seq @@ Stack.to_seq stack
        and seq = [cond] in

        let t_stack_after = check_seq input t_stack_before takes seq in
        if t_stack_after <> Primitive Bool :: t_stack_before then
            raise @@ Error (loc, "condition must only return bool");

        let t_stack_before = t_stack_after in

        let t_stack_after_true  = check_seq input t_stack_before takes true_branch
        and t_stack_after_false = check_seq input t_stack_before takes false_branch in
        if t_stack_after_true <> t_stack_after_false then
            raise @@ Error (loc, "true and false branches must have the same return types");
        if List.length t_stack_after_true < List.length t_stack_before then
            raise @@ Error (loc, "branches must not drain the stack");

        Stack.clear stack;
        List.iter (fun typ -> Stack.push typ stack) @@ List.rev t_stack_after_true
    in

    let rec check_operator loc op left right =
        let n_operands =
            match op with
            | Itof | Ftoi
            | Ref  | Deref
            | Putc | Puts
                -> 1
            | _ -> 2
        in
        let check_node' loc = function
            | (Op _ | Push_literal _ | Proc_call _ | Macro_call _) as node ->
                    check_node node
            | node ->
                    raise @@ Error (loc, sprintf "expected operand, got %s" (string_of_node node))
        and expected_typ i top op =
            let typ =
                match op with
                | Eq | NEq | Lt | LEq | Gt | GEq ->
                        Primitive (if i = 0 then Int else Float) 

                | Add | Sub | Mul | Div | Mod
                | LAnd | LOr | LXor | Lsl | Lsr
                | Itof -> Primitive Int

                | FAdd | FSub | FMul | FDiv
                | Ftoi -> Primitive Float

                | And  | Or -> Primitive Bool
                | Putc -> Primitive Char
                | Puts -> Primitive String
                | Ref -> top
                | Deref ->
                        match top with
                        | Ptr t -> t
                        | _ -> raise @@ Error (loc, "can only deref pointer")
            in
            List.init n_operands (fun _ -> typ)
        and get_typ _ =
            try Stack.pop stack
            with Stack.Empty ->
                raise @@ Error (loc, "not enough elements on the stack")
        in

        if left == Empty && right == Empty then
            raise @@ Error (loc, "expected at least one operand")
        else if n_operands = 2 && right == Empty then
            raise @@ Error (loc, "expected 2 operands, got 1");

        if right <> Empty then check_node' loc right;
        if left  <> Empty then check_node' loc left;

        let types = List.init n_operands get_typ in
        let expected_0 = expected_typ 0 (List.hd types) op
        and expected_1 = expected_typ 1 (List.hd types) op in

        if expected_0 <> types && expected_1 <> types then
            if expected_0 = expected_1 then
                raise @@ Error (loc, sprintf "expected %s, got %s"
                    (string_of_typs expected_0) (string_of_typs types))
            else
                raise @@ Error (loc, sprintf "expected %s or %s, got %s"
                    (string_of_typs expected_0) (string_of_typs expected_1) (string_of_typs types));

        ()

    and check_node node =
        match node with
        | Empty ->
                ()
        | Take { loc; vars } ->
                pop_to_takes loc vars
        | Peek { loc; vars } ->
                peek_to_takes loc vars
        | Push_take { loc; name } ->
                push_take loc name
        | Push_literal { data; _ } ->
                push_literal data
        | Proc_call { loc; func; args } ->
                handle_proc_call loc func args
        | Macro_call { loc; func; args } ->
                handle_macro_call loc func args

        | If_statement { loc; cond; true_branch; false_branch } ->
                check_if_statement loc cond true_branch false_branch

        | Op { loc; op; left; right } ->
                check_operator loc op left right

        | Var _ | Mem _ ->
                raise @@ Unreachable "out of place var/mem should be handled in Parser"
        (* TEMPORARY *)
        | _ -> failwith @@ sprintf "checking %s not implemented yet" (string_of_node node)
    in
    List.iter check_node seq;
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
    Hashtbl.iter (fun _ f -> check_func f) input.procs;
    ()
