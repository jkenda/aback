open Format

open Common
open Parser_types

(** check whether the function is recursive or not *)
let is_recursive { name; seq; _ } =
    let rec is_recursive' = function
        | [] -> false
        | { n = Macro_call { func; _ }; _} :: _
        | { n = Proc_call { func; _ }; _} :: _
            when func.name = name ->
                true
        | { n = Op { left; right; _ }; _ } :: _ ->
                is_recursive' [left] || is_recursive' [right]
        | { n = If_statement { cond; true_branch; false_branch; _ }; _ } :: _ ->
                is_recursive' [cond] || is_recursive' true_branch || is_recursive' false_branch
        | { n = While_statement { cond; body; _ }; _ } :: _ ->
                is_recursive' [cond] || is_recursive' body
        | _ :: tl -> is_recursive' tl
    in
    is_recursive' seq

(** refuse recursive macros *)
let check_rec_macro ({ loc; _ } as func : func) =
    if is_recursive func then
        raise @@ Error (loc, "recursive macro")

let rec check_seq input stack takes seq =
    let (stack : type_hl Stack.t) = Stack.of_seq @@ List.to_seq stack in

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
        let type_hl = type_of_data_hl data in
        Stack.push type_hl stack

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
            | { n = (Op _ | Push_data _ | Proc_call _ | Macro_call _); _ } as node ->
                    check_node node
            | node ->
                    raise @@ Error (loc, sprintf "expected operand, got %s" (string_of_node node))
        and expected_typ i top op =
            let (typ : type_gen) =
                match op with
                | Eq | NEq | Lt | LEq | Gt | GEq ->
                        (if i = 0 then Integer else Floating) 

                | Add | Sub | Mul | Div | Mod
                | LAnd | LOr | LXor | Lsl | Lsr
                | Itof -> Integer

                | FAdd | FSub | FMul | FDiv
                | Ftoi -> Integer

                | And  | Or -> Boolean
                | Putc -> Character
                | Puts -> String
                | Ref -> top
                | Deref ->
                        match top with
                        | Pointer t -> t
                        | _ -> raise @@ Error (loc, "can only deref pointer")
            in
            List.init n_operands (fun _ -> typ)
        and get_typ _ =
            try Stack.pop stack |> type_gen_of_type_hl
            with Stack.Empty ->
                raise @@ Error (loc, "not enough elements on the stack")
        in

        if left.n = Empty && right.n = Empty then
            raise @@ Error (loc, "expected at least one operand")
        else if n_operands = 2 && right.n == Empty then
            raise @@ Error (loc, "expected 2 operands, got 1");

        if left.n   <> Empty then check_node' loc left;
        if right.n  <> Empty then check_node' loc right;

        let types = List.init n_operands get_typ in
        let expected_0 = expected_typ 0 (List.hd types) op
        and expected_1 = expected_typ 1 (List.hd types) op in

        if not (expected_0 = types || expected_1 = types)   then
            if expected_0 = expected_1 then
                raise @@ Error (loc, sprintf "expected %s, got %s"
                    (string_of_types_gen expected_0) (string_of_types_gen types))
            else
                raise @@ Error (loc, sprintf "expected %s or %s, got %s"
                    (string_of_types_gen expected_0) (string_of_types_gen expected_1) (string_of_types_gen types));

        ()

    and check_node node =
        match node.n with
        | Empty ->
                ()
        | Take { vars } ->
                pop_to_takes node.l vars
        | Peek { vars } ->
                peek_to_takes node.l vars
        | Push_take { name } ->
                push_take node.l name
        | Push_data { data; _ } ->
                push_literal  data
        | Proc_call { func; args } ->
                handle_proc_call node.l func args
        | Macro_call { func; args } ->
                handle_macro_call node.l func args

        | If_statement { cond; true_branch; false_branch } ->
                check_if_statement node.l cond true_branch false_branch

        | Op { op; left; right } ->
                check_operator node.l op left right

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
        let compare t_exp t_act =
            let compare acc = function
                | t_exp, General t_act ->
                        acc && type_gen_of_type_hl t_exp = t_act
                | t_exp, t_act ->
                        acc && t_exp = t_act
            in
            List.fold_left compare true @@ List.combine t_exp t_act
        in

        let takes = Hashtbl.create 0 in
        let t_out_actual = check_seq input types.t_in takes seq in

        if not (compare types.t_out t_out_actual) then
            raise @@ Error (loc, sprintf "expected %s, got %s" (string_of_types_hl types.t_out) (string_of_types_hl t_out_actual))
        else
            print_endline @@ string_of_types_hl t_out_actual
    in

    Hashtbl.iter (fun _ f -> check_rec_macro f) input.macros;
    Hashtbl.iter (fun _ f -> check_func f) input.macros;
    Hashtbl.iter (fun _ f -> check_func f) input.procs;

    let output = input in
    output
