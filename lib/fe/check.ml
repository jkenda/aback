open Format

open Common
open Parser_types

let list_of_stack stack =
    stack
    |> Stack.to_seq
    |> List.of_seq

let stack_of_list list =
    list
    |> List.rev
    |> List.to_seq
    |> Stack.of_seq

let replace_stack stack types_hl =
    Stack.clear stack;
    types_hl
    |> List.rev
    |> List.to_seq
    |> Stack.add_seq stack

let take_top n stack =
    Stack.to_seq stack
    |> Seq.take n
    |> List.of_seq

let add_none =
    List.map (fun t -> t, None)

let compare t_exp t_act =
    if List.length t_exp <> List.length t_act then
        false
    else
        let compare' = function
            (* TEMPORARY: special case for character -> u8 *)
            | (t_spec, _), (Generic _, node_opt)
            | (Generic _, node_opt), (t_spec, _) ->
                    (match node_opt with
                    | Some node -> node.t <- Some [t_spec]
                    | None -> ());
                    true

            | (t_spec, _), (General t_gen, node_opt)
            | (General t_gen, node_opt), (t_spec, _) ->
                    (match node_opt with
                    | Some node -> node.t <- Some [t_spec]
                    | None -> ());
                    let t_gen_of_spec =
                        try type_gen_of_type_hl t_spec
                        with _ ->
                            match node_opt with
                            | Some node ->
                                    raise @@ Not_implemented (node.l, sprintf "node %s doesn't have type: %s"
                                        (show_node_hl node.n)
                                        (show_type_hl t_spec))
                            | None ->
                                    failwith @@ sprintf "node doesn't have type: %s" (show_type_hl t_spec)
                    in
                    t_gen_of_spec = t_gen

            | (t_exp, _), (t_act, _) ->
                    t_exp = t_act
        in
        List.for_all compare' @@ List.combine t_exp t_act

let raise_unexpected_stack msg loc t_exp t_act =
    raise @@ Error (loc, sprintf "%s: expected %s, got %s" msg
        (string_of_types_hl t_exp)
        (string_of_types_hl t_act))

let raise_not_enough_elements loc =
    raise @@ Error (loc, "not enough elements on the stack")


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

let rec check_seq caller input stack takes seq =
    let stack = stack_of_list stack in

    (* pop types from the stack and add them to takes *)
    let pop_to_takes node names =
        let pop_to_takes' name =
            let loc =
                match caller with
                | Some l -> { node.l with expanded_from = l :: node.l.expanded_from }
                | None -> node.l
            in
            match Stack.pop_opt stack with
            | None -> raise_not_enough_elements loc
            | Some type_src ->
                    Hashtbl.add takes name type_src;
                    fst type_src
        in
        node.t <- Some (List.map pop_to_takes' names)

    (* peek types from the stack and add them to takes *)
    and peek_to_takes node names =
        let peek_to_takes' (name, type_source) =
            Hashtbl.add takes name type_source
        in
        let names = List.to_seq names
        and loc =
            match caller with
            | Some l -> { node.l with expanded_from = l :: node.l.expanded_from }
            | None -> node.l
        in
        if Stack.length stack < Seq.length names then
            raise_not_enough_elements loc;

        Seq.zip names (Stack.to_seq stack)
        |> Seq.iter peek_to_takes';
        node.t <- Some (List.map fst @@ take_top (Seq.length names) stack)


    (* push a type from takes to stack *)
    and push_take node name =
        try
            let type_hl, node_opt = Hashtbl.find takes name in
            Stack.push (type_hl, node_opt) stack;
            node.t <- Some [type_hl]
        with Not_found ->
            raise @@ Error (node.l, sprintf "take %s not found" name)
    and push_mem node name =
        try
            let type_hl, _len = Hashtbl.find input.mems name in
            Stack.push (type_hl, Some node) stack;
            node.t <- Some [type_hl]
        with Not_found ->
            raise @@ Error (node.l, sprintf "take %s not found" name)
    and push_var node name =
        try
            let type_hl = Hashtbl.find input.vars name in
            Stack.push (type_hl, Some node) stack;
            node.t <- Some [type_hl]
        with Not_found ->
            raise @@ Error (node.l, sprintf "take %s not found" name)

    and push_literal data node =
        let type_hl = type_of_data_hl data in
        Stack.push (type_hl, Some node) stack

    in

    (* handle a proc call *)
    let rec check_func_call loc func args =
        let is_generic = function
            | Generic _ -> true
            | _ -> false
        in

        (* push args *)
        List.iter check_node @@ List.rev args;

        if List.exists is_generic func.types.t_in then
            (* check function on the inside *)
            check_seq (Some (loc, "")) input (list_of_stack stack) takes func.seq
            |> replace_stack stack
        else
            (* check function on the outside *)
            let t_in_exp = func.types.t_in
            and t_in_act = take_top (List.length func.types.t_in) stack in

            if not @@ compare (add_none t_in_exp) t_in_act then
            begin
                let t_in_act = fst @@ List.split t_in_act in
                raise_unexpected_stack "input" loc t_in_exp t_in_act
            end;

            (* simulate calling the function *)
            if Stack.length stack < List.length func.types.t_in then
                raise_not_enough_elements loc;
            List.iter (fun _ -> Stack.pop stack |> ignore) func.types.t_in;

            (* push return values *)
            let t_out = List.map (fun t -> t, None) func.types.t_out in
            List.iter (fun s -> Stack.push s stack) t_out;

    (* check an if statement *)
    and check_if_statement loc cond true_branch false_branch =
        let t_stack_before = list_of_stack stack in

        (* check that condition returns only bool *)
        let (t_stack_after : (type_hl * node option) list) = check_seq caller input t_stack_before takes [cond] in
        (match t_stack_after with
        | (Primitive Bool, _) :: tl when tl = t_stack_before -> ()
        | _ ->
                let t_stack_before = List.map fst t_stack_before
                and t_stack_after = List.map fst t_stack_after in
                raise_unexpected_stack "condition" loc ((Primitive Bool) :: t_stack_before) t_stack_after);

        (* check that true and false branches leave the same stack *)
        let t_stack_after_true  = check_seq caller input t_stack_before takes true_branch
        and t_stack_after_false = check_seq caller input t_stack_before takes false_branch in
        if not @@ compare t_stack_after_true t_stack_after_false then
        begin
            let msg =
                sprintf "true and false branches must have the same return types\n\ttrue: %s\n\tfalse: %s"
                (string_of_types_hl (List.map fst t_stack_after_true))
                (string_of_types_hl (List.map fst t_stack_after_false))
            in
            raise @@ Error (loc, msg)
        end;
        if List.length t_stack_after_true < List.length t_stack_before then
            raise @@ Error (loc, "branches must not shrink the stack");

        replace_stack stack t_stack_after_true

    and check_while_statement loc cond body =
        let t_stack_before = list_of_stack stack in

        (* check that condition returns only bool *)
        let (t_stack_after : (type_hl * node option) list) = check_seq caller input t_stack_before takes [cond] in
        (match t_stack_after with
        | (Primitive Bool, _) :: tl when tl = t_stack_before -> ()
        | _ ->
                let t_stack_before = List.map fst t_stack_before
                and t_stack_after = List.map fst t_stack_after in
                raise_unexpected_stack "condition" loc ((Primitive Bool) :: t_stack_before) t_stack_after);

        let t_stack_after_body  = check_seq caller input t_stack_before takes body in
        if List.length t_stack_after_body <> List.length t_stack_before then
            raise @@ Error (loc, "loops must not grow or shrink");
    
    and check_assign node val_node =
        check_node val_node;
        let t_act =
            try Stack.pop stack
            with _ -> raise_not_enough_elements node.l;
        and t_exp =
            match node.n with
            | Assign_to_mem { name; _ } ->
                    Hashtbl.find input.mems name |> fst
            | Assign_to_var { name; _ } ->
                    Hashtbl.find input.vars name
            | _ -> raise @@ Unreachable ""
        in
        if not @@ compare [t_exp, None] [t_act] then
            let t_act = fst t_act in
            raise_unexpected_stack "assign" val_node.l [t_exp] [t_act]

    and check_operator node op left right =
        (* add stack offset *)
        node.id <- Some (Stack.length stack);

        let loc = node.l in
        let check_node' loc node =
            if is_node_operand node.n then
                check_node node
            else
                raise @@ Error (loc, sprintf "expected operand, got %s" (show_node_hl node.n))
        and t_in_exp top = function
            | Eq | NEq | Lt | LEq | Gt | GEq
            | Add | Sub | Mul | Div | Mod -> Numeric

            | LAnd | LOr | LXor | Lsl | Lsr -> Integer

            | Cast_to _ -> type_gen_of_type_hl top

            | And | Or -> Boolean
            | Ref -> type_gen_of_type_hl top
            | Deref ->
                    match type_gen_of_type_hl top with
                    | Pointer t -> t
                    | _ -> raise @@ Error (loc, sprintf "cannot deref %s" (string_of_type_hl top))
        in

        if left.n = Empty && right.n = Empty then
            raise @@ Error (loc, "expected at least one operand")
        else if n_operands op = 2 && right.n == Empty then
            raise @@ Error (loc, "expected 2 operands, got 1");

        (* check operands *)
        if left.n  <> Empty then check_node' loc left;
        if right.n <> Empty then check_node' loc right;

        if left.n <> Empty && right.n <> Empty then
        begin
            let left_t =
                try List.hd @@ Option.get left.t
                with _ -> raise @@ Not_implemented (left.l, sprintf "%s: no return type" (show_node_hl left.n))
            and right_t =
                try List.hd @@ Option.get right.t
                with _ -> raise @@ Not_implemented (right.l, sprintf "%s: no return type" (show_node_hl right.n))
            in

            match left_t, right_t with
            | Generic t_genl, Generic t_genr ->
                    if t_genl <> t_genr then
                        raise_unexpected_stack (show_operator op) loc [left_t; left_t] [left_t; right_t]
            | Generic _, t_spec
            | t_spec, Generic _ ->
                    left.t  <- Some [t_spec];
                    right.t <- Some [t_spec];
            | (Primitive Ptr _ | Ptr _ as ptr), (Primitive U64 as off) ->
                    left.t  <- Some [ptr];
                    right.t <- Some [off];
                    node.t  <- Some [ptr]
            | t_specl, t_specr ->
                    if not @@ compare [t_specl, Some left] [t_specr, Some right] then
                        raise_unexpected_stack (show_operator op) loc [left_t; left_t] [left_t; right_t];
        end;

        let t_in_act =
            if right.n <> Empty then
                match fst @@ Stack.pop stack, fst @@ Stack.pop stack with
                | (General _ | Generic _), t_spec
                | t_spec, (General _ | Generic _) ->
                        t_spec
                | tl, _tr -> tl
            else
                try Stack.pop stack |> fst
                with Stack.Empty -> raise_not_enough_elements loc;
        in

        
        begin
            match t_in_act with
            | Generic _ -> 
                    left.t  <- Some [General (t_in_exp t_in_act op)];
                    right.t <- Some [General (t_in_exp t_in_act op)];
            | _ ->
                let t_in_gen =
                    try type_gen_of_type_hl t_in_act
                    with Failure str -> raise @@ Error (loc, str)
                in
                let t_in_exp = t_in_exp t_in_act op in
                if not @@ type_gen_eq t_in_gen t_in_exp  then
                    raise_unexpected_stack (show_operator op) loc [General t_in_exp] [General t_in_gen]
        end;

        let (t_out : type_hl) =
            match op with
            | Eq | NEq | Lt | LEq | Gt | GEq -> Primitive Bool
            | Add | Sub | Mul | Div | Mod -> t_in_act
            | Cast_to t -> Primitive (type_ll_of_type_tok t)
            | LAnd | LOr | LXor | Lsl | Lsr -> t_in_act
            | And | Or -> Primitive Bool
            | Ref -> Ptr t_in_act
            | Deref ->
                    match t_in_act with Ptr t -> t
                    | _ -> raise @@ Error (loc, sprintf "cannot deref %s" (string_of_type_hl t_in_act))
        in

        let t_out =
            match node.t with
            | Some [t] -> t
            | _ -> t_out
        in
        Stack.push (t_out, Some node) stack;
        node.t <- Some [t_out]

    and check_node node =
        node.id <- Some (Stack.length stack);

        match node.n with
        | Empty -> ()
        | Take { vars } -> pop_to_takes node vars
        | Peek { vars } -> peek_to_takes node vars
        | Scoped_take { vars; body }
        | Scoped_peek { vars; body } ->
                (match node.n with
                | Scoped_take _ ->
                        pop_to_takes node vars
                | _ ->
                        peek_to_takes node vars);

                check_seq caller input (list_of_stack stack) takes body
                |> replace_stack stack

        | Push_data data ->
                push_literal data node
        | Push_take name ->
                push_take node name
        | Push_var  name ->
                push_var node name
        | Push_mem  name ->
                push_mem node name

        | Proc_call { func; args }
        | Macro_call { func; args } ->
                check_func_call node.l func args

        | Assign_to_mem { value; _ }
        | Assign_to_var { value; _ } ->
                check_assign node value
        | If_statement { cond; true_branch; false_branch } ->
                check_if_statement node.l cond true_branch false_branch
        | While_statement { cond; body } ->
                check_while_statement node.l cond body

        | Op { op; left; right } ->
                check_operator node op left right

        | Unknown_sequence { length } ->
                if Stack.length stack < length then
                    raise_not_enough_elements node.l;
                node.t <- Some (take_top length stack |> List.map fst)

        | Var _ | Mem _ ->
                raise @@ Unreachable "out of place var/mem should be handled in parser"
        (* TEMPORARY *)
        | _ -> raise @@ Not_implemented (node.l, sprintf "checking %s not implemented yet" (show_node_hl node.n))
    in

    List.iter check_node seq;
    list_of_stack stack


let check options input =
    let check_func { loc; seq; types; is_signature; _ } =
        if is_signature then ()
        else
            let takes = Hashtbl.create 10 in
            let t_in_act = List.map (fun t -> t, None) types.t_in in
            let t_out_act = check_seq None input t_in_act takes seq in

            if not @@ compare (add_none types.t_out) t_out_act then
                let t_out_act = List.map fst t_out_act in
                raise_unexpected_stack "output" loc types.t_out t_out_act
    in

    if not @@ List.mem No_check options.flags then
    begin
        Hashtbl.iter (fun _ f -> check_rec_macro f) input.macros;
        Hashtbl.iter (fun _ f -> check_func f) input.macros;
        Hashtbl.iter (fun _ f -> check_func f) input.procs
    end;

    let output = input in
    output


type ints = int list
[@@deriving show { with_path = false }]

let%test "list -> stack -> list" =
    let expect_eq a b =
        if a <> b then
            printf "expected %s, got %s\n" (show_ints a) (show_ints b);
        a = b
    in

    let list = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    expect_eq
    list
    (list |> stack_of_list |> list_of_stack)

let%test "stack -> list -> stack" =
    let stack = Stack.of_seq @@ List.to_seq [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    stack |> list_of_stack |> stack_of_list = stack

let%test "stack -> list; replace_stack" =
    let expected = Stack.of_seq @@ List.to_seq [1; 2; 3; 4; 5; 6; 7; 8; 9]
    and actual = Stack.create () in

    let list = list_of_stack expected in
    replace_stack actual list;
    expected = actual
