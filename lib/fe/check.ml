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

let compare t_exp t_act =
    if List.length t_exp <> List.length t_act then
        false
    else
        let compare = function
            (* TEMPORARY: special case for character -> u8 *)
            | (Primitive U8 : type_hl), (General Character, _) -> true
            | t_spec, (General t_gen, node_opt)
            | (General t_gen), (t_spec, node_opt) ->
                    (match node_opt with
                    | Some node -> node.t <- Some [t_spec]
                    | None -> ());
                    let type_gen =
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
                    type_gen = t_gen
            | t_exp, (t_act, _) ->
                    t_exp = t_act
        in
        List.for_all compare @@ List.combine t_exp t_act

let raise_unexpected_stack msg loc t_exp t_act =
    raise @@ Error (loc, sprintf "%s: expected %s, got %s" msg
        (string_of_types_hl t_exp)
        (string_of_types_hl t_act))


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
    let stack = stack_of_list stack in

    (* pop types from the stack and add them to takes *)
    let pop_to_takes loc names =
        let pop_to_takes' name =
            match Stack.pop_opt stack with
            | None -> raise @@ Error (loc, "not enough elements on the stack")
            | Some type_src -> Hashtbl.add takes name type_src
        in
        List.iter pop_to_takes' names

    (* peek types from the stack and add them to takes *)
    and peek_to_takes loc names =
        let peek_to_takes' (name, type_source) =
            Hashtbl.add takes name type_source
        in
        let stack = Stack.to_seq stack
        and names = List.to_seq names in
        if Seq.length stack < Seq.length names then
            raise @@ Error (loc, "not enough elements on the stack");

        Seq.zip names stack
        |> Seq.iter peek_to_takes'


    (* push a type from takes to stack *)
    and push_take node name =
        try
            let type_hl, node_opt = Hashtbl.find takes name in
            Stack.push (type_hl, node_opt) stack;
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
            check_seq input (list_of_stack stack) takes func.seq
            |> replace_stack stack
        else
            (* check function on the outside *)
            let t_in_exp = func.types.t_in
            and t_in_act = take_top (List.length func.types.t_in) stack in

            if not @@ compare t_in_exp t_in_act then
            begin
                let t_in_act = fst @@ List.split t_in_act in
                raise_unexpected_stack "input" loc t_in_exp t_in_act
            end;

            (* simulate calling the function *)
            if Stack.length stack < List.length func.types.t_in then
                raise @@ Error (loc, "not enough elements on the stack");
            List.iter (fun _ -> Stack.pop stack |> ignore) func.types.t_in;

            (* push return values *)
            let t_out = List.map (fun t -> t, None) func.types.t_out in
            List.iter (fun s -> Stack.push s stack) t_out;

    (* check an if statement *)
    and check_if_statement loc cond true_branch false_branch =
        let t_stack_before = list_of_stack stack in

        (* check that condition returns only bool *)
        let (t_stack_after : (type_hl * node option) list) = check_seq input t_stack_before takes [cond] in
        (match t_stack_after with
        | (Primitive Bool, _) :: tl when tl = t_stack_before -> ()
        | _ ->
                let t_stack_before = List.map fst t_stack_before
                and t_stack_after = List.map fst t_stack_after in
                raise_unexpected_stack "condition" loc ((Primitive Bool) :: t_stack_before) t_stack_after);

        (* check that true and false branches leave the same stack *)
        let t_stack_after_true  = check_seq input t_stack_before takes true_branch
        and t_stack_after_false = check_seq input t_stack_before takes false_branch in
        if List.map fst t_stack_after_true <> List.map fst t_stack_after_false then
            raise @@ Error (loc, "true and false branches must have the same return types\n");
        if List.length t_stack_after_true < List.length t_stack_before then
            raise @@ Error (loc, "branches must not drain the stack");

        replace_stack stack t_stack_after_true
    
    and check_assign node =
        let type_stack =
            try Stack.pop stack
            with _ -> raise @@ Error (node.l, "not enough elements on the stack")
        and type_mem =
            match node.n with
            | Assign_to_mem { name; _ } ->
                    Hashtbl.find input.mems name |> fst
            | Assign_to_var { name; _ } ->
                    Hashtbl.find input.vars name
            | _ -> raise @@ Unreachable ""
        in
        if not @@ compare [type_mem] [type_stack] then
            let t_exp = fst type_stack in
            raise_unexpected_stack "assign" node.l [t_exp] [type_mem]

    and check_operator node op left right =
        (* add stack offset *)
        node.id <- Some (Stack.length stack);

        let loc = node.l in
        let check_node' loc = function
            | { n = (Op _ | Push_data _ | Push_take _ | Proc_call _ | Macro_call _); _ } as node ->
                    check_node node
            | node ->
                    raise @@ Error (loc, sprintf "expected operand, got %s" (string_of_node node))
        and t_in_exp top = function
            | Eq | NEq | Lt | LEq | Gt | GEq -> Numeric

            | Add | Sub | Mul | Div | Mod
            | LAnd | LOr | LXor | Lsl | Lsr
            | Itof -> Integer

            | Ftoi -> Floating

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
                with _ -> raise @@ Not_implemented (right.l, sprintf "%s: no return type" (show_node_hl right.n)) in

            match left_t, right_t with
            | Generic t_genl, Generic t_genr ->
                    if t_genl <> t_genr then
                        raise_unexpected_stack (show_operator op) loc [left_t; left_t] [left_t; right_t]
            | Generic _, t_spec
            | t_spec, Generic _ ->
                    left.t  <- Some [t_spec];
                    right.t <- Some [t_spec];
            | t_specl, t_specr ->
                    if not @@ compare [t_specl] [t_specr, None] then
                        raise_unexpected_stack (show_operator op) loc [left_t; left_t] [left_t; right_t];
        end;

        let t_in_act =
            try Stack.pop stack |> fst
            with Stack.Empty ->
                raise @@ Error (loc, "not enough elements on the stack")
        in

        if right.n <> Empty then
            Stack.pop stack |> ignore;
        
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
                    raise_unexpected_stack "" loc [General t_in_exp] [General t_in_gen]
        end;

        let (t_out : type_hl) =
            match op with
            | Eq | NEq | Lt | LEq | Gt | GEq -> Primitive Bool
            | Add | Sub | Mul | Div | Mod -> t_in_act
            | Itof -> Primitive (if t_in_act = Primitive I32 then F32 else F64)
            | Ftoi -> Primitive (if t_in_act = Primitive F32 then I32 else I64)
            | LAnd | LOr | LXor | Lsl | Lsr -> t_in_act
            | And | Or -> Primitive Bool
            | Ref -> Ptr t_in_act
            | Deref ->
                    match t_in_act with Ptr t -> t
                    | _ -> raise @@ Error (loc, sprintf "cannot deref %s" (string_of_type_hl t_in_act))
        in

        Stack.push (t_out, Some node) stack;
        node.t <- Some [t_out]

    and check_node node =
        node.id <- Some (Stack.length stack);

        match node.n with
        | Empty -> ()
        | Take { vars } -> pop_to_takes node.l vars
        | Peek { vars } -> peek_to_takes node.l vars
        | Scoped_take { vars; body }
        | Scoped_peek { vars; body } ->
                (match node.n with
                | Scoped_take _ ->
                        pop_to_takes node.l vars
                | _ ->
                        peek_to_takes node.l vars);

                check_seq input (list_of_stack stack) takes body
                |> replace_stack stack

        | Push_take { name; _ } ->
                push_take node name
        | Push_data { data; _ } ->
                push_literal data node
        | Proc_call { func; args }
        | Macro_call { func; args } ->
                check_func_call node.l func args

        | Assign_to_mem { value; _ }
        | Assign_to_var { value; _ } ->
                check_node value;
                check_assign node

        | If_statement { cond; true_branch; false_branch } ->
                check_if_statement node.l cond true_branch false_branch

        | Op { op; left; right } ->
                check_operator node op left right

        | Unknown_sequence _ ->
                (* TODO: check the types on stack *)
                ()
        | Var _ | Mem _ ->
                raise @@ Unreachable "out of place var/mem should be handled in Parser"
        (* TEMPORARY *)
        | _ -> raise @@ Not_implemented (node.l, sprintf "checking %s not implemented yet" (show_node_hl node.n))
    in

    List.iter check_node seq;
    list_of_stack stack


let check input =
    let check_func { loc; seq; types; is_signature; _ } =
        if is_signature then ()
        else
            let takes = Hashtbl.create 10 in
            let t_in_act = List.map (fun t -> t, None) types.t_in in
            let t_out_act = check_seq input t_in_act takes seq in

            if not @@ compare types.t_out t_out_act then
                let t_out_act = List.map fst t_out_act in
                raise_unexpected_stack "output" loc types.t_out t_out_act
    in

    Hashtbl.iter (fun _ f -> check_rec_macro f) input.macros;
    Hashtbl.iter (fun _ f -> check_func f) input.macros;
    Hashtbl.iter (fun _ f -> check_func f) input.procs;

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
