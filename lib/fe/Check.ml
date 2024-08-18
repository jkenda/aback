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
            | Some typ -> Hashtbl.add takes name typ
        in
        List.iter pop_to_takes' names

    (* peek types from the stack and add them to takes *)
    and peek_to_takes loc names =
        let peek_to_takes' (name, typ) =
            Hashtbl.add takes name typ
        in
        let stack = Stack.to_seq stack
        and names = List.to_seq names in
        if Seq.length stack < Seq.length names then
            raise @@ Error (loc, "not enough elements on the stack");

        Seq.zip names stack
        |> Seq.iter peek_to_takes'


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
        let t_out_actual = check_seq input (list_of_stack stack) takes func.seq in
        replace_stack stack t_out_actual

    (* handle a macro call *)
    and handle_macro_call _loc func _args =
        (* TODO: handle arguments *)
        let t_out_actual = check_seq input (list_of_stack stack) takes func.seq in
        replace_stack stack t_out_actual

    (* check an if statement *)
    and check_if_statement loc cond true_branch false_branch =
        let t_stack_before = list_of_stack stack in

        (* check that condition returns only bool *)
        let t_stack_after = check_seq input t_stack_before takes [cond] in
        if t_stack_after <> Primitive Bool :: t_stack_before then
            raise @@ Error (loc, "condition must only return bool");

        let t_stack_before = t_stack_after in

        (* check that true and false branches leave the same stack *)
        let t_stack_after_true  = check_seq input t_stack_before takes true_branch
        and t_stack_after_false = check_seq input t_stack_before takes false_branch in
        if t_stack_after_true <> t_stack_after_false then
            raise @@ Error (loc, "true and false branches must have the same return types");
        if List.length t_stack_after_true < List.length t_stack_before then
            raise @@ Error (loc, "branches must not drain the stack");

        replace_stack stack t_stack_after_true
    in

    let rec check_operator node op left right =
        let loc = node.l in
        let n_operands =
            match op with
            | Itof | Ftoi
            | Ref  | Deref
                -> 1
            | _ -> 2
        in
        let check_node' loc = function
            | { n = (Op _ | Push_data _ | Proc_call _ | Macro_call _); _ } as node ->
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
            | Ref -> top
            | Deref ->
                    match top with
                    | Pointer t -> t
                    | _ -> raise @@ Error (loc, "can only deref pointer")
        in

        if left.n = Empty && right.n = Empty then
            raise @@ Error (loc, "expected at least one operand")
        else if n_operands = 2 && right.n == Empty then
            raise @@ Error (loc, "expected 2 operands, got 1");

        if left.n  <> Empty then check_node' loc left;
        if right.n <> Empty then check_node' loc right;

        if left.n <> Empty && right.n <> Empty && left.t <> right.t then
            raise @@ Error (loc, "operands must have the same type");

        let t_in_act =
            try Stack.pop stack
            with Stack.Empty ->
                raise @@ Error (loc, "not enough elements on the stack")
        in
        let t_in_gen = type_gen_of_type_hl t_in_act in
        let t_in_exp = t_in_exp t_in_gen op in
        if t_in_gen <> t_in_exp  then
                raise @@ Error (loc, sprintf "expected %s, got %s"
                    (string_of_type_gen t_in_exp) (string_of_type_hl t_in_act));

        let (t_out : type_hl option) =
            match op with
            | Eq | NEq | Lt | LEq | Gt | GEq -> Some (Primitive Bool)
            | Add | Sub | Mul | Div | Mod -> Some (t_in_act)
            | Itof -> Some (Primitive (if t_in_act = Primitive I32 then F32 else F64))
            | Ftoi -> Some (Primitive (if t_in_act = Primitive F32 then I32 else I64))
            | LAnd | LOr | LXor | Lsl | Lsr -> Some t_in_act
            | And | Or -> Some (Primitive Bool)
            | Ref -> Some (Ptr (t_in_act))
            | Deref ->
                    match t_in_act with Ptr t -> Some t
                    | _ -> raise @@ Error (loc, sprintf "cannot deref %s" (string_of_type_hl t_in_act))
        in

        match t_out with Some t -> Stack.push t stack
        | None ->();

        node.t <- Option.map (fun t -> [t]) t_out

    and check_node node =
        match node.n with
        | Empty ->
                ()
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
                check_operator node op left right

        | Var _ | Mem _ ->
                raise @@ Unreachable "out of place var/mem should be handled in Parser"
        (* TEMPORARY *)
        | _ -> failwith @@ sprintf "checking %s not implemented yet" (show_node_hl node.n)
    in
    List.iter check_node seq;
    list_of_stack stack


let check input =
    let check_func { loc; seq; types; is_prototype; _ } =
        if is_prototype then ()
        else
            let compare t_exp t_act =
                if List.length t_exp <> List.length t_act then
                    false
                else
                    let compare acc = function
                        | t_exp, General t_act ->
                                acc && type_gen_of_type_hl t_exp = t_act
                        | t_exp, t_act ->
                                acc && t_exp = t_act
                    in
                    List.fold_left compare true @@ List.combine t_exp t_act
            in

            let takes = Hashtbl.create 10 in
            let t_out_actual = check_seq input types.t_in takes seq in

            if not (compare types.t_out t_out_actual) then
                raise @@ Error (loc, sprintf "expected %s, got %s" (string_of_types_hl types.t_out) (string_of_types_hl t_out_actual))
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
