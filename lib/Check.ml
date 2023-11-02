open Format
open Lexer
open Preprocess
open Parser
open Program

let loc_id = {
    filename = "[test]";
    included_from = [];
    expanded_from = [];
    row = 1; col = 1
}

let check procs macros program =
    (* get top n elements of the stack *)
    let rec get_top list = function
        | 0 -> []
        | n -> List.hd list :: get_top (List.tl list) (n - 1)
    (* remove top n elements from the stack *)
    and remove_top list = function
        | 0 -> list
        | n -> remove_top (List.tl list) (n - 1)
    (* find the first element on the stack that differs *)
    and stack_diff = function
        | ([], [])
        | ([], _ :: _)
        | (_ :: _, []) -> None
        | ((_, t1 as h1) :: _, (_, t2 as h2) :: _) when t1 <> t2 -> Some (h1, h2)
        | (_ :: t1, _ :: t2) -> stack_diff (t1, t2)
    (* return the state after console output *)
    and put t loc = function
        | (_, Type Int : location * prep) :: rest when t = PUTI -> rest
        | (_, Type Bool) :: rest when t = PUTB -> rest
        | (_, Type Char) :: rest when t = PUTC -> rest
        | (_, Type Float) :: rest when t = PUTF -> rest
        | (_, Type Ptr) :: (_, Type Int) :: rest when t = PUTS -> rest
        | (_, hd) :: _ -> raise @@ Error (loc,
            sprintf "invalid argument for %s: %s" (show_ir t) (print_prep hd))
        | [] -> raise @@ Error (loc,
            sprintf "%s: not enough data on stack" (show_ir t))
    in

    let storage = Hashtbl.create 10
    and stack_sizes = Hashtbl.create 10
    and elseless = Hashtbl.create 10 in

    let check_numbered =
        let check' stack_size (loc, ir) =
        if stack_size < 0 then
            raise @@ Error (loc, "stack undeflow");

        match ir with
        | FN name ->
                (let func =
                    try Hashtbl.find procs name with Not_found ->
                    Hashtbl.find macros name
                in
                match func.types with
                | Untyped -> stack_size
                | Numbered (n_in, _) ->
                        if stack_size < n_in then
                            raise @@ Error (loc,
                            sprintf "not enough arguments for function '%s'" name)
                        else stack_size
                | Typed (_, _) ->
                        raise @@ Error (loc, "cannot use typed function inside numbered one"))
        | FN_END -> stack_size

        | IF id ->
                Hashtbl.replace stack_sizes id stack_size;
                stack_size
        | THEN id ->
                let diff =
                    let prev_size = Hashtbl.find stack_sizes id in
                    stack_size - prev_size
                in
                if diff = 1 then (
                    Hashtbl.replace stack_sizes id (stack_size - 1);
                    stack_size - 1)
                else
                    raise @@ Error (loc, sprintf "expected stack to grow by 1, grew %d" diff)
        | ELSE id ->
                let diff = stack_size - (Hashtbl.find stack_sizes id) in
                if diff < 0 then raise @@ Error (loc, "cannot shrink stack inside if clause")
                else
                    Hashtbl.add elseless id false;
                    Hashtbl.add stack_sizes id diff;
                    stack_size - diff
        | END_IF id when (try Hashtbl.find elseless id with Not_found -> true) ->
                if stack_size = Hashtbl.find stack_sizes id then stack_size
                else raise @@ Error (loc, "cannot grow or shrink stack inside elseless if")
        | END_IF id ->
                let diff = Hashtbl.find stack_sizes id in
                Hashtbl.remove stack_sizes id;
                if diff = stack_size - (Hashtbl.find stack_sizes id) then stack_size
                else raise @@ Error (loc, "branches have different stack sizes")

        | WHILE id ->
                Hashtbl.replace stack_sizes id stack_size; stack_size
        | DO id -> 
                let diff =
                    let prev_size = Hashtbl.find stack_sizes id in
                    stack_size - prev_size
                in
                if diff = 1 then (
                    Hashtbl.replace stack_sizes id (stack_size - 1);
                    stack_size - 1)
                else raise @@ Error (loc, sprintf "expected stack to grow by 1, grew %d" diff)
        | END_WHILE id ->
                let diff = stack_size - Hashtbl.find stack_sizes id in
                if diff = 0 then stack_size
                else if diff > 0 then
                    raise @@ Error (loc, "cannot grow stack inside loop")
                else raise @@ Error (loc, "cannot shrink stack inside loop")

        | PEEK (depth, addr) ->
                Hashtbl.replace storage addr ();
                if depth < stack_size then stack_size
                else raise @@ Error (loc,
                    sprintf "cannot peek %d deep into stack of length %d"
                    (depth + 1) stack_size)
        | TAKE addr ->
                Hashtbl.replace storage addr ();
                stack_size - 1
        | PUT addr -> (
                match Hashtbl.find_opt storage addr with
                | Some () -> stack_size + 1
                | None -> raise @@ Error (loc,
                        sprintf "nothing storeed at this address"))

        | PUSH _ -> stack_size + 1

        | ADD | SUB | MUL | DIV | MOD
        | FADD | FSUB | FMUL | FDIV | FMOD
        | BAND | BOR | BXOR | LSL | LSR
        | AND | OR
        | EQ | NE | LT | LE | GT | GE -> stack_size - 1

        | PUTS -> stack_size - 1
        | PUTC | PUTI | PUTF | PUTB -> stack_size - 1
        in
        List.fold_left check'
    in

    (* simulate the program running on the stack to typecheck it *)
    let check_typed t_in =
        let storage = Hashtbl.create 10
        and stack_sizes = Hashtbl.create 10
        and stack_tops = Hashtbl.create 10
        and elseless = Hashtbl.create 10 in

        let gen_to_concrete = Hashtbl.create 10
        and type_stack = Stack.create () in

        let check' (((stack : (location * prep) list), stack_size) as s) (loc, instr) =
            match instr with
            | FN name ->
                    (let func =
                        try Hashtbl.find procs name with Not_found ->
                        Hashtbl.find macros name
                    in
                    match func.types with
                    | Untyped ->
                            Stack.push [] type_stack;
                            s
                    | Numbered (n_expected, _) ->
                            Stack.push [] type_stack;
                            if stack_size < n_expected then
                                raise @@ Error (loc,
                                sprintf "not enough arguments for function '%s'" name)
                            else s
                    | Typed (t_expected, _) -> (
                            let t_expected = t_expected |> List.split |> snd in
                            Stack.push t_expected type_stack;
                            let check_types' i (t_expected, t) =
                                match t_expected, t with
                                | Type expected, Type actual ->
                                        if expected = actual then ()
                                        else raise @@ Error (loc,
                                            sprintf "Invalid argument n. %d for function '%s'. " i name ^
                                            sprintf "Expected: %s, " (print_typ expected) ^
                                            sprintf "actual: %s." (print_typ actual))
                                | expected, actual ->
                                        (match Hashtbl.find_opt gen_to_concrete expected with
                                        | Some expected when expected = actual -> ()
                                        | Some expected -> raise @@ Error (loc,
                                            sprintf "Invalid argument n. %d for function '%s'. " i name ^
                                            sprintf "Expected: %s, " (print_prep expected) ^
                                            sprintf "actual: %s." (print_prep actual))
                                        | None -> Hashtbl.add gen_to_concrete expected actual)
                            in
                            let t_in =
                                try
                                    get_top stack (List.length t_expected)
                                    |> List.split |> snd
                                with _ -> raise @@ Error (loc,
                                    sprintf "Not enough arguments for function '%s'." name)
                            in
                            List.iteri check_types' @@ List.combine t_expected t_in; s))
            | FN_END ->
                    Stack.pop type_stack
                    |> List.iter (Hashtbl.remove gen_to_concrete);
                    s

            | IF id -> Hashtbl.replace stack_sizes id stack_size; s
            | THEN id ->
                    let diff =
                        let prev_size = Hashtbl.find stack_sizes id in
                        stack_size - prev_size
                    in
                    if diff = 1 then
                        let stack =
                            match stack with
                            | (_, Type Bool) :: tl -> tl
                            | (_, t) :: _ ->
                                    raise @@ Error (loc,
                                    sprintf "Expected Bool, got %s." (print_prep t))
                            | [] -> raise @@ Error (loc, "empty stack")
                        in
                        Hashtbl.replace stack_sizes id (stack_size - 1);
                        stack, stack_size - 1
                    else
                        raise @@ Error (loc, sprintf "Expected stack to grow by 1, grew %d." diff)
            | ELSE id ->
                    let diff = stack_size - (Hashtbl.find stack_sizes id) in
                    if diff < 0 then raise @@ Error (loc, "Cannot shrink stack inside if clause.")
                    else
                        Hashtbl.add elseless id false;
                        Hashtbl.add stack_sizes id diff;
                        Hashtbl.add stack_tops id (get_top stack diff);
                        remove_top stack diff, stack_size - diff
            | END_IF id when (try Hashtbl.find elseless id with Not_found -> true) ->
                    if stack_size = Hashtbl.find stack_sizes id then s 
                    else raise @@ Error (loc, "Cannot grow or shrink stack inside elseless if.")
            | END_IF id ->
                    let diff = Hashtbl.find stack_sizes id in
                    Hashtbl.remove stack_sizes id;
                    if diff = stack_size - (Hashtbl.find stack_sizes id) then
                        match stack_diff (Hashtbl.find stack_tops id, stack) with
                        | None -> s
                        | Some ((l1, a), (l2, b)) -> raise @@ Error (loc,
                            sprintf "Branches have different types on stack:\n\t%s (%s)\n\t%s (%s)."
                            (print_prep a) (print_location l1) (print_prep b) (print_location l2))
                    else raise @@ Error (loc, "branches have different stack sizes")

            | WHILE id ->
                    Hashtbl.replace stack_sizes id stack_size; s
            | DO id -> 
                    let diff =
                        let prev_size = Hashtbl.find stack_sizes id in
                        stack_size - prev_size
                    in
                    if diff = 1 then (
                        Hashtbl.replace stack_sizes id (stack_size - 1);
                        let stack =
                            match stack with
                            | (_, Type Bool) :: tl -> tl
                            | (_, t) :: _ ->
                                    raise @@ Error (loc,
                                    sprintf "expected Bool, got %s" (print_prep t))
                            | [] -> raise @@ Error (loc, "empty stack")
                        in
                        stack, stack_size - 1)
                    else
                        raise @@ Error (loc, sprintf "expected stack to grow by 1, grew %d" diff)
            | END_WHILE id ->
                    let diff = stack_size - Hashtbl.find stack_sizes id in
                    if diff = 0 then s 
                    else if diff > 0 then
                        let loc, top = List.split @@ get_top stack diff in
                        raise @@ Error (List.hd loc,
                            sprintf "cannot grow stack inside loop: %s" (print_prep_stack top))
                    else
                        raise @@ Error (loc, "cannot shrink stack inside loop")

            | PEEK (depth, addr) ->
                    let _, data =
                        try List.nth stack depth
                        with _ -> raise @@ Error (loc,
                            sprintf "cannot peek with depth %d into stack with size %d" depth
                            (List.length stack))
                    in
                    Hashtbl.replace storage addr data; stack, stack_size
            | TAKE addr ->
                    let data, stack =
                        match stack with
                        | (_, hd) :: tl -> hd, tl
                        | _ -> raise @@ Error (loc, "cannot take from empty stack")
                    in
                    Hashtbl.replace storage addr data; stack, stack_size - 1
            | PUT addr -> (loc, Hashtbl.find storage addr) :: stack, stack_size + 1

            | PUSH d -> (loc, prep_of_data d) :: stack, stack_size + 1

            | ADD | SUB | MUL | DIV | MOD
            | BAND | BOR | BXOR | LSL | LSR ->
                    (match stack with
                    | (_, Type Int) :: (_, Type Int) :: tl -> (loc, Type Int) :: tl, stack_size - 1
                    | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                            sprintf "expected Int Int, got %s %s; operator is not generic"
                            (print_prep a) (print_prep b))
                    | (_, a) ::  _ -> raise @@ Error (loc,
                            sprintf "expected Int Int, got %s" (print_prep a))
                    | _ -> raise @@ Error (loc, "not enough elements on the stack"))

            | AND | OR ->
                    (match stack with
                    | (_, Type Bool) :: (_, Type Bool) :: tl -> (loc, Type Bool) :: tl, stack_size - 1
                    | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                            sprintf "expected Bool Bool, got %s %s; operator is not generic"
                            (print_prep a) (print_prep b))
                    | (_, a) ::  _ -> raise @@ Error (loc,
                            sprintf "expected Bool Bool, got %s" (print_prep a))
                    | _ -> raise @@ Error (loc, "not enough elements on the stack"))
            | EQ | NE | LT | LE | GT | GE ->
                    (match stack with
                    | (_, a) :: (_, b) :: tl when a = b -> (loc, Type Bool) :: tl, stack_size - 1
                    | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                            sprintf "expected types to equal: %s %s" (print_prep a) (print_prep b))
                    | (_, a) ::  _ -> raise @@ Error (loc,
                            sprintf "expected Bool Bool, got %s" (print_prep a))
                    | _ -> raise @@ Error (loc, "not enough elements on the stack"))

            | PUTS ->
                    (match stack with
                    | (_, Type Ptr) :: (_, Type Int) :: tl -> tl, stack_size - 2
                    | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                            sprintf "expected Ptr Int, got %s %s" (print_prep a) (print_prep b))
                    | _ -> raise @@ Error (loc, "not enough elements on the stack"))
            | (PUTC | PUTI | PUTF | PUTB) as t -> put t loc stack, stack_size - 1

            | FADD | FSUB | FMUL | FDIV | FMOD ->
                    (match stack with
                    | (_, Type Float) :: (_, Type Float) :: tl -> (loc, Type Float) :: tl, stack_size - 1
                    | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                            sprintf "expected Float Float, got %s %s" (print_prep a) (print_prep b))
                    | (_, a) ::  _ -> raise @@ Error (loc,
                            sprintf "expected Float Float, got %s" (print_prep a))
                    | _ -> raise @@ Error (loc, "not enough elements on the stack"))
        in
        List.fold_left check' (t_in, List.length t_in)
    in
    (* typecheck typed procs and macros *)
    let check_func_inside name { loc; types; seq } =
        match types with
        | Untyped -> ()
        | Numbered (n_in, n_out_expected) -> (
            let n_out =
                let strings = ref ""
                and max_addr = ref 0 in
                let parse =
                    parse strings procs macros max_addr
                in
                parse seq
                |> check_numbered n_in
            in
            if n_out = n_out_expected then ()
            else raise @@ Error (loc,
                sprintf "'%s': unexpected number of return elements: expected: %d actual: %d"
                name n_out_expected n_out))
        | Typed (t_in, t_out) ->
            let stack, t_out =
                let strings = ref ""
                and max_addr = ref 0 in
                let parse =
                    parse strings procs macros max_addr
                and remove_loc = List.map (fun (_, typ) -> typ) in
                let stack, _ =
                    parse seq
                    |> check_typed t_in
                in
                remove_loc stack, remove_loc t_out
            in
            match stack with
            | stack when stack = t_out -> ()
            | stack -> raise @@ Error (loc,
                sprintf "'%s': unexpected return value. expected : %s actual   : %s"
                name (print_prep_stack t_out) (print_prep_stack stack))
    in

    (* check procs and macros *)
    Hashtbl.iter check_func_inside procs;
    Hashtbl.iter check_func_inside macros;

    (* check the main program *)
    match fst @@ check_typed [] program with
    | [] -> program
    | (loc, _) :: _ as stack ->
            let stack =
                List.map (fun (_, typ) -> typ) stack
            in
            raise @@ Error (loc,
                sprintf "%s left on the stack at the end of program"
                (print_prep_stack stack))
