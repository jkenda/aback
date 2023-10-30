open Format
open Lexer
open Parser
open Program

let loc_id = {
    filename = "[test]";
    included_from = [];
    expanded_from = [];
    row = 1; col = 1
}

let check procs macros program =
    let rec list_top list = function
        | 0 -> []
        | n -> List.hd list :: list_top (List.tl list) (n - 1)
    and remove_top list = function
        | 0 -> list
        | n -> remove_top (List.tl list) (n - 1)
    and stack_diff = function
        | ([], [])
        | ([], _ :: _)
        | (_ :: _, []) -> None
        | ((_, t1 as h1) :: _, (_, t2 as h2) :: _) when t1 <> t2 -> Some (h1, h2)
        | (_ :: t1, _ :: t2) -> stack_diff (t1, t2)
    and put t loc = function
        | (_, Int : location * typ) :: rest when t = PUTI -> rest
        | (_, Bool) :: rest when t = PUTB -> rest
        | (_, Char) :: rest when t = PUTC -> rest
        | (_, Float) :: rest when t = PUTF -> rest
        | (_, Ptr) :: (_, Int) :: rest when t = PUTS -> rest
        | (_, hd) :: _ -> raise @@ Error (loc,
            sprintf "invalid argument for %s: %s" (show_ir t) (show_typ hd))
        | [] -> raise @@ Error (loc,
            sprintf "%s: not enough data on stack" (show_ir t))
    in

    let storage = Hashtbl.create 10

    and stack_sizes = Hashtbl.create 10
    and stack_tops = Hashtbl.create 10
    and elseless = Hashtbl.create 10
    and diffs = Hashtbl.create 10 in
    let check' (((stack : (location * typ) list), stack_size) as s) (loc, instr) =
        match instr with
        | IF id ->
                Hashtbl.replace stack_sizes id stack_size;
                s
        | THEN id ->
                let diff =
                    let prev_size = Hashtbl.find stack_sizes id in
                    stack_size - prev_size
                in
                if diff = 1 then
                    let stack =
                        match stack with
                        | (_, Bool) :: tl -> tl
                        | (_, t) :: _ ->
                                raise @@ Error (loc,
                                sprintf "expected Bool, got %s" (show_typ t))
                        | [] -> raise @@ Error (loc, "empty stack")
                    in
                    Hashtbl.replace stack_sizes id (stack_size - 1);
                    stack, stack_size - 1
                else
                    raise @@ Error (loc, sprintf "expected stack to grow by 1, grew %d" diff)
        | ELSE id ->
                let diff = stack_size - (Hashtbl.find stack_sizes id) in
                if diff < 0 then raise @@ Error (loc, "cannot shrink stack inside if clause")
                else
                    Hashtbl.replace elseless id false;
                    Hashtbl.replace diffs id diff;
                    Hashtbl.replace stack_tops id (list_top stack diff);
                    remove_top stack diff, stack_size - diff
        | END_IF id when (try Hashtbl.find elseless id with Not_found -> true) ->
                if stack_size = Hashtbl.find stack_sizes id then s 
                else raise @@ Error (loc, "cannot grow or shrink stack inside elseless if")
        | END_IF id ->
                let diff = stack_size - (Hashtbl.find stack_sizes id) in
                if diff = Hashtbl.find diffs id then
                    match stack_diff (Hashtbl.find stack_tops id, stack) with
                    | None -> s
                    | Some ((l1, a), (l2, b)) -> raise @@ Error (loc,
                        sprintf "branches have different types on stack:\n\t%s (%s)\n\t%s (%s)"
                        (show_typ a) (print_location l1) (show_typ b) (print_location l2))
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
                        | (_, Bool) :: tl -> tl
                        | (_, t) :: _ ->
                                raise @@ Error (loc,
                                sprintf "expected Bool, got %s" (show_typ t))
                        | [] -> raise @@ Error (loc, "empty stack")
                    in
                    stack, stack_size - 1)
                else
                    raise @@ Error (loc, sprintf "expected stack to grow by 1, grew %d" diff)
        | END_WHILE id ->
                if stack_size = Hashtbl.find stack_sizes id then s 
                else raise @@ Error (loc, "cannot grow or shrink stack inside while loop")

        | PEEK (depth, addr) ->
                let _, data =
                    try List.nth stack depth
                    with _ -> raise @@ Error (loc,
                        sprintf "cannot peek with depth %d into stack with size %d" depth (List.length stack))
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

        | PUSH d -> (loc, type_of_data d) :: stack, stack_size + 1

        | ADD | SUB | MUL | DIV | MOD
        | BAND | BOR | BXOR | LSL | LSR ->
                (match stack with
                | (_, Int) :: (_, Int) :: tl -> (loc, Int) :: tl, stack_size - 1
                | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s %s" (show_typ a) (show_typ b))
                | (_, a) ::  _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s" (show_typ a))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))

        | AND | OR  ->
                (match stack with
                | (_, Bool) :: (_, Bool) :: tl -> (loc, Bool) :: tl, stack_size - 1
                | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                        sprintf "expected Bool Bool, got %s %s" (show_typ a) (show_typ b))
                | (_, a) ::  _ -> raise @@ Error (loc,
                        sprintf "expected Bool Bool, got %s" (show_typ a))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))

        | EQ | NE | LT | LE | GT | GE ->
                (match stack with
                | (_, Int) :: (_, Int) :: tl -> (loc, Bool) :: tl, stack_size - 1
                | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s %s" (show_typ a) (show_typ b))
                | (_, a) ::  _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s" (show_typ a))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))

        | PUTS ->
                (match stack with
                | (_, Ptr) :: (_, Int) :: tl -> tl, stack_size - 2
                | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                        sprintf "expected Ptr Int, got %s %s" (show_typ a) (show_typ b))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))
        | (PUTC | PUTI | PUTF | PUTB) as t -> put t loc stack, stack_size - 1

        | FADD | FSUB | FMUL | FDIV | FMOD ->
                (match stack with
                | (_, Float) :: (_, Float) :: tl -> (loc, Float) :: tl, stack_size - 1
                | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                        sprintf "expected Float Float, got %s %s" (show_typ a) (show_typ b))
                | (_, a) ::  _ -> raise @@ Error (loc,
                        sprintf "expected Float Float, got %s" (show_typ a))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))
    in
    (* typecheck typed procs and macros *)
    let check_func name { loc; types; seq } =
        match types with
        | None -> ()
        | Some (t_in, t_out) ->
            let stack, t_out =
                let remove_loc = List.map (fun (_, typ) -> typ) in
                let stack, _ =
                    List.fold_left check' (t_in, List.length t_in) seq
                in
                remove_loc stack, remove_loc t_out

            in
            match stack with
            | stack when stack = t_out -> ()
            | stack -> raise @@ Error (loc,
                sprintf "stacks at the end of %s don't match.\nexpected: %s\nactual: %s"
                name (print_typ_stack t_out) (print_typ_stack stack))
    in

    (* TODO: check that untyped procs don't overflow *)
    Hashtbl.iter check_func procs;
    Hashtbl.iter check_func macros;

    let stack, _ = List.fold_left check' ([], 0) program in
    match stack with
    | [] -> program
    | (loc, typ) :: _ -> raise @@ Error (loc,
        sprintf "%s left on the stack at the end of program" (show_typ typ))
