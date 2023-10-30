open Format
open Lexer
open Program

let check program =
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
        | (h1 :: _, h2 :: _) when h1 <> h2 -> Some (h1, h2)
        | (_ :: t1, _ :: t2) -> stack_diff (t1, t2)
    and put t loc = function
        | ((_, Int) : location * typ) :: rest when t = PUTI -> rest
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
                Hashtbl.add stack_sizes id stack_size;
                Hashtbl.add elseless id true;
                s
        | THEN id ->
                let prev_size = Hashtbl.find stack_sizes id in
                if stack_size = prev_size + 1 then
                    (Hashtbl.add stack_sizes id (stack_size - 1);
                    (loc, Bool) :: List.tl stack, stack_size - 1)
                else
                    let diff = stack_size - prev_size in
                    raise @@ Error (loc, sprintf "expected stack to grow by 1, grew %d" diff)
        | ELSE id ->
                let diff = stack_size - (Hashtbl.find stack_sizes id) in
                if diff < 0 then raise @@ Error (loc, "cannot shrink stack inside if clause")
                else
                    Hashtbl.add elseless id false;
                    Hashtbl.add diffs id diff;
                    Hashtbl.add stack_tops id (list_top stack diff);
                    remove_top stack diff, stack_size - diff
        | END_IF id when Hashtbl.find elseless id ->
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
                Hashtbl.add stack_sizes id stack_size; s
        | DO id -> 
                let prev_size = Hashtbl.find stack_sizes id in
                if stack_size = prev_size + 1 then
                    (Hashtbl.add stack_sizes id (stack_size - 1);
                    let stack, stack_size =
                        match stack with
                        | (_, Bool) :: tl -> tl, stack_size - 1
                        | [] -> raise @@ Error (loc, "empty stack")
                        | (_, t) :: _ ->
                                raise @@ Error (loc,
                                sprintf "expected Bool, got %s" (show_typ t))
                in
                    stack, stack_size)
                else
                    let diff = stack_size - prev_size in
                    raise @@ Error (loc, sprintf "expected stack to grow by 1, grew %d" diff)
        | END_WHILE id ->
                if stack_size = Hashtbl.find stack_sizes id then s 
                else raise @@ Error (loc, "cannot grow or shrink stack inside while loop")

        | PEEK (depth, addr) ->
                let data =
                    try List.nth stack depth
                    with _ -> raise @@ Error (loc, "cannot peek into empty stack")
                in
                Hashtbl.add storage addr data; stack, stack_size
        | TAKE addr ->
                let data, stack, stack_size =
                    match stack with
                    | hd :: tl -> hd, tl, stack_size - 1
                    | _ -> raise @@ Error (loc, "cannot take from empty stack")
                in
                Hashtbl.add storage addr data; stack, stack_size
        | PUT addr -> Hashtbl.find storage addr :: stack, stack_size + 1

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

        | FADD
        | FSUB
        | FMUL
        | FDIV
        | FMOD ->
                (match stack with
                | (_, Float) :: (_, Float) :: tl -> (loc, Float) :: tl, stack_size - 1
                | (_, a) :: (_, b) :: _ -> raise @@ Error (loc,
                        sprintf "expected Float Float, got %s %s" (show_typ a) (show_typ b))
                | (_, a) ::  _ -> raise @@ Error (loc,
                        sprintf "expected Float Float, got %s" (show_typ a))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))
    in
    let stack, _ = List.fold_left check' ([], 0) program in
    match stack with
    | [] -> program
    | (loc, typ) :: _ -> raise @@ Error (loc,
        sprintf "%s left on the stack at the end of program" (show_typ typ))
