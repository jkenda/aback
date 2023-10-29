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
    and compare_tops = function
        | ([], [])
        | ([], _ :: _)
        | (_ :: _, []) -> true
        | (h1 :: _, h2 :: _) when h1 != h2 -> false
        | (_ :: t1, _ :: t2) -> compare_tops (t1, t2)
    and put t loc = function
        | (Int : typ) :: rest when t = PUTI -> rest
        | Bool :: rest when t = PUTB -> rest
        | Char :: rest when t = PUTC -> rest
        | Float :: rest when t = PUTF -> rest
        | Ptr :: Int :: rest when t = PUTS -> rest
        | hd :: _ -> raise @@ Error (loc,
            sprintf "invalid argument for %s: %s" (show_ir t) (show_typ hd))
        | [] -> raise @@ Error (loc,
            sprintf "%s: not enough data on stack" (show_ir t))
    in

    let storage = Hashtbl.create 10

    and stack_sizes = Hashtbl.create 10
    and stack_tops = Hashtbl.create 10
    and elseless = Hashtbl.create 10
    and diffs = Hashtbl.create 10 in
    let check' (((stack : typ list), stack_size) as s) (loc, instr) =
        match instr with
        | IF id ->
                Hashtbl.add stack_sizes id stack_size;
                Hashtbl.add elseless id true;
                s
        | THEN id ->
                let prev_size = Hashtbl.find stack_sizes id in
                if stack_size = prev_size + 1 then
                    (Hashtbl.add stack_sizes id (stack_size - 1);
                    Bool :: List.tl stack, stack_size - 1)
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
                    if compare_tops (stack, Hashtbl.find stack_tops id) then s
                    else raise @@ Error (loc, "branches have different types on stack")
                else raise @@ Error (loc, "branches have different stack sizes")

        | WHILE id ->
                Hashtbl.add stack_sizes id stack_size; s
        | DO id -> 
                let prev_size = Hashtbl.find stack_sizes id in
                if stack_size = prev_size + 1 then
                    (Hashtbl.add stack_sizes id (stack_size - 1);
                    let stack, stack_size =
                        match stack with
                        | Bool :: tl -> tl, stack_size - 1
                        | [] -> raise @@ Error (loc, "empty stack")
                        | t :: _ ->
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

        | PUSH d -> type_of_data d :: stack, stack_size + 1

        | ADD | SUB | MUL | DIV | MOD ->
                (match stack with
                | Int :: Int :: tl -> Int :: tl, stack_size - 1
                | a :: b :: _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s %s" (show_typ a) (show_typ b))
                | a ::  _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s " (show_typ a))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))

        | EQ | NE | LT | LE | GT | GE ->
                (match stack with
                | Int :: Int :: tl -> Bool :: tl, stack_size - 1
                | a :: b :: _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s %s" (show_typ a) (show_typ b))
                | a ::  _ -> raise @@ Error (loc,
                        sprintf "expected Int Int, got %s " (show_typ a))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))

        | PUTS ->
                (match stack with
                | Ptr :: Int :: tl -> tl, stack_size - 2
                | a :: b :: _ -> raise @@ Error (loc,
                        sprintf "expected Ptr Int, got %s %s" (show_typ a) (show_typ b))
                | _ -> raise @@ Error (loc, "not enough elements on the stack"))
        | (PUTC | PUTI | PUTF | PUTB) as t -> put t loc stack, stack_size - 1

        | ir -> raise @@ Not_implemented (loc, show_ir ir)

        (*

        | FADD -> float_op ( +. ) stack, ip + 1
        | FSUB -> float_op ( -. ) stack, ip + 1
        | FMUL -> float_op ( *. ) stack, ip + 1
        | FDIV -> float_op ( /. ) stack, ip + 1
        | FMOD -> float_op (fun a b ->
                let _, c = modf a in
                let _, d = modf b in
                c /. d) stack, ip + 1

        | AND -> bool_op ( && ) stack, ip + 1
        | OR  -> bool_op ( || ) stack, ip + 1

        | BAND -> int_op ( land ) stack, ip + 1
        | BOR  -> int_op ( lor  ) stack, ip + 1
        | BXOR -> int_op ( lxor ) stack, ip + 1
        | LSL  -> int_op ( lsl  ) stack, ip + 1
        | LSR  -> int_op ( lsr  ) stack, ip + 1

        *)
    in
    List.fold_left check' ([], 0) program |> ignore;
    program
