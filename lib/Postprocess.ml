open Lexer
open Program

let postprocess program =
    let strings = ref "" in

    (* add a string to the string table *)
    let _add_string str =
        (* try to find and reuse existing string *)
        try
            let re = Str.regexp_string str in
            Str.search_forward re !strings 0, String.length str
        (* if not found, add new string *)
        with Not_found ->
            let addr = String.length !strings in
            strings := !strings ^ str ^ "\x00";
            addr, String.length str
    in

    let while_addr = Hashtbl.create 10
    and else_addr = Hashtbl.create 10
    and end_addr = Hashtbl.create 10 in

    let collect_jumps instrs =
        let collect' (acc, addr) (_, inst as ir) =
            match inst with
            | FN _ | FN_END
            | IF _ -> acc, addr
            | ELSE id -> Hashtbl.replace else_addr id (addr + 1); ir :: acc, addr + 1
            | END_IF id -> Hashtbl.replace end_addr id addr; acc, addr
            
            | WHILE id -> Hashtbl.replace while_addr id addr; acc, addr
            | DO _ -> ir :: acc, addr + 1
            | END_WHILE id -> Hashtbl.replace end_addr id (addr + 1); ir :: acc, addr + 1

            | _ -> ir :: acc, addr + 1
        in
        fst @@ List.fold_left collect' ([], 0) instrs
    and ids_to_addrs instrs =
        let id_to_addr acc (loc, inst as ir) =
            match inst with
            | THEN id ->
                    let addr =
                        try Hashtbl.find else_addr id with Not_found ->
                        try Hashtbl.find end_addr id
                        with Not_found -> raise @@ Error (loc, "expected 'else' or 'end'")
                    in
                    (loc, THEN addr) :: acc
            | ELSE id ->
                    let addr =
                        try Hashtbl.find end_addr id
                        with Not_found -> raise @@ Error (loc, "expected 'end'")
                    in
                    (loc, ELSE addr) :: acc

            | DO id ->
                    let addr =
                        try Hashtbl.find end_addr id
                        with Not_found -> raise @@ Error (loc, "expected 'end'")
                    in
                    (loc, DO addr) :: acc
            | END_WHILE id ->
                    let addr =
                        try Hashtbl.find while_addr id
                        with Not_found -> raise @@ Error (loc, "expected 'while'")
                    in
                    (loc, END_WHILE addr) :: acc

            | _ -> ir :: acc
        in
        List.fold_left id_to_addr [] instrs
    in

    program
    |> collect_jumps
    |> ids_to_addrs
    |> Array.of_list
    |> Array.split
