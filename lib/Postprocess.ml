open Lexer
open Program

let postprocess program =
    let while_addr = Hashtbl.create 10 in
    let else_addr = Hashtbl.create 10 in
    let end_addr = Hashtbl.create 10 in
    let collect_jumps (acc, addr) (_, inst as ir) =
        match inst with
        | IF _ -> acc, addr
        | ELSE id -> Hashtbl.replace else_addr id addr; ir :: acc, addr + 1
        | END_IF id -> Hashtbl.replace end_addr id addr; acc, addr
        
        | WHILE id -> Hashtbl.replace while_addr id addr; acc, addr
        | DO _ -> ir :: acc, addr + 1
        | END_WHILE id -> Hashtbl.replace end_addr id addr; ir :: acc, addr + 1

        | _ -> ir :: acc, addr + 1
    and postprocess' acc (loc, inst as ir) =
        match inst with
        | THEN id ->
                let addr =
                    try Hashtbl.find else_addr id with Not_found ->
                    try Hashtbl.find end_addr id
                    with Not_found -> raise @@ Error (loc, "expected 'else' or 'end'")
                in
                (loc, THEN (addr + 1)) :: acc
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
                (loc, DO (addr + 1)) :: acc
        | END_WHILE id ->
                let addr =
                    try Hashtbl.find while_addr id
                    with Not_found -> raise @@ Error (loc, "expected 'while'")
                in
                (loc, END_WHILE addr) :: acc

        | _ -> ir :: acc
    in
    let acc, _ = List.fold_left collect_jumps ([], 0) program in
    List.fold_left postprocess' [] acc
    |> Array.of_list
    |> Array.split

