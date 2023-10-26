open Program

let postprocess program =
    let else_addr = Hashtbl.create 10 in
    let end_if_addr = Hashtbl.create 10 in
    let collect_jumps (acc, addr) = function
        | IF _ -> acc, addr
        | ELSE id as ir -> Hashtbl.add else_addr id addr; ir :: acc, addr + 1
        | END_IF id -> Hashtbl.add end_if_addr id addr; acc, addr
        | ir -> ir :: acc, addr + 1
    and postprocess' acc = function
        | THEN id ->
                let addr =
                    try Hashtbl.find else_addr id with Not_found ->
                    Hashtbl.find end_if_addr id
                in
                THEN (addr + 1) :: acc
        | ELSE id -> ELSE (Hashtbl.find end_if_addr id) :: acc
        | ir -> ir :: acc
    in
    let acc, _ = List.fold_left collect_jumps ([], 0) program in
    List.fold_left postprocess' [] acc

