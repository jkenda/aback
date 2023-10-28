open Program

let postprocess program =
    let while_addr = Hashtbl.create 10 in
    let else_addr = Hashtbl.create 10 in
    let end_addr = Hashtbl.create 10 in
    let collect_jumps (acc, addr) = function
        | IF _ -> acc, addr
        | ELSE id as ir -> Hashtbl.add else_addr id addr; ir :: acc, addr + 1
        | END_IF id -> Hashtbl.add end_addr id addr; acc, addr
        
        | WHILE id -> Hashtbl.add while_addr id addr; acc, addr
        | DO _ as ir -> ir :: acc, addr + 1
        | END_WHILE id as ir -> Hashtbl.add end_addr id addr; ir :: acc, addr + 1

        | ir -> ir :: acc, addr + 1
    and postprocess' acc = function
        | THEN id ->
                let addr =
                    try Hashtbl.find else_addr id with Not_found ->
                    try Hashtbl.find end_addr id
                    with Not_found -> raise @@ Failure "expected 'else' or 'end'"
                in
                THEN (addr + 1) :: acc
        | ELSE id ->
                let addr =
                    try Hashtbl.find end_addr id
                    with Not_found -> raise @@ Failure "expected 'end'"
                in
                ELSE addr :: acc

        | DO id ->
                let addr =
                    try Hashtbl.find end_addr id
                    with Not_found -> raise @@ Failure "expected 'end'"
                in
                DO (addr + 1) :: acc
        | END_WHILE id ->
                let addr =
                    try Hashtbl.find while_addr id
                    with Not_found -> raise @@ Failure "expected 'while'"
                in
                END_WHILE addr :: acc

        | ir -> ir :: acc
    in
    let acc, _ = List.fold_left collect_jumps ([], 0) program in
    List.fold_left postprocess' [] acc

