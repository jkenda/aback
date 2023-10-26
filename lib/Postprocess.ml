open Format
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
                    Hashtbl.find end_addr id
                in
                THEN (addr + 1) :: acc
        | ELSE id -> ELSE (try Hashtbl.find end_addr id with Not_found ->
                raise @@ Failure (sprintf "(END %d) not found" id)) :: acc

        | DO id -> DO (Hashtbl.find end_addr id + 1) :: acc
        | END_WHILE id -> END_WHILE (Hashtbl.find while_addr id) :: acc

        | ir -> ir :: acc
    in
    let acc, _ = List.fold_left collect_jumps ([], 0) program in
    List.fold_left postprocess' [] acc

