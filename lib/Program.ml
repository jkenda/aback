open Format

type data =
    | Int of int
    | Ptr of int
    | Char of char
    | Float of float
[@@deriving show { with_path = false }]

type ir =
    | Push of data

    | Add | Fadd
    | Sub | Fsub
    | Mul | Fmul
    | Div | Fdiv
    | Mod | Fmod

    | Putc | Puts | Puti | Putf
[@@deriving show { with_path = false }]

type program = {
    ir : ir list;
    strings : string array
}

let exec program =
    let int_op op = function
        | Int i :: Int j :: rest -> Int (op i j) :: rest
        | a :: b :: _ -> raise @@ Failure (
            sprintf "expected Int Int, got %s %s" (show_data a) (show_data b))
        | _ -> raise @@ Failure "not enough data on stack"
    and put t = function
        | Int i   :: rest when t = Puti -> print_int i; rest
        | Char c  :: rest when t = Putc -> print_char c; rest
        | Float f :: rest when t = Putf -> print_float f; rest
        | Ptr p :: Int _ :: rest when t = Puts -> print_string (program.strings.(p)); rest
        | [] -> raise @@ Failure (sprintf "%s: not enough data on stack" (show_ir t))
        | stack -> raise @@ Failure (sprintf "Expected %s, got %s" (show_ir t) (show_data (List.hd stack)))
    in
    let exec' stack = function
        | Push d -> d :: stack

        | Add -> int_op ( + ) stack
        | Sub -> int_op ( - ) stack
        | Mul -> int_op ( * ) stack
        | Div -> int_op ( / ) stack
        | Mod -> int_op (mod) stack

        | Putc -> put Putc stack
        | Puts -> put Puts stack
        | Puti -> put Puti stack
        | Putf -> put Putf stack

        | ir -> raise @@ Failure (show_ir ir ^ " not implemented")
    in
    List.fold_left exec' [] program.ir
