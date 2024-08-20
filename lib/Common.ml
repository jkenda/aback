open Format

(*
    location of tokens, nodes
    used for error messages, debugging data
 *)
type location = {
    filename : string;
    included_from : string list;
    expanded_from : (location * string) list;
    row : int;
    col : int;
}
[@@deriving show { with_path = false }]
let string_of_location loc = sprintf "'%s':%d:%d" loc.filename loc.row loc.col


(*
    custom exceptions
 *)
exception Error of location * string
exception Not_implemented of location * string
exception Unreachable of string

let print_error loc msg =
    List.iter (fun (loc, name) -> printf "expanded from %s (%s)\n" (string_of_location loc) name) loc.expanded_from;
    printf "%s:\n" (string_of_location loc);
    printf "\t%s\n" msg;
    if List.length loc.included_from > 0 then printf "\n";
    List.iter (fun filename -> printf "included from '%s'\n" filename) loc.included_from;

type strings = string list
[@@deriving show { with_path = false }]


(*
   terminal colors
 *)
type color =
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan

let print_in_color color text =
    printf "\o033[%dm%s\o033[39m"
    (match color with
    | Red     -> 31
    | Green   -> 32
    | Yellow  -> 33
    | Blue    -> 34
    | Magenta -> 35
    | Cyan    -> 36)
    text


(*
    operators
    intrinsic functions that have one or two arguments
 *)
type operator =
    | Eq | NEq | Lt | LEq | Gt | GEq
    | Add | Sub | Mul | Div | Mod

    | Itof | Ftoi

    | LAnd | LOr | LXor | Lsl | Lsr
    | And  | Or
    | Ref | Deref
[@@deriving show { with_path = false }]


type type_gen =
    | Numeric
    | Integer
    | Floating
    | Boolean
    | Character
    | String
    | CString
    | Pointer of type_gen
[@@deriving show { with_path = false }]

let rec string_of_type_gen = function
    | Numeric -> "numeric"
    | Integer -> "integer"
    | Floating -> "decimal"
    | Boolean -> "boolean"
    | Character -> "character"
    | String -> "string"
    | CString -> "cstring"
    | Pointer t -> string_of_type_gen t ^ " pointer"

let string_of_types_gen =
    List.fold_left (fun acc typ -> acc ^ string_of_type_gen typ ^ " ") ""


(*
    types produced by the lexer
 *)
type type_tok =
    | I8 | I16 | I32 | I64
    | U8 | U16 | U32 | U64
    | F32 | F64
    | Bool
    | Ptr
    | String | CStr
    | Generic of string
[@@deriving show { with_path = false }]

let string_of_type_tok = function
    | I8 -> "i8" | I16 -> "i16" | I32 -> "i32" | I64 -> "i64"
    | U8 -> "u8" | U16 -> "u16" | U32 -> "u32" | U64 -> "u64"
    | F32 -> "f32" | F64 -> "f64"
    | Bool -> "bool"
    | Ptr -> "ptr"
    | String -> "string"
    | CStr -> "cstr"
    | Generic s -> s

let string_of_types_tok =
    List.fold_left (fun acc typ -> acc ^ string_of_type_tok typ ^ " ") ""


(*
   low-level types
 *)
type type_ll =
    | I8 | I16 | I32 | I64
    | U8 | U16 | U32 | U64
    | F32 | F64
    | Bool
    | Ptr of type_ll
[@@deriving show { with_path = false }]

let rec string_of_type_ll = function
    | I8 -> "i8" | I16 -> "i16" | I32 -> "i32" | I64 -> "i64"
    | U8 -> "u8" | U16 -> "u16" | U32 -> "u32" | U64 -> "u64"
    | F32 -> "f32" | F64 -> "f64"
    | Bool -> "bool"
    | Ptr t -> sprintf "%s ptr" (string_of_type_ll t)

let string_of_types_ll =
    List.fold_left (fun acc typ -> acc ^ string_of_type_ll typ ^ " ") ""

let type_ll_of_string = function
    | "i8" -> I8 | "i16" -> I16 | "i32" -> I32 | "i64" -> I64
    | "u8" -> U8 | "u16" -> U16 | "u32" -> U32 | "u64" -> U64
    | "f32" -> F32 | "f64" -> F64
    | "bool" -> Bool

    | _ -> raise @@ Unreachable "not primitive"

let (type_ll_of_type_tok : type_tok -> type_ll) = function
    | I8 -> I8 | I16 -> I16 | I32 -> I32 | I64 -> I64
    | U8 -> U8 | U16 -> U16 | U32 -> U32 | U64 -> U64
    | F32 -> F32 | F64 -> F64
    | Bool -> Bool
    | t -> failwith @@ sprintf "%s not directly convertible" (show_type_tok t)

let rec type_gen_of_type_ll = function
    | I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 -> Integer
    | F32 | F64 -> Floating
    | Bool -> Boolean
    | Ptr t -> Pointer (type_gen_of_type_ll t)


(*
   high-level types
 *)
type type_hl =
    | Primitive of type_ll
    | Struc of (string * type_hl) list
    | Union of (string * type_hl) list
    | Str | CStr
    | Ptr of type_hl
    | General of type_gen
    | Generic of string
[@@deriving show { with_path = false }]

type types_hl = type_hl list
[@@deriving show { with_path = false }]

let rec string_of_type_hl = function
    | Primitive t -> string_of_type_ll t
    | Struc tl -> Format.sprintf "struc { %s }" @@ (List.map (fun t -> snd t |> string_of_type_hl) tl |> List.fold_left (^) "")
    | Union tl -> Format.sprintf "union { %s }" @@ (List.map (fun t -> snd t |> string_of_type_hl) tl |> List.fold_left (^) "")
    | Str -> "str" | CStr -> "cstr"
    | Ptr t -> string_of_type_hl t
    | General t -> string_of_type_gen t
    | Generic s -> s

let string_of_types_hl =
    List.fold_left (fun acc typ -> acc ^ string_of_type_hl typ ^ " ") ""

let type_hl_of_type_ll type_ll =
    Primitive type_ll

let rec type_ll_of_type_hl = function
    | Primitive t -> t
    | Ptr t -> Ptr (type_ll_of_type_hl t)
    | General Boolean -> Bool
    | General Character -> U8
    | t -> failwith @@ sprintf "%s not directly convertible to type_ll" (show_type_hl t)

let rec type_gen_of_type_hl = function
    | Primitive ll -> type_gen_of_type_ll ll
    | Str -> String | CStr -> CString
    | Ptr t -> Pointer (type_gen_of_type_hl t)
    | General t -> t
    | t -> failwith @@ sprintf "%s not directly convertible" (show_type_hl t)

let (type_hl_of_type_tok : type_tok -> type_hl) = function
    | (I8 | I16 | I32 | I64
    | U8 | U16 | U32 | U64
    | F32 | F64
    | Bool as t) -> Primitive (type_ll_of_type_tok t)
    | String -> Str | CStr -> CStr
    | t -> failwith @@ sprintf "%s not directly convertible to type_hl" (show_type_tok t)


(*
   data of token
 *)
type data_tok =
    | Integer of int
    | Decimal of float
    | Char of char
    | Bool of bool
    | String of string | CString of string
[@@deriving show { with_path = false }]

let string_of_data_tok = function
    | Integer i -> string_of_int i
    | Decimal f -> string_of_float f
    | Char c -> String.make 1 c
    | Bool b -> string_of_bool b
    | String s -> sprintf "\"%s\"" (String.escaped s) | CString s -> sprintf "c\"%s\"" (String.escaped s)

let (type_of_data_tok : data_tok -> type_gen) = function
    | Integer _ -> Integer
    | Decimal _ -> Floating
    | Char    _ -> Character
    | Bool    _ -> Boolean
    | String  _ -> String
    | CString    _ -> CString


type data_lit =
    | Integer of int
    | Decimal of float
    | Char of char
    | Bool of bool
    | Const_str of string * int * int
    | Const_cstr of string * int
[@@deriving show { with_path = false }]

let string_of_data_lit = function
    | Integer i -> string_of_int i
    | Decimal f -> string_of_float f
    | Char c -> String.make 1 c
    | Bool b -> string_of_bool b
    | Const_str (str, off, len) -> sprintf "\"%s\" (strs[%d], %d)" (String.escaped str) off len
    | Const_cstr (str, off) -> sprintf "\"%s\" (strs[%d])" (String.escaped str) off 

let (data_lit_of_data_tok : data_tok -> data_lit) = function
    | Integer i -> Integer i
    | Decimal f -> Decimal f
    | Char c -> Char c
    | Bool b -> Bool b
    | String _ | CString _ -> failwith "not directly convertible"

let (type_gen_of_data_lit : data_lit -> type_gen) = function
    | Integer   _ -> Integer
    | Decimal   _ -> Floating
    | Char      _ -> Character
    | Bool      _ -> Boolean
    | Const_str _ -> String
    | Const_cstr _ -> CString

(*
   data of low-level type
 *)
type data_ll =
    | I8 of int | I16 of int | I32 of int | I64 of int
    | U8 of int | U16 of int | U32 of int | U64 of int
    | Bool of bool
    | F32 of float | F64 of float
    | Ptr of type_ll * string * int
[@@deriving show { with_path = false }]

let string_of_data_ll = function
    | I8 d | I16 d | I32 d | I64 d -> string_of_int d
    | U8 d | U16 d | U32 d | U64 d -> string_of_int d
    | Bool b -> string_of_bool b
    | F32 f | F64 f -> string_of_float f
    | Ptr (_, s, i) -> sprintf "&%s[%d]" s i

let (type_of_data_ll : data_ll -> type_ll) = function
    | I8  _ -> I8 | I16 _ -> I16 | I32 _ -> I32 | I64 _ -> I64
    | U8  _ -> U8 | U16 _ -> U16 | U32 _ -> U32 | U64 _ -> U64
    | F32 _ -> F32 | F64 _ -> F64
    | Bool _ -> Bool
    | Ptr (t, _, _) -> Ptr t


(*
   data of high-level type
 *)
type data_hl =
    | Primitive of data_ll
    | Struc of (string * data_hl) list
    | Union of (string * data_hl) list
    | Str of string * int * int
    | CStr of string * int
    | Ptr of type_hl * string * int
    | Literal of data_lit
[@@deriving show { with_path = false }]

let string_of_data_hl data =
    let rec add_string_of_name_data acc (name, data) =
        acc ^ sprintf "%s: %s\n" name (string_of_data_hl' data)

    and string_of_data_hl' = function
    | Primitive t -> string_of_data_ll t
    | Struc tl -> sprintf "struc { %s }" (List.fold_left add_string_of_name_data "" tl)
    | Union tl -> sprintf "union { %s }" (List.fold_left add_string_of_name_data "" tl)
    | Str (str, off, len) -> sprintf "%S : str (strs[%d], %d)" (String.escaped str) off len
    | CStr (str, off) -> sprintf "%S : cstr (strs[%d])" (String.escaped str) off
    | Ptr (t, s, i) -> sprintf "%s[%d] : %s ptr" s i (string_of_type_hl t)
    | Literal l -> "(LITERAL) " ^ string_of_data_lit l
    in
    string_of_data_hl' data

let type_of_data_hl data =
    let rec name_type_of_name_data (name, data) =
        name, type_of_data_hl' data

    and (type_of_data_hl' : data_hl -> type_hl) = function
        | Primitive t -> Primitive (type_of_data_ll t)
        | Struc l -> Struc (List.map name_type_of_name_data l)
        | Union l -> Union (List.map name_type_of_name_data l)
        | Str _ -> Str
        | CStr  _ -> CStr
        | Ptr (t, _, _) -> Ptr t
        | Literal d -> General (type_gen_of_data_lit d)
    in
    type_of_data_hl' data

let data_ll_of_data_hl = function
    | Primitive data_ll -> data_ll
    | Str (_, off, _)
    | CStr (_, off) -> Ptr (U8, "strs", off)
    | Ptr (t, s, i) -> Ptr (type_ll_of_type_hl t, s, i)
    | _ -> failwith "not directly convertible"

let data_hl_of_data_lit loc (t : type_hl) data =
    match data, t with
    | Integer i, Primitive p ->
            Primitive
            (match p with
            | I8 -> I8 i | I16 -> I16 i | I32 -> I32 i | I64 -> I64 i
            | U8 -> U8 i | U16 -> U16 i | U32 -> U32 i | U64 -> U64 i
            | _ -> raise @@ Error (loc,
                sprintf "cannot concretize %s with type %s"
                (string_of_type_gen @@ type_gen_of_data_lit data)
                (string_of_type_hl t)))

    | Decimal d, Primitive p ->
            Primitive
            (match p with
            | F32 -> F32 d | F64 -> F64 d
            | _ -> raise @@ Error (loc,
                sprintf "cannot concretize %s with type %s"
                (string_of_type_gen @@ type_gen_of_data_lit data)
                (string_of_type_hl t)))

    | Char c, Primitive U8 -> Primitive (U8 (Char.code c))
    | Bool b, Primitive Bool -> Primitive (Bool b)
    | Const_str (str, off, len), Str -> Str (str, off, len)
    | Const_cstr (str, off), CStr -> CStr (str, off)
    | _ -> raise @@ Error (loc, sprintf "cannot concretize %s with type %s" (string_of_data_lit data) (string_of_type_hl t))


(* read file from the current dir *)
let read_src_file filename =
    if not (String.ends_with ~suffix:".ab" filename) then
        (let loc = { filename; included_from = []; expanded_from = []; row = 0; col = 0 } in
        raise @@ Error (loc, "Aback source files should have '.ab' extension"));

    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
