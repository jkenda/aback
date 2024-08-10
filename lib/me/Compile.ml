open Format
open IR_generator

let compile parser_output = 
    let formatter = formatter_of_out_channel stdout in
    generate_qbe_ir formatter parser_output;
    pp_print_flush formatter ()

(*
open Unix
let compile parser_output = 
    let (pipe1_read, pipe1_write) = pipe () in

    match create_process "qbe" [| "qbe" |] pipe1_read stdout stderr with
    | _ -> close pipe1_read;

    let out_channel = out_channel_of_descr pipe1_write in

    let formatter = formatter_of_out_channel out_channel in
    generate_qbe_ir formatter parser_output;
    pp_print_flush formatter ()
*)
