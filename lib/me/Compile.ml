open IR_generator

let compile parser_output = 
    generate_qbe_ir stdout parser_output;
    flush stdout

(*
open Unix
let compile parser_output = 
    let (pipe1_read, pipe1_write) = pipe () in

    let qbe_pid = create_process "qbe" [| "qbe" |] pipe1_read stdout stderr in
    close pipe1_read;

    let out_channel = out_channel_of_descr pipe1_write in
    generate_qbe_ir out_channel parser_output;
    flush out_channel;

    waitpid qbe_pid
 *)
