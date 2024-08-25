open Unix

open Common
open Ir_generator

let wait_for = function
    | None -> ()
    | Some pid ->
            let status = snd @@ waitpid [] pid in
            match status with
            | WEXITED status ->
                    if status <> 0 then
                        exit status
            | _ -> ()

let compile options parser_output = 
    let path_out = Option.get options.path_out in

    let qbe_in, qbe_out = pipe ~cloexec:true ()
    and gcc_in, gcc_out = pipe ~cloexec:true () in

    let qbe_output, gcc_pid_opt =
        if List.mem Stdout_asm options.flags || List.mem Stdout_il options.flags then
            stdout, None
        else
            let pid_gcc = create_process "gcc" [| "gcc"; "-xassembler"; "-o"; path_out; "-" |] gcc_in stdout stderr in
            close gcc_in;
            gcc_out, Some pid_gcc
    in
    let aback_output, qbe_pid_opt =
        if List.mem Stdout_il options.flags then
            stdout, None
        else
            let pid_qbe = create_process "qbe" [| "qbe" |] qbe_in qbe_output stderr in
            close qbe_in;
            qbe_out, Some pid_qbe
    in

    let aback_out = out_channel_of_descr aback_output in
    generate_qbe_ir aback_out path_out parser_output;
    flush aback_out;

    if aback_output <> stdout then
        close aback_output;

    wait_for qbe_pid_opt;

    if qbe_output <> stdout then
        close qbe_output;

    wait_for gcc_pid_opt;
