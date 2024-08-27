// HEADER

#ifdef APPLE
    #define scall_reg x16
    #define scall_int #0x80
#else
    #define scall_reg x8
    #define scall_int #0x0
#endif

.text
.globl _start
_start:
    ldr x0, [sp]
    add x1, sp, #8

    bl  main_proc
    b   exit_proc

.macro def_scall name num
.globl \name
\name:
    mov scall_reg, \num
    svc scall_int

    ret
.endm

