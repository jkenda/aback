// HEADER

.text
.globl _start
_start:
    ld a0, 0(sp)
    addi a1, sp, 8

    call main_proc
    j    exit_proc

.macro def_scall name num
.globl \name
\name:
    li a7, \num
    ecall
    ret
.endm
