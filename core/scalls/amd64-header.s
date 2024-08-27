// HEADER

.text
.globl _start
_start:
    // get argc, argv
    mov 0(%rsp), %rdi
    lea 8(%rsp), %rsi

    call main_proc

    mov %rax, %rdi
    jmp exit_proc

.macro def_scall name num
.globl \name
\name:
    push %rcx
    push %r11

    mov $\num, %rax
    syscall

    pop %r11
    pop %rcx

    ret
.endm

