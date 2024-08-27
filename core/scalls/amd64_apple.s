.equ main_proc, _main
.equ exit_proc, _exit


// SYSCALL TABLE

def_scall   _exit,   1
// ...
def_scall   _read,   3
def_scall   _write,  4
// ...
