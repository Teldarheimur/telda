# Virtual kernel

The virtual kernel is an implentantion of the kernel handled by the telda emulator (`t`) instead of one in Telda to be able to write telda programs before a
working Telda OS has been written. It also will have basic I/O with the host system so that you can e.g. open, read and write to files.
Below will be an overview of the currently implemented syscalls and first a short overview of how to use them.

## Using syscalls in assembly

A special instruction `syscall` initiates a syscall trap which the kernel handles. The instruction itself is one byte and takes with it no operands, instead
arguments to syscalls are put in registers, like you would do with calling a function you have made. `r1` holds the syscall number and determines which syscall
you want. `r2`-`r4` hold arguments and the syscall might use those registers to return values back to you.

```text
SYSCALL  | NO | ARGS | DESCRIPTION
dbg      | 00 |      | Various debugging, right now it prints the memory mapping
cin      | 03 | >r1l | Reads one byte from STDIN (or an in port) to r1l
cout     | 04 | r2l  | Writes one byte from r2l to STDOUT (or an out port)
errhandl | 15 | r2   | Installs an error handler. The kernel will jump to the location in r2 to handle errors. See the section on error handling
...
```

`cin` and `cout` are mostly here for legacy purposes. Syscalls similar to Linux' `read` and `write` will mostly replace these and `cin` and `cout` might be unsupported in future kernels. Likewise `dbg` will have various effects and is reserved for misc. debugging purposes and will probably be non-existant in many kernels.

## Error handler

When an error like division by zero happens, the kernel will hand execution over to the installed error handler (syscall `errhandl`). Before doing this, the number of the error will be written to `r1`. Currently the codes are as follows:

- `0x5` syscall (error handler will not see this)
- `0x8` zerodiv (a division or remainder instruction was given 0 as the RHS operand)
- `0xa` halt (error handler will not see this)
- `0xe` level 1 page fault
- `0xf` level 2 page fault
- `0x10` illegal operation
- `0x11` illegal read (tried to read in a segment with the read permission bit unset)
- `0x12` illegal write (tried to write in a segment with the write permission bit unset)
- `0x13` illegal execute (program counter is in a segment without execute permissions)
- `0x1f` illegal handler return (tried to return from handler without the trap flag being set)
