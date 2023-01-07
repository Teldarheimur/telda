# telda

This is a silly little "CPU architecture" with an emulator and a bunch of other tools, including:
a compiler (assembler), linker, debugger and some tools for dealing with telda object files like `tobjdump` and `tstrip`.

It is written in rust and requires version Rust 1.65 to compile (watch out for this being out-of-date).
Following will be some documentation on the telda machine, tools and various other telda concepts.

## General specs

Telda is a 16-bit machine with data accesible both in 8-bit and 16-bit. That is,
general purpose register can be used both as bytes (8-bit) and wides (16-bit), but special purpose
registers and addresses are wides. Its address space is 2^16 and can so only access 64 kB of memory so far,
except address 0xffe0-0xffff are mapped to I/O.
In the future, support will be added for accessing more memory than 64kB using virtual memory pages.

Instructions are variable size varying between 1 and 4 bytes. The first byte is an opcode and uniquely
determines the following amount of bytes which encode the operands.

Lastly, the machine will sometimes trap (some instructions are meant specifically to trap, others will only do so if something is wrong). If no trap handler is installed, this will make the machine stop (and the 
emulator will show what trap mode it was). Otherwise, the machine will be able to handle traps itself
and this can also be used to implement things like syscalls.

## The byte registers

Byte registers can only access the general purpose register which are just numbered from 1-10.
Since registers all hold wide values, the byte registers have a suffix in their name indicating how
they access part of the whole register. -`l` means it accesses the lower byte, leaving the higher byte alone, -`h` means it accesses the higher byte, leaving the lower byte alone, and finally -`b` means it pretends the whole register is just a byte, which means that it reads the lower byte if you read from it, and if you write to it, it'll write to the lower byte and clear (zero) the higher byte.
`r1`-`r5` can take either `l` or `h`, whilst `r5-r10` only take `b`.

Lastly, the special (still general purpose) register `r0` only takes `r0b` as a convention. It could principally also use `r0l` and `r0h` since this register is always zero no matter what you write to it.

The following is a table of their encodings and names:

```
0 r0b   constant zero
1 r1l   correspond to least significant portion of r1
2 r1h   correspond to most  significant portion of r1
3 r2l   correspond to least significant portion of r2
4 r2h   correspond to most  significant portion of r2
5 r3l   correspond to least significant portion of r3
6 r3h   correspond to most  significant portion of r3
7 r4l   correspond to least significant portion of r4
8 r4h   correspond to most  significant portion of r4
9 r5l   correspond to least significant portion of r5
a r5h   correspond to most  significant portion of r5
b r6b   least significant portion of r6, most significant portion zeroes on writes
c r7b   least significant portion of r7, most significant portion zeroes on writes
d r8b   least significant portion of r8, most significant portion zeroes on writes
e r9b   least significant portion of r9, most significant portion zeroes on writes
f r10b  least significant portion of r10, most significant portion zeroes on writes
```

## The wide registers

The wide registers include the same general purpose registers `r0`-`r10` accesible as byte registers,
as well as the special purpose registers of which 5 are accesible through instructions (and therefore have an encoding).

```
0 r0    zero
1 r1    byte portions fully accessible
2 r2    byte portions fully accessible
3 r3    byte portions fully accessible
4 r4    byte portions fully accessible
5 r5    byte portions fully accessible
6 r6    high byte zeroes on byte writes
7 r7    high byte zeroes on byte writes
8 r8    high byte zeroes on byte writes
9 r9    high byte zeroes on byte writes
a r10   high byte zeroes on byte writes
b rs    stack pointer
c rl    link pointer (location of instruction after last `call`)
d rf    frame pointer (unused for now)
e rp    page table pointer (unused for now)
f rh    trap handler pointer
_ rpc   program counter, not accessible directly and therefore has no number
_ rflags flags
```

The stack pointer `rs` gets affected by stack operations. Its value is initially `0xffe0` on machine startup, putting it right before the I/O mapped memory. Pushing first decrements this pointer and then writes the value to this new location. Popping will first read the value and then increment the pointer.
The frame pointer `rf` has the same start value but is unused right now, but can be used to implement stack frames.

The link pointer `rl` is set by the `call` instruction to be the location of the instruction following
the call instruction. This is used by the `ret` instruction to return from the sub-routine by just setting the program counter to `rl` (and adding some value to `rs` for stack cleanup).
You will need to `push rl` in a sub-routine before calling another sub-routine in order to be able return again, because otherwise the return location is lost.

`rp` is unused for now but will store the location of the page table. A value of `0` indicates no page table is to be used and instead direct memory access.

`rh` has the location of the trap handler, it starts with the value `0` which indicates that no trap handler is set, if it's set to something else, then when a trap is triggered, the program counter will be set to `rh` after having pushed all registers to the stack. The trap mode will be written to `r1`
so that the trap handler can determine what to do based on this value. The instruction `reth` can be used
to return from a trap handler, which will pop all registers and continue execution.

Lastly, the names of the hidden registers `rpc` and `rflags` are subject to change since they are inaccessible.
They are the program counter and flags respectively. The program counter is the location of the next instruction to be loaded and run,
it gets updated when an instruction is read and by various other like jumps, `call`, `ret`, `reth`, ...
`rflags` are flags set by arithmetic instructions which conditional jumps depend on.

## Instruction and operand encoding

An instruction is encoded in three steps. First is the opcode which is one byte. This byte
uniquely determines the operands it takes and thus you can determine the size of the whole instruction
from this alone.

Secondly, all registers are encoded in order. Each register only takes up 4 bits, using first the most significant 4 bits of a byte, then the least significant,
and if the instruction takes an uneven number of registers, the least significant 4-bit portion of a byte has to be zero (as if it was r0(b)).
The operands _in the assembly language representation_ might have an immediate between some registers,
but the encoded representation will put all registers before any immediates.

Then lastly, any immediate operand will be written after it in little-endian byte order (if it is a wide,
if it's a byte, then byte order does not matter, haha).

Below is a table of the operand types, their sizes and what they mean. Use this as a legend for the
instruction table that will come in the next section.

```
OPERAND | SIZE | PURPOSE
br      | 4 b  | a byte register, (can be followed by another r)
wr      | 4 b  | a wide register, (can be followed by another r)
o       | 4 b  | options, instruction-dependant
w       | 16 b | a wide immediate
b       | 8 b  | a byte immediate
```

Note, that byte registers and wide registers can be encoded into the same byte. Since the opcode
determines the operand types, this leads to no problems.

## Instruction set

Below will just be table of all instructions and a description of what they do:

```
INSTRUCTION            | OPCODE | DESCRIPTION
null                   | 00     | invalid instruction, triggers invalid opcode trap (so do all opcodes not mentioned here)
...                    | 01-09  | ...
halt                   | 0a     | triggers halt trap
ctf                    | 0b     | clear trap flag
...                    | 0c     | ...
reth                   | 0d     | returns from trap handler, pops all registers, clears trap flag
...                    | 0e-1f  | ...
nop                    | 20     | no operation; does nothing
push br                | 21     | push byte value of register to stack (first decrementing `rs` by one and then writing there)
push wr                | 22     | push wide value of register to stack (first decrementing `rs` by two and then writin there)
pop br                 | 23     | pop byte value from stack into register (first reading byte at `rs` and then incrementing `rs` by one)
pop wr                 | 24     | pop wide value from stack into register (first reading wide at `rs` and then incrementing `rs` by two)
call w                 | 25     | Write next instruction location to `rl` and set program counter to w
ret b                  | 26     | Add `b` to stack pointer (removing b bytes from stack) and jump to `rl` (returns from sub-routine) (this is shorter than `jmp rl`)
store wr1, w, br2      | 27     | Write byte in br2 to memory at location [wr1 + w]
store wr1, w, wr2      | 28     | Write wide in wr2 to memory at location [wr1 + w] (in little-endian format)
store wr1, wr2, br3    | 29     | Write byte in br3 memory at location [wr1 + wr2]
store wr1, wr2, wr3    | 2a     | Write wide in wr3 memory at location [wr1 + wr2] (in little-endian format)
load br1, wr2, w       | 2b     | Load byte into br1 from location in memory [wr2 + w]
load wr1, wr2, w       | 2c     | Load wide into wr1 from location in memory [wr2 + w] (in little-endian format)
load br1, wr2, wr3     | 2d     | Load byte into br1 from location in memory [wr2 + wr3]
load wr1, wr2, wr3     | 2e     | Load wide into wr1 from location in memory [wr2 + wr3] (in little-endian format)
jez w                  | 2f     | Conditional jump to w if zero flag is set
jlt w                  | 30     | Conditional jump to w if sign flag is not equal to overflow flag
jle w                  | 31     | Conditional jump to w if sign flag is not equal to overflow flag AND zero flag is set
jgt w                  | 32     | Conditional jump to w if sign flag is equal to overflow flag and zero flag is not set
jge w                  | 33     | Conditional jump to w if sign flag is equal to overflow flag
jnz w                  | 34     | Conditional jump to w if zero flag is not set
jo  w                  | 35     | Conditional jump to w if overflow flag is set
jno w                  | 36     | Conditional jump to w if overflow flag is not set
jb,jc w                | 37     | Conditional jump to w if carry flag is set
jae,jnc w              | 38     | Conditional jump to w if carry flag is not set
ja  w                  | 39     | Conditional jump to w if carry flag and zero flag are both not set
jbe w                  | 3a     | Conditional jump to w if carry flag OR zero flag are set
...                    | 3b-3e  | funny stuff
ldi br, b              | 3f     | load immediate value into register
ldi wr, w              | 40     | load immediate value into register (encoded as `ldi wr, r0, w` per the rule about uneven number of registers)
jmp w                  | 40     | jumps to w (sets program counter to immediate value), encoded as `ldi r0, r1, w`
jmp wr                 | 40     | jumps to value in register, encoded as `ldi wr, r1 (wr!=0), 0`
add br1, br2, br3      | 41     | br1 = br2 + br3
add wr1, wr2, wr3      | 42     | wr1 = wr2 + wr3
sub br1, br2, br3      | 43     | br1 = br2 - br3
sub wr1, wr2, wr3      | 44     | wr1 = wr2 - wr3
and br1, br2, br3      | 45     | br1 = br2 & br3
and wr1, wr2, wr3      | 46     | wr1 = wr2 & wr3
or  br1, br2, br3      | 47     | br1 = br2 | br3
or  wr1, wr2, wr3      | 48     | wr1 = wr2 | wr3
xor br1, br2, br3      | 49     | br1 = br2 ^ br3
xor wr1, wr2, wr3      | 4a     | wr1 = wr2 ^ wr3
shl br1, br2, br3      | 4b     | br1 = br2 << br3
shl wr1, wr2, wr3      | 4c     | wr1 = wr2 << wr3
asr br1, br2, br3      | 4d     | br1 = br2 >> br3 (arithmetic, sign bit (most significant) is copied to the right)
asr wr1, wr2, wr3      | 4e     | wr1 = wr2 >> wr3 (arithmetic, sign bit (most significant) is copied to the right)
lsr br1, br2, br3      | 4f     | br1 = br2 >> br3 (logical)
lsr wr1, wr2, wr3      | 50     | wr1 = wr2 >> wr3 (logical)
div br1, br2, br3, br4 | 51     | br1 = br3 / br4; br2 = br3 % br4
div wr1, wr2, wr3, wr4 | 52     | wr1 = wr3 / wr4; wr2 = wr3 % wr4
mul br1, br2, br3, br4 | 53     | br2, br1 = br3 * br4 (br2 has the upper bytes)
mul wr1, wr2, wr3, wr4 | 54     | wr2, wr1 = wr3 * wr4 (wr2 has the upper bytes)
```

## Missing documentation

- Traps: what trap modes exist, what triggers each of them
- Telda object file format (álvur2)
- Flags: which flags are there, what sets them


## Tools

All tools take various options using `clap`, run them `-h` for help and (possibly) more information.

- `t` the emulator or telda binary object file runner. Runs objects with an entry point until they halt.
- `tc` the compiler/assembler, takes a `.telda` source file and compiles in into an object file with extension `.to`.
- `tl` the linker, combines several object files into one object resolving global symbols (included undefined ones (references)).
- `tobjdump` shows information about an object file like disassembly of its code, the symbol table and relocation entries in the disassembly.
- `tdbg` the debugger, runs an object file and disassembles it when stopping, giving you a prompt to determine how to continue or alter and inspect it during execution.
- `tstrip` removes unnecessary information from an object file.
