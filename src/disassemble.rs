use std::fmt::{self, Write, Display};

use crate::{mem::{Memory, IO_MAPPING_CUTOFF}, cpu::{Registers, ByteRegister, WideRegister, R0}, U4, isa::{arg_pair, arg_imm_wide}};

struct StrictMemory<'a> {
    slice: &'a [u8]
}

impl Memory for StrictMemory<'_> {
    fn read(&mut self, addr: u16) -> u8 {
        if addr < IO_MAPPING_CUTOFF {
            self.slice.get(addr as usize).copied().unwrap_or(0)
        } else {
            unimplemented!("no I/O for strict memory")
        }
    }
    fn write(&mut self, _addr: u16, _val: u8) {
        unimplemented!("no writing to strict memory")
    }
}

pub struct DisassembledInstruction {
    pub annotated_source: String,
    pub ends_block: bool,
    pub nesting_difference: i32,
    pub next_instruction_location: u16,
}

fn id<T>(x: T) -> T { x }

pub fn disassemble_instruction<'a, F: FnOnce(u16) -> Option<&'a str>>(location: u16, binary_code: &[u8], label_lookup: F) -> DisassembledInstruction {
    use crate::isa::*;
    let r = &mut Registers::new(location);
    let m = &mut StrictMemory { slice: binary_code } as &mut dyn Memory;

    let addr = r.program_counter;
    let opcode = m.read(addr);
    r.program_counter += 1;

    let mut op = String::with_capacity(32);
    let f = &mut op;
    let mut nesting_difference = 0;
    let mut ends_block = false;

    match opcode {
        NULL => {
            write!(f, "null").unwrap();
            ends_block = true;
        }
        HALT => {
            write!(f, "halt").unwrap();
            ends_block = true;
        }
        CTF => {
            write!(f, "ctf").unwrap();
        }
        RETH => {
            write!(f, "reth").unwrap();
            ends_block = true;
        }
        NOP => write!(f, "nop").unwrap(),
        PUSH_B => {
            let (r1, _) = arg_pair(r, m, ByteRegister, id);
            write!(f, "push {r1}").unwrap();
        }
        PUSH_W => {
            let (r1, _) = arg_pair(r, m, WideRegister, id);
            write!(f, "push {r1}").unwrap();
        }
        POP_B => {
            let (r1, _r2) = arg_pair(r, m, ByteRegister, id);
            write!(f, "pop {r1}").unwrap();
        }
        POP_W => {
            let (r1, _r2) = arg_pair(r, m, WideRegister, id);
            write!(f, "pop {r1}").unwrap();
        }
        CALL => {
            let w = Operand::Wide(arg_imm_wide(r, m)).looked_up(label_lookup);
            write!(f, "call {w}").unwrap();
            nesting_difference = 1;
        }
        RET => {
            let b = arg_imm_byte(r, m);
            write!(f, "ret {b}").unwrap();
            nesting_difference = -1;
            ends_block = true;
        }
        STORE_BI => {
            let (r1, r2) = arg_pair(r, m, WideRegister, ByteRegister);
            let offset = Operand::Wide(arg_imm_wide(r, m)).looked_up(label_lookup);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        STORE_WI => {
            let (r1, r2) = arg_pair(r, m, WideRegister, WideRegister);
            let offset = Operand::Wide(arg_imm_wide(r, m)).looked_up(label_lookup);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        STORE_BR => {
            let (r1, r2) = arg_pair(r, m, WideRegister, WideRegister);
            let (r3, _) = arg_pair(r, m, ByteRegister, id);
            write!(f, "store {r1}, {r2}, {r3}").unwrap();
        }
        STORE_WR => {
            let (r1, r2) = arg_pair(r, m, WideRegister, WideRegister);
            let (r3, _) = arg_pair(r, m, WideRegister, id);
            write!(f, "store {r1}, {r2}, {r3}").unwrap();
        }
        LOAD_BI => {
            let (r1, r2) = arg_pair(r, m, ByteRegister, WideRegister);
            let offset = Operand::Wide(arg_imm_wide(r, m)).looked_up(label_lookup);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        LOAD_WI => {
            let (r1, r2) = arg_pair(r, m, WideRegister, WideRegister);
            let offset = Operand::Wide(arg_imm_wide(r, m)).looked_up(label_lookup);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        LOAD_BR => {
            let (r1, r2) = arg_pair(r, m, ByteRegister, WideRegister);
            let (r3, _) = arg_pair(r, m, WideRegister, id);
            write!(f, "load {r1}, {r2}, {r3}").unwrap();
        }
        LOAD_WR => {
            let (r1, r2) = arg_pair(r, m, WideRegister, WideRegister);
            let (r3, _) = arg_pair(r, m, WideRegister, id);
            write!(f, "load {r1}, {r2}, {r3}").unwrap();
        }
        JEZ => cjmp("jez", r, m, label_lookup, f),
        JLT => cjmp("jlt", r, m, label_lookup, f),
        JLE => cjmp("jle", r, m, label_lookup, f),
        JGT => cjmp("jgt", r, m, label_lookup, f),
        JGE => cjmp("jge", r, m, label_lookup, f),
        JNZ => cjmp("jnz", r, m, label_lookup, f),
        JO => cjmp("jo", r, m, label_lookup, f),
        JNO => cjmp("jno", r, m, label_lookup, f),
        JB => cjmp("jb", r, m, label_lookup, f),
        JAE => cjmp("jae", r, m, label_lookup, f),
        JA => cjmp("ja", r, m, label_lookup, f),
        JBE => cjmp("jbe", r, m, label_lookup, f),
        LDI_B => {
            let (r1, _o) = arg_pair(r, m, ByteRegister, id);
            let b = arg_imm_byte(r, m);

            write!(f, "ldi {r1}, {}", Operand::Byte(b)).unwrap();
        }
        LDI_W => {
            let (r1, o) = arg_pair(r, m, WideRegister, u8::from);
            let w = Operand::Wide(arg_imm_wide(r, m)).looked_up(label_lookup);

            match u8::from(o) {
                // ldi
                0 => write!(f, "ldi {r1}, {w}").unwrap(),
                // jmp, jump
                1 => {
                    if r1 == R0 {
                        // jmp imm
                        write!(f, "jmp {w}").unwrap();
                        ends_block = true;
                    } else {
                        // jmp r
                        write!(f, "jmp {r1}").unwrap();
                        ends_block = true;
                    }
                }
                n => write!(f, "invalid ldi{n}, {r1}, {w}").unwrap(),
            }
        }
        ADD_B => binop("add", ByteRegister, r, m, f),
        ADD_W => binop("add", WideRegister, r, m, f),
        SUB_B => binop("sub", ByteRegister, r, m, f),
        SUB_W => binop("sub", WideRegister, r, m, f),
        AND_B => binop("and", ByteRegister, r, m, f),
        AND_W => binop("and", WideRegister, r, m, f),
        OR_B => binop("or", ByteRegister, r, m, f),
        OR_W => binop("or", WideRegister, r, m, f),
        XOR_B => binop("xor", ByteRegister, r, m, f),
        XOR_W => binop("xor", WideRegister, r, m, f),
        SHL_B => binop("shl", ByteRegister, r, m, f),
        SHL_W => binop("shl", WideRegister, r, m, f),
        ASR_B => binop("asr", ByteRegister, r, m, f),
        ASR_W => binop("asr", WideRegister, r, m, f),
        LSR_B => binop("lsr", ByteRegister, r, m, f),
        LSR_W => binop("lsr", WideRegister, r, m, f),
        DIV_B => binop("div", ByteRegister, r, m, f),
        DIV_W => binop("div", WideRegister, r, m, f),
        MUL_B => binop("mul", ByteRegister, r, m, f),
        MUL_W => binop("mul", WideRegister, r, m, f),
        b => {
            write!(f, "0x{b:02x}").unwrap();
            ends_block = true;
        }
    }

    let next_instruction_location = r.program_counter;


    let mut annotated_source = String::with_capacity(op.len()+21);
    write!(&mut annotated_source, "  {addr:04x}: ").unwrap();
    
    if let Some(slice) = binary_code.get(addr as usize..next_instruction_location as usize) {
        for b in slice {
            write!(&mut annotated_source, " {b:02x}").unwrap();
        }
    } else {
        for _ in addr as usize..next_instruction_location as usize {
            write!(&mut annotated_source, " __").unwrap();
        }
    }

    for _ in 0..(4-(next_instruction_location-addr)) {
        write!(&mut annotated_source, "   ").unwrap();
    }
    write!(&mut annotated_source, "    {op}").unwrap();

    DisassembledInstruction {
        annotated_source,
        next_instruction_location,
        ends_block,
        nesting_difference,
    }
}

fn cjmp<'a, F: FnOnce(u16) -> Option<&'a str>>(name: &str, r: &mut Registers, m: &mut dyn Memory, label_lookup: F, f: &mut String) {
    write!(f, "{name} {}", Operand::Wide(arg_imm_wide(r, m)).looked_up(label_lookup)).unwrap();
}

fn binop<T: Display, RF: Fn(U4) -> T>(name: &str, rf: RF, r: &mut Registers, m: &mut dyn Memory, f: &mut String) {
    let (r1, r2) = arg_pair(r, m, &rf, &rf);
    let (r3, _o) = arg_pair(r, m, &rf, &id);
    write!(f, "{name} {r1}, {r2}, {r3}").unwrap();
}

enum Operand<'a> {
    Byte(u8),
    Wide(u16),
    Label(&'a str),
}

impl<'a> Operand<'a> {
    pub fn looked_up<F: FnOnce(u16) -> Option<&'a str>>(mut self, label_lookup: F) -> Self {
        self.convert_wide_to_label(label_lookup);
        self
    }
    pub fn convert_wide_to_label<F: FnOnce(u16) -> Option<&'a str>>(&mut self, label_lookup: F) {
        match &*self {
            &Operand::Wide(w) => {
                if let Some(lbl) = label_lookup(w) {
                    *self = Operand::Label(lbl);
                }
            }
            _ => ()
        }
    }
}

impl Display for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte(b) => write!(f, "0x{b:02x}"),
            Self::Wide(w) => write!(f, "0x{w:03x}"),
            Self::Label(l) => l.fmt(f),
        }
    }
}
