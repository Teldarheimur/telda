use std::fmt::{self, Write, Display};

use crate::{mem::Memory, cpu::{Registers, ByteRegister, WideRegister, IoPort}};

struct StrictMemory<'a> {
    slice: &'a [u8]
}

impl Memory for StrictMemory<'_> {
    fn read(&self, addr: u16) -> u8 {
        self.slice.get(addr as usize).copied().unwrap_or(0)
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

struct UnreachablePort;

impl IoPort for UnreachablePort {
    fn read(&mut self) -> u8 {
        unreachable!()
    }
    fn write(&mut self, _val: u8) {
        unreachable!()
    }
}

pub fn disassemble_instruction<'a, F: FnOnce(u16) -> Option<&'a str>>(location: u16, binary_code: &[u8], label_lookup: F) -> DisassembledInstruction {
    use crate::isa::*;
    let r = &mut Registers::new(location, Box::new(UnreachablePort));
    let m = &mut StrictMemory { slice: binary_code } as &mut dyn Memory;

    let addr = r.pc;
    let opcode = m.read(addr);
    r.pc += 1;

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
        SETH => {
            let b = arg_imm_byte(r, m);
            write!(f, "seth {b}").unwrap();
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
            let big_r = byte_big_r(r, m).looked_up(label_lookup);
            write!(f, "push {}", big_r).unwrap();
        }
        PUSH_W => {
            let big_r = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "push {}", big_r).unwrap();
        }
        POP_B => {
            let (r1, _r2) = arg_byte_registers(r, m);
            write!(f, "pop {r1}").unwrap();
        }
        POP_W => {
            let (r1, _r2) = arg_wide_registers(r, m);
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
        STORE_B => {
            let (r1, r2) = arg_wide_byte_registers(r, m);
            let offset = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        STORE_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let offset = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        LOAD_B => {
            let (r1, r2) = arg_byte_wide_registers(r, m);
            let offset = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        LOAD_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let offset = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        JUMP => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = label_lookup(w) {
                write!(f, "jmp {lbl}").unwrap();
                ends_block = true;
            } else {
                // TODO: this is wrong
                println!("jmp 0x{w:02x}");
                println!("<0x{w:02x}>:");
                r.pc = w;
            }
        }
        JUMP_REG => {
            let (wr, _) = arg_wide_registers(r, m);
            write!(f, "jmp {wr}").unwrap();
            ends_block = true;
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
        ADD_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m).looked_up(label_lookup);
            write!(f, "add {r1}, {r2}, {big_r}").unwrap();
        }
        ADD_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "add {r1}, {r2}, {big_r}").unwrap();
        }
        SUB_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m).looked_up(label_lookup);
            write!(f, "sub {r1}, {r2}, {big_r}").unwrap();
        }
        SUB_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "sub {r1}, {r2}, {big_r}").unwrap();
        }
        AND_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m).looked_up(label_lookup);
            write!(f, "and {r1}, {r2}, {big_r}").unwrap();
        }
        AND_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "and {r1}, {r2}, {big_r}").unwrap();
        }
        OR_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m).looked_up(label_lookup);
            write!(f, "or {r1}, {r2}, {big_r}").unwrap();
        }
        OR_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "or {r1}, {r2}, {big_r}").unwrap();
        }
        XOR_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m).looked_up(label_lookup);
            write!(f, "xor {r1}, {r2}, {big_r}").unwrap();
        }
        XOR_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m).looked_up(label_lookup);
            write!(f, "xor {r1}, {r2}, {big_r}").unwrap();
        }
        MUL_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let (r3, r4) = arg_byte_registers(r, m);
            write!(f, "mul {r1}, {r2}, {r3}, {r4}").unwrap()
        }
        MUL_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let (r3, r4) = arg_wide_registers(r, m);
            write!(f, "mul {r1}, {r2}, {r3}, {r4}").unwrap()
        }
        DIV_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let (r3, r4) = arg_byte_registers(r, m);
            write!(f, "div {r1}, {r2}, {r3}, {r4}").unwrap()
        }
        DIV_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let (r3, r4) = arg_wide_registers(r, m);
            write!(f, "div {r1}, {r2}, {r3}, {r4}").unwrap()
        }
        b => {
            write!(f, "0x{b:02x}").unwrap();
            ends_block = true;
        }
    }

    let next_instruction_location = r.pc;


    let mut annotated_source = String::with_capacity(op.len()+21);
    write!(&mut annotated_source, "  {addr:04x}: ").unwrap();
    for b in binary_code[addr as usize..next_instruction_location as usize].iter() {
        write!(&mut annotated_source, " {b:02x}").unwrap();
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

fn cjmp<'a, F: FnOnce(u16) -> Option<&'a str>>(name: &str, r: &mut Registers, m: &dyn Memory, label_lookup: F, f: &mut String) {
    write!(f, "{name} {}", Operand::Wide(crate::isa::arg_imm_wide(r, m)).looked_up(label_lookup)).unwrap();
}

enum Operand<'a> {
    Byte(u8),
    Wide(u16),
    Label(&'a str),
    BReg(ByteRegister),
    WReg(WideRegister),
}

#[inline]
fn byte_big_r(r: &mut Registers, m: &dyn Memory) -> Operand<'static> {
    let operand = m.read(r.pc);
    r.pc += 1;
    if operand >= 8 {
        Operand::Byte(operand - 7)
    } else {
        Operand::BReg(ByteRegister::new(operand))
    }
}
#[inline]
fn wide_big_r(r: &mut Registers, m: &dyn Memory) -> Operand<'static> {
    let operand = m.read_wide(r.pc);
    r.pc += 2;
    if operand >= 8 {
        Operand::Wide(operand - 7)
    } else {
        Operand::WReg(WideRegister::new(operand as u8))
    }
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
            Self::BReg(r) => r.fmt(f),
            Self::WReg(r) => r.fmt(f),
        }
    }
}
