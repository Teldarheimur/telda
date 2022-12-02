use std::{collections::HashMap, fmt::{self, Write, Display}};

use crate::{mem::Memory, cpu::{Registers, ByteRegister, WideRegister}};

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
    pub does_not_end_function: bool,
    pub next_instruction_location: u16,
}

pub fn disassemble_instruction<'a>(location: u16, binary_code: &[u8], labels_discored: &mut Vec<&'a str>, labels: &'a HashMap<u16, String>) -> DisassembledInstruction {
    use crate::isa::*;
    let r = &mut Registers::new(location);
    let m = &mut StrictMemory { slice: binary_code } as &mut dyn Memory;

    let addr = r.pc;
    let opcode = m.read(addr);
    r.pc += 1;

    let mut op = String::with_capacity(32);
    let f = &mut op;
    let mut does_not_end_function = true;

    match opcode {
        NULL => {
            write!(f, "null").unwrap();
            does_not_end_function = false;
        }
        HALT => {
            write!(f, "halt").unwrap();
            does_not_end_function = false;
        }
        NOP => write!(f, "nop").unwrap(),
        PUSH_B => {
            let big_r = byte_big_r(r, m);
            write!(f, "push {big_r}").unwrap();
        }
        PUSH_W => {
            let big_r = wide_big_r(r, m);
            write!(f, "push {big_r}").unwrap();
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
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                write!(f, "call {lbl}").unwrap();
                labels_discored.push(lbl);
            } else {
                write!(f, "call 0x{w:02x}").unwrap();
            }
        }
        RET => {
            let b = arg_imm_byte(r, m);
            write!(f, "ret {b}").unwrap();
            does_not_end_function = false;
        }
        STORE_B => {
            let (r1, r2) = arg_wide_byte_registers(r, m);
            let offset = wide_big_r(r, m);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        STORE_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let offset = wide_big_r(r, m);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        LOAD_B => {
            let (r1, r2) = arg_byte_wide_registers(r, m);
            let offset = wide_big_r(r, m);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        LOAD_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let offset = wide_big_r(r, m);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        JUMP => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                write!(f, "jmp {lbl}").unwrap();
                labels_discored.push(lbl);
                does_not_end_function = false;
            } else {
                println!("jmp 0x{w:02x}");
                println!("<0x{w:02x}>:");
                r.pc = w;
            }
        }
        JUMP_REG => {
            let (wr, _) = arg_wide_registers(r, m);
            write!(f, "jmp {wr}").unwrap();
            does_not_end_function = false;
        }
        JEZ => cjmp("jez", r, m, labels, labels_discored, f),
        JLT => cjmp("jlt", r, m, labels, labels_discored, f),
        JLE => cjmp("jle", r, m, labels, labels_discored, f),
        JGT => cjmp("jgt", r, m, labels, labels_discored, f),
        JGE => cjmp("jge", r, m, labels, labels_discored, f),
        JNZ => cjmp("jnz", r, m, labels, labels_discored, f),
        JO => cjmp("jo", r, m, labels, labels_discored, f),
        JNO => cjmp("jno", r, m, labels, labels_discored, f),
        JB => cjmp("jb", r, m, labels, labels_discored, f),
        JAE => cjmp("jae", r, m, labels, labels_discored, f),
        JA => cjmp("ja", r, m, labels, labels_discored, f),
        JBE => cjmp("jbe", r, m, labels, labels_discored, f),
        ADD_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            write!(f, "add {r1}, {r2}, {big_r}").unwrap();
        }
        ADD_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            write!(f, "add {r1}, {r2}, {big_r}").unwrap();
        }
        SUB_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            write!(f, "sub {r1}, {r2}, {big_r}").unwrap();
        }
        SUB_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            write!(f, "sub {r1}, {r2}, {big_r}").unwrap();
        }
        AND_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            write!(f, "and {r1}, {r2}, {big_r}").unwrap();
        }
        AND_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            write!(f, "and {r1}, {r2}, {big_r}").unwrap();
        }
        OR_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            write!(f, "or {r1}, {r2}, {big_r}").unwrap();
        }
        OR_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            write!(f, "or {r1}, {r2}, {big_r}").unwrap();
        }
        XOR_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            write!(f, "xor {r1}, {r2}, {big_r}").unwrap();
        }
        XOR_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
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
            does_not_end_function = false;
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
        annotated_source: annotated_source,
        does_not_end_function,
        next_instruction_location,
    }
}

fn cjmp<'a>(name: &str, r: &mut Registers, m: &dyn Memory, labels: &'a HashMap<u16, String>, labels_discored: &mut Vec<&'a str>, f: &mut String) {
    let w = crate::isa::arg_imm_wide(r, m);
    if let Some(lbl) = labels.get(&w) {
        write!(f, "{name} {lbl}").unwrap();
        labels_discored.push(lbl);
    } else {
        write!(f, "{name} 0x{w:02x}").unwrap();
    }
}

enum BigR {
    Byte(u8),
    Wide(u16),
    BReg(ByteRegister),
    WReg(WideRegister),
}

#[inline]
fn byte_big_r(r: &mut Registers, m: &dyn Memory) -> BigR {
    let operand = m.read(r.pc);
    r.pc += 1;
    if operand >= 8 {
        BigR::Byte(operand - 7)
    } else {
        BigR::BReg(ByteRegister::new(operand))
    }
}
#[inline]
fn wide_big_r(r: &mut Registers, m: &dyn Memory) -> BigR {
    let operand = m.read_wide(r.pc);
    r.pc += 2;
    if operand >= 8 {
        BigR::Wide(operand - 7)
    } else {
        BigR::WReg(WideRegister::new(operand as u8))
    }
}

impl Display for BigR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BigR::Byte(b) => write!(f, "0x{b:02x}"),
            BigR::Wide(w) => write!(f, "0x{w:03x}"),
            BigR::BReg(r) => r.fmt(f),
            BigR::WReg(r) => r.fmt(f),
        }
    }
}
