use std::{fs::File, env::args, io::{BufReader, Read, BufRead}, collections::HashMap, fmt::{Display, self}};

use telda2::{mem::{Lazy, Memory}, cpu::{Cpu, Registers, ByteRegister, WideRegister}};

fn main() {
    for arg in args().skip(1) {
        let mut mem = Vec::new();
        let mut f = File::open(format!("{arg}.bin")).unwrap();
        f.read_to_end(&mut mem).unwrap();
        let mut labels = HashMap::new();
        let mut pos_to_labels = HashMap::new();
        let f = File::open(format!("{arg}.symbols")).unwrap();
        for line in BufReader::new(f).lines() {
            let line = line.unwrap();
            let colon = line.find(':').unwrap();
            let lbl = line[..colon].to_owned();
            let pos = u16::from_str_radix(&line[colon+4..], 16).unwrap();
            labels.insert(
                lbl.clone(),
                pos
            );
            pos_to_labels.insert(
                pos,
                lbl
            );
        }


        let mem: &dyn Memory = &Lazy { mem };
        let mut regs = Cpu::new(0).registers;
        regs.a = 0xfffe;
        regs.b = 0xfdfc;
        regs.x = 0x6532;
        regs.y = 0xabab;

        let mut printed_labels = vec!["_start"];
        let mut found_labels = vec!["_start"];

        loop {
            if found_labels.is_empty() {
                break;
            }

            let mut new_labels = Vec::new();
            for label in found_labels {
                printed_labels.push(label);
                regs.pc = labels[label];

                println!("<{label}>:");
                while read_instruction(&mut regs, mem, &mut new_labels, &pos_to_labels) {

                }
                println!();
            }
            new_labels.sort();
            new_labels.dedup();
            new_labels.retain(|s| !printed_labels.contains(s));

            found_labels = new_labels;
        }
    }
}

fn read_instruction<'a>(r: &mut Registers, m: &dyn Memory, new_labels: &mut Vec<&'a str>, labels: &'a HashMap<u16, String>) -> bool {
    use telda2::isa::*;
    let addr = r.pc;
    let opcode = m.read(addr);
    r.pc += 1;

    let mut ret = true;

    print!("    {addr:04x}: ");

    match opcode {
        NULL => {
            print!("null");
            ret = false;
        }
        HALT => {
            print!("halt");
            ret = false;
        }
        NOP => print!("nop"),
        PUSH_B => {
            let big_r = byte_big_r(r, m);
            print!("push {big_r}");
        }
        PUSH_W => {
            let big_r = wide_big_r(r, m);
            print!("push {big_r}");
        }
        POP_B => {
            let (r1, _r2) = arg_byte_registers(r, m);
            print!("pop {r1}");
        }
        POP_W => {
            let (r1, _r2) = arg_wide_registers(r, m);
            print!("pop {r1}");
        }
        CALL => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("call {lbl}");
                new_labels.push(lbl);
            } else {
                print!("call 0x{w:02x}");
            }
        }
        RET => {
            let b = arg_imm_byte(r, m);
            print!("ret {b}");
            ret = false;
        }
        STORE_B => {
            let (r1, r2) = arg_wide_byte_registers(r, m);
            let offset = wide_big_r(r, m);
            print!("store {r1}, {offset}, {r2}");
        }
        STORE_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let offset = wide_big_r(r, m);
            print!("store {r1}, {offset}, {r2}");
        }
        LOAD_B => {
            let (r1, r2) = arg_byte_wide_registers(r, m);
            let offset = wide_big_r(r, m);
            print!("load {r1}, {r2}, {offset}");
        }
        LOAD_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let offset = wide_big_r(r, m);
            print!("load {r1}, {r2}, {offset}");
        }
        JUMP => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("jmp {lbl}");
                new_labels.push(lbl);
                ret = false;
            } else {
                println!("jmp 0x{w:02x}");
                println!("<0x{w:02x}>:");
                r.pc = w;
            }
        }
        JUMP_REG => {
            let (wr, _) = arg_wide_registers(r, m);
            print!("jmp {wr}");
            ret = false;
        }
        JEZ => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("jez {lbl}");
                new_labels.push(lbl);
            } else {
                print!("jez 0x{w:02x}");
            }
        }
        JLT => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JLT {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JLT 0x{w:02x}");
            }
        }
        JLE => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JLE {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JLE 0x{w:02x}");
            }
        }
        JGT => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JGT {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JGT 0x{w:02x}");
            }
        }
        JGE => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JGE {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JGE 0x{w:02x}");
            }
        }
        JNZ => {
            let w = arg_imm_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JNZ {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JNZ 0x{w:02x}");
            }
        }
        ADD_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            print!("add {r1}, {r2}, {big_r}");
        }
        ADD_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            print!("add {r1}, {r2}, {big_r}");
        }
        SUB_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            print!("sub {r1}, {r2}, {big_r}");
        }
        SUB_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            print!("sub {r1}, {r2}, {big_r}");
        }
        AND_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            print!("and {r1}, {r2}, {big_r}");
        }
        AND_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            print!("and {r1}, {r2}, {big_r}");
        }
        OR_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            print!("or {r1}, {r2}, {big_r}");
        }
        OR_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            print!("or {r1}, {r2}, {big_r}");
        }
        XOR_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let big_r = byte_big_r(r, m);
            print!("xor {r1}, {r2}, {big_r}");
        }
        XOR_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let big_r = wide_big_r(r, m);
            print!("xor {r1}, {r2}, {big_r}");
        }
        MUL_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let (r3, r4) = arg_byte_registers(r, m);
            print!("mul {r1}, {r2}, {r3}, {r4}")
        }
        MUL_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let (r3, r4) = arg_wide_registers(r, m);
            print!("mul {r1}, {r2}, {r3}, {r4}")
        }
        DIV_B => {
            let (r1, r2) = arg_byte_registers(r, m);
            let (r3, r4) = arg_byte_registers(r, m);
            print!("div {r1}, {r2}, {r3}, {r4}")
        }
        DIV_W => {
            let (r1, r2) = arg_wide_registers(r, m);
            let (r3, r4) = arg_wide_registers(r, m);
            print!("div {r1}, {r2}, {r3}, {r4}")
        }
        b => {
            print!("0x{b:02x}");
            ret = false;
        }
    }
    println!();

    ret
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
