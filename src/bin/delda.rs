use std::{fs::File, env::args, io::{BufReader, Read, BufRead}, collections::HashMap, fmt::{Display, self}};

use telda2::{mem::{Lazy, Memory}, cpu::{Cpu, Registers, Register}};

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
    use telda2::isa::{ADD, AND, CALL, HALT, JEZ, JGE, JGT, JLE, JLT, JNZ, JUMP, LDSTK, LOAD, NOP, NULL, OR, POP, PUSH, READ, RET, STORE, STSTK, SUB, WRITE, XOR, read_hr, read_registers, read_rh, read_wide};
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
        WRITE => {
            let big_r = read_big_r(r, m);
            print!("write {big_r}");
        }
        READ => {
            let (r1, _r2) = read_registers(r, m);
            print!("read {r1}");
        }
        HALT => {
            print!("halt");
            ret = false;
        }
        NOP => print!("nop"),
        PUSH => {
            let big_r = read_big_r(r, m);
            print!("push {big_r}");
        }
        CALL => {
            let w = read_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("call {lbl}");
                new_labels.push(lbl);
            } else {
                print!("call 0x{w:02x}");
            }
        }
        JUMP => {
            let w = read_wide(r, m);
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
        RET => {
            let b = m.read(r.pc);
            r.pc += 1;
            print!("ret {b}");
            ret = false;
        }
        POP => {
            let (r1, _r2) = read_registers(r, m);
            print!("pop {r1}");
        }
        LDSTK => {
            let (r1, h) = read_rh(r, m);
            print!("ldstk {r1}, {h}");
        }
        STORE => {
            let (r1, r2) = read_registers(r, m);
            let big_r = read_big_r(r, m);
            print!("store {r1}, {r2}, {big_r}");
        }
        STSTK => {
            let (h, r1) = read_hr(r, m);
            print!("ststk {h}, {r1}");
        }
        LOAD => {
            let (r1, r2) = read_registers(r, m);
            let (r3, _r4) = read_registers(r, m);
            print!("load {r1}, {r2}, {r3}");
        }
        JEZ => {
            let w = read_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("jez {lbl}");
                new_labels.push(lbl);
            } else {
                print!("jez 0x{w:02x}");
            }
        }
        JLT => {
            let w = read_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JLT {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JLT 0x{w:02x}");
            }
        }
        JLE => {
            let w = read_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JLE {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JLE 0x{w:02x}");
            }
        }
        JGT => {
            let w = read_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JGT {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JGT 0x{w:02x}");
            }
        }
        JGE => {
            let w = read_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JGE {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JGE 0x{w:02x}");
            }
        }
        JNZ => {
            let w = read_wide(r, m);
            if let Some(lbl) = labels.get(&w) {
                print!("JNZ {lbl}");
                new_labels.push(lbl);
            } else {
                print!("JNZ 0x{w:02x}");
            }
        }
        ADD => {
            let (r1, r2) = read_registers(r, m);
            let big_r = read_big_r(r, m);
            print!("add {r1}, {r2}, {big_r}");
        }
        SUB => {
            let (r1, r2) = read_registers(r, m);
            let big_r = read_big_r(r, m);
            print!("sub {r1}, {r2}, {big_r}");
        }
        AND => {
            let (r1, r2) = read_registers(r, m);
            let big_r = read_big_r(r, m);
            print!("and {r1}, {r2}, {big_r}");
        }
        OR => {
            let (r1, r2) = read_registers(r, m);
            let big_r = read_big_r(r, m);
            print!("or {r1}, {r2}, {big_r}");
        }
        XOR => {
            let (r1, r2) = read_registers(r, m);
            let big_r = read_big_r(r, m);
            print!("xor {r1}, {r2}, {big_r}");
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
    Reg(Register)
}

fn read_big_r(r: &mut Registers, m: &dyn Memory) -> BigR {
    let operand = m.read(r.pc);
    r.pc += 1;
    if operand >= 8 {
        BigR::Byte(operand - 7)
    } else {
        BigR::Reg(Register::new(operand))
    }
}

impl Display for BigR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BigR::Byte(b) => write!(f, "0x{b:02x}"),
            BigR::Reg(r) => r.fmt(f),
        }
    }
}