use std::{collections::HashMap, io::{Lines, BufRead}};

use crate::isa;

type Opcode = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Reg {
    Zero = 0,
    Al = 1,
    Ah = 2,
    Bl = 3,
    A = 4,
    B = 5,
    X = 6,
    Y = 7,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceOperand {
    Number(i32),
    Label(Box<str>),
    Reg(Reg),
}

#[derive(Debug, Clone)]
pub enum SourceLine {
    Label(String),
    Ins(String, Vec<SourceOperand>),
    Comment,
    DirString(String),
    DirByte(u8),
    DirWide(u16),
}

pub struct SourceLines<B> {
    lines: Lines<B>,
}

impl<B: BufRead> SourceLines<B> {
    pub fn new(r: B) -> Self {
        SourceLines {
            lines: r.lines()
        }
    }
}

impl<B: BufRead> Iterator for SourceLines<B> {
    type Item = SourceLine;
    fn next(&mut self) -> Option<Self::Item> {
        Some(loop {
            let line = self.lines.next()?;
            let line = line.unwrap();
            let line = line.trim();

            if line.is_empty() {
                continue;
            }
            if line.starts_with(";") || line.starts_with("//") {
                break SourceLine::Comment;
            }
            if line.starts_with(".") {
                let line = &line[1..];
                let i = line.find(' ').unwrap_or(line.len());
                let arg = &line[i+1..];
                match &line[..i] {
                    "string" => break SourceLine::DirString(arg.to_owned()),
                    "byte" => break SourceLine::DirByte(arg.parse().unwrap()),
                    "wide" | "word" => break SourceLine::DirWide(arg.parse().unwrap()),
                    s => panic!("unknown directive {s}"),
                }
            }
            if line.ends_with(":") {
                break SourceLine::Label((line[..line.len()-1]).to_owned())
            }
            if let Some(i) = line.find(' ') {
                let (ins, args) = line.split_at(i);
                let mut sos = Vec::new();

                for arg in args.split(',') {
                    let arg = arg.trim();

                    sos.push(if let Ok(n) = arg.parse() {
                        SourceOperand::Number(n)
                    } else {
                        match arg {
                            "z" | "0" => SourceOperand::Reg(Reg::Zero),
                            "al" => SourceOperand::Reg(Reg::Al),
                            "ah" => SourceOperand::Reg(Reg::Ah),
                            "bl" => SourceOperand::Reg(Reg::Bl),
                            "a" => SourceOperand::Reg(Reg::A),
                            "b" => SourceOperand::Reg(Reg::B),
                            "x" => SourceOperand::Reg(Reg::X),
                            "y" => SourceOperand::Reg(Reg::Y),
                            l => SourceOperand::Label(l.to_owned().into_boxed_str()),
                        }
                    });
                }

                break SourceLine::Ins(ins.to_owned(), sos);
            } else {
                break SourceLine::Ins(line.to_owned(), Vec::new());
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum DataLine {
    Ins(Opcode, DataOperand),
    Raw(Vec<u8>),
}

pub fn process(lines: impl Iterator<Item=SourceLine>) -> (HashMap<String, u16>, Vec<DataLine>) {
    let mut data_lines = Vec::new();
    let mut labels = HashMap::new();
    let mut cur_offset = 0;

    for line in lines {
        match line {
            SourceLine::Label(s) => {
                labels.insert(s, cur_offset);
            }
            SourceLine::Ins(s, ops) => {
                let (opcode, dat_op) = parse_ins(s, ops);
                cur_offset += 1 + dat_op.size();
                data_lines.push(DataLine::Ins(opcode, dat_op));
            }
            SourceLine::DirByte(b) => {
                cur_offset += 1;
                data_lines.push(DataLine::Raw(vec![b]));
            }
            SourceLine::DirWide(w) => {
                let [l, h] = w.to_le_bytes();
                cur_offset += 2;
                data_lines.push(DataLine::Raw(vec![l, h]));
            }
            SourceLine::DirString(s) => {
                let b = s.into_bytes();
                cur_offset += b.len() as u16;
                data_lines.push(DataLine::Raw(b));
            }
            SourceLine::Comment => (),
        }
    }

    (labels, data_lines)
}

fn parse_ins(s: String, ops: Vec<SourceOperand>) -> (u8, DataOperand) {
    use self::isa::*;
    use self::DataOperand as O;
    match &*s {
        "null" => (NULL, O::parse_nothing(ops).expect("nothing")),
        "write" => (WRITE, O::parse_big_r(ops).expect("big R")),
        "halt" => (HALT, O::parse_nothing(ops).expect("nothing")),
        "read" => (READ, O::parse_register(ops).expect("one register")),
        "nop" => (NOP, O::parse_nothing(ops).expect("nothing")),
        "push" => (PUSH, O::parse_big_r(ops).expect("big R")),
        "call" => (CALL, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jmp" | "jump" => (JUMP, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "ret" => (RET, O::parse_nothing(ops.clone()).map(|_| DataOperand::ImmediateByte(0)).or_else(|| O::parse_immediate_u8(ops)).expect("either nothing or a byte")),
        "pop" => (POP, O::parse_register(ops).expect("one register")),
        "ldstk" => (LDSTK, O::parse_rh(ops).expect("one register and one half number")),
        "store" => (STORE, O::parse_two_registers_and_big_r(ops).expect("two registers and big r")),
        "ststk" => (STSTK, O::parse_hr(ops).expect("one half number and one register")),
        "load" => (LOAD, O::parse_three_registers(ops).expect("three registers")),

        "jez" => (JEZ, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jlt" => (JLT, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jle" => (JLE, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jgt" => (JGT, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jge" => (JGE, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jnz" => (JNZ, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jo" => (JO, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jno" => (JNO, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jb" | "jc" => (JB, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jae" | "jnc" => (JAE, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "ja" => (JA, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),
        "jbe" => (JBE, O::parse_immediate_u16(ops).expect("a wide (addr like a label or just a number)")),

        "add" => (ADD, O::parse_two_registers_and_big_r(ops).expect("two regs and big r")),
        "sub" => (SUB, O::parse_two_registers_and_big_r(ops).expect("two regs and big r")),
        "and" => (AND, O::parse_two_registers_and_big_r(ops).expect("two regs and big r")),
        "or" => (OR, O::parse_two_registers_and_big_r(ops).expect("two regs and big r")),
        "xor" => (XOR, O::parse_two_registers_and_big_r(ops).expect("two regs and big r")),
        "mul" => {
            if let Some(dat_op) = O::parse_four_registers(ops.clone()) {
                (MUL, dat_op)
            } else if let Some(dat_op) = O::parse_b_registers(ops) {
                (BMUL, dat_op)
            } else {
                panic!("either four registers or (bmul) a register followed by two byte registers")
            }
        }
        "bmul" => (BMUL, O::parse_b_registers(ops).expect("three regs (reg, byte, byte")),
        "div" => (DIV, O::parse_four_registers(ops).expect("four regs")),
        _ => panic!("unknown instruction {s}"),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Wide {
    Number(u16),
    Label(Box<str>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BigR {
    Register(Reg),
    Byte(u8),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataOperand {
    Nothing,
    BigR(BigR),
    Register(Reg),
    TwoRegistersAndBigR(Reg, Reg, BigR),
    ImmediateByte(u8),
    ImmediateWide(Wide),
    Hr(u16, Reg),
    Rh(Reg, u16),
    FourRegisters(Reg, Reg, Reg, Reg),
    ThreeRegisters(Reg, Reg, Reg),
    BRegisters(Reg, Reg, Reg),
}

impl DataOperand {
    fn size(&self) -> u16 {
        match self {
            DataOperand::Nothing => 0,
            DataOperand::BigR(_) => 1,
            DataOperand::Register(_) => 1,
            DataOperand::TwoRegistersAndBigR(_, _, _) => 2,
            DataOperand::ImmediateByte(_) => 1,
            DataOperand::ImmediateWide(_) => 2,
            DataOperand::Hr(_, _) => 2,
            DataOperand::Rh(_, _) => 2,
            DataOperand::FourRegisters(_, _, _, _) => 2,
            DataOperand::ThreeRegisters(_, _, _) => 2,
            DataOperand::BRegisters(_, _, _) => 1,
        }
    }
    fn parse_nothing(ops: Vec<SourceOperand>) -> Option<DataOperand> {
        if ops.is_empty() {
            Some(DataOperand::Nothing)
        } else { None }
    }
    fn parse_register(ops: Vec<SourceOperand>) -> Option<DataOperand> {
        match &*ops {
            &[SourceOperand::Reg(r)] => Some(DataOperand::Register(r)),
            _ => None
        }
    }
    fn parse_big_r(mut ops: Vec<SourceOperand>) -> Option<DataOperand> {
        if ops.len() != 1 { return None }
        match ops.pop().unwrap() {
            SourceOperand::Label(_) => None,
            SourceOperand::Number(n) => Some(DataOperand::BigR(BigR::Byte(n as u8))),
            SourceOperand::Reg(r) => Some(DataOperand::BigR(BigR::Register(r))),
        }
    }
    fn parse_two_registers_and_big_r(mut ops: Vec<SourceOperand>) -> Option<DataOperand> {
        let mut regs = ops.drain(..2);
        let reg1 = regs.next()?;
        let reg2 = regs.next()?;
        drop(regs);
        match (reg1, reg2, Self::parse_big_r(ops)?) {
            (SourceOperand::Reg(r1), SourceOperand::Reg(r2), DataOperand::BigR(br)) => Some(DataOperand::TwoRegistersAndBigR(r1, r2, br)),
            _ => None,
        }
    }
    fn parse_immediate_u8(mut ops: Vec<SourceOperand>) -> Option<DataOperand> {
        if ops.len() != 1 { return None }
        match ops.pop().unwrap() {
            SourceOperand::Number(n) => Some(DataOperand::ImmediateByte(n as u8)),
            _ => None,
        }
    }
    fn parse_immediate_u16(mut ops: Vec<SourceOperand>) -> Option<DataOperand> {
        if ops.len() != 1 { return None }
        Some(DataOperand::ImmediateWide(match ops.pop().unwrap() {
            SourceOperand::Number(n) => Wide::Number(n as u16),
            SourceOperand::Label(lbl) => Wide::Label(lbl),
            _ => return None,
        }))
    }
    fn parse_hr(ops: Vec<SourceOperand>) -> Option<DataOperand> {
        match &*ops {
            &[SourceOperand::Number(h), SourceOperand::Reg(r)]
                => Some(DataOperand::Hr(h as u16, r)),
            _ => None
        }
    }
    fn parse_rh(ops: Vec<SourceOperand>) -> Option<DataOperand> {
        match &*ops {
            &[SourceOperand::Reg(r), SourceOperand::Number(h)]
                => Some(DataOperand::Rh(r, h as u16)),
            _ => None
        }
    }
    fn parse_four_registers(ops: Vec<SourceOperand>) -> Option<DataOperand> {
        match &*ops {
            &[SourceOperand::Reg(r1), SourceOperand::Reg(r2), SourceOperand::Reg(r3), SourceOperand::Reg(r4)]
                => Some(DataOperand::FourRegisters(r1, r2, r3, r4)),
            _ => None
        }
    }
    fn parse_three_registers(ops: Vec<SourceOperand>) -> Option<DataOperand> {
        match &*ops {
            &[SourceOperand::Reg(r1), SourceOperand::Reg(r2), SourceOperand::Reg(r3)]
                => Some(DataOperand::ThreeRegisters(r1, r2, r3)),
            _ => None
        }
    }
    fn parse_b_registers(ops: Vec<SourceOperand>) -> Option<DataOperand> {
        match &*ops {
            &[SourceOperand::Reg(r1), SourceOperand::Reg(r2), SourceOperand::Reg(r3)]
                => Some(DataOperand::BRegisters(r1, r2, r3)),
            _ => None
        }
    }
}