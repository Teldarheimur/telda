use std::{
    convert::identity,
    fmt::{self, Display, Write},
};

use crate::{
    blf4::{
        isa::{arg_imm_wide, arg_pair},
        Blf4, ByteRegister, HandlerContext, OpRes, TrapMode, WideRegister, R0,
    },
    machine::Machine,
    mem::MainMemory,
    PAGE_SIZE_P, U4,
};

struct StrictMemory<'a, M: MainMemory> {
    inner: &'a mut M,
}

impl<M: MainMemory> MainMemory for StrictMemory<'_, M> {
    fn read(&mut self, addr: u32) -> u8 {
        if addr < PAGE_SIZE_P {
            unimplemented!("no I/O for strict memory");
        }
        self.inner.read(addr)
    }
    fn write(&mut self, _addr: u32, _val: u8) {
        unimplemented!("no writing to strict memory")
    }
}

#[derive(Debug)]
pub struct DisassembledInstruction {
    pub annotated_source: String,
    pub ends_block: bool,
    pub nesting_difference: i32,
    pub next_instruction_location: u16,
}

pub fn disassemble_instruction<'a, M: MainMemory, F: FnOnce(u16) -> Option<&'a str>>(
    machine: &mut Machine<M, Blf4>,
    label_lookup: F,
) -> Result<DisassembledInstruction, TrapMode> {
    use crate::blf4::isa::*;
    let m = &mut StrictMemory {
        inner: &mut machine.memory,
    };
    let mut c = machine.cpu.context(m);

    let addr = c.cpu.program_counter;
    let opcode = c.fetch()?;

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
        SYSCALL => {
            write!(f, "syscall").unwrap();
        }
        RETH => {
            write!(f, "reth").unwrap();
            ends_block = true;
        }
        NOP => write!(f, "nop").unwrap(),
        PUSH_B => {
            let (r1, _) = arg_pair(&mut c, ByteRegister, identity)?;
            write!(f, "push {r1}").unwrap();
        }
        PUSH_W => {
            let (r1, _) = arg_pair(&mut c, WideRegister, identity)?;
            write!(f, "push {r1}").unwrap();
        }
        POP_B => {
            let (r1, _r2) = arg_pair(&mut c, ByteRegister, identity)?;
            write!(f, "pop {r1}").unwrap();
        }
        POP_W => {
            let (r1, _r2) = arg_pair(&mut c, WideRegister, identity)?;
            write!(f, "pop {r1}").unwrap();
        }
        ABS_CALL => {
            let w = Operand::Wide(arg_imm_wide(&mut c)?).looked_up(label_lookup);
            write!(f, "abscall {w}").unwrap();
            nesting_difference = 1;
        }
        RET => {
            let b = arg_imm_byte(&mut c)?;
            write!(f, "ret {b}").unwrap();
            nesting_difference = -1;
            ends_block = true;
        }
        STORE_BI => {
            let (r1, r2) = arg_pair(&mut c, WideRegister, ByteRegister)?;
            let offset = Operand::Wide(arg_imm_wide(&mut c)?).looked_up(label_lookup);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        STORE_WI => {
            let (r1, r2) = arg_pair(&mut c, WideRegister, WideRegister)?;
            let offset = Operand::Wide(arg_imm_wide(&mut c)?).looked_up(label_lookup);
            write!(f, "store {r1}, {offset}, {r2}").unwrap();
        }
        STORE_BR => {
            let (r1, r2) = arg_pair(&mut c, WideRegister, WideRegister)?;
            let (r3, _) = arg_pair(&mut c, ByteRegister, identity)?;
            write!(f, "store {r1}, {r2}, {r3}").unwrap();
        }
        STORE_WR => {
            let (r1, r2) = arg_pair(&mut c, WideRegister, WideRegister)?;
            let (r3, _) = arg_pair(&mut c, WideRegister, identity)?;
            write!(f, "store {r1}, {r2}, {r3}").unwrap();
        }
        LOAD_BI => {
            let (r1, r2) = arg_pair(&mut c, ByteRegister, WideRegister)?;
            let offset = Operand::Wide(arg_imm_wide(&mut c)?).looked_up(label_lookup);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        LOAD_WI => {
            let (r1, r2) = arg_pair(&mut c, WideRegister, WideRegister)?;
            let offset = Operand::Wide(arg_imm_wide(&mut c)?).looked_up(label_lookup);
            write!(f, "load {r1}, {r2}, {offset}").unwrap();
        }
        LOAD_BR => {
            let (r1, r2) = arg_pair(&mut c, ByteRegister, WideRegister)?;
            let (r3, _) = arg_pair(&mut c, WideRegister, identity)?;
            write!(f, "load {r1}, {r2}, {r3}").unwrap();
        }
        LOAD_WR => {
            let (r1, r2) = arg_pair(&mut c, WideRegister, WideRegister)?;
            let (r3, _) = arg_pair(&mut c, WideRegister, identity)?;
            write!(f, "load {r1}, {r2}, {r3}").unwrap();
        }
        ABS_JEZ => abs_cjmp("absjez", &mut c, label_lookup, f)?,
        ABS_JLT => abs_cjmp("absjlt", &mut c, label_lookup, f)?,
        ABS_JLE => abs_cjmp("absjle", &mut c, label_lookup, f)?,
        ABS_JGT => abs_cjmp("absjgt", &mut c, label_lookup, f)?,
        ABS_JGE => abs_cjmp("absjge", &mut c, label_lookup, f)?,
        ABS_JNZ => abs_cjmp("absjnz", &mut c, label_lookup, f)?,
        ABS_JO => abs_cjmp("absjo", &mut c, label_lookup, f)?,
        ABS_JNO => abs_cjmp("absjno", &mut c, label_lookup, f)?,
        ABS_JB => abs_cjmp("absjb", &mut c, label_lookup, f)?,
        ABS_JAE => abs_cjmp("absjae", &mut c, label_lookup, f)?,
        ABS_JA => abs_cjmp("absja", &mut c, label_lookup, f)?,
        ABS_JBE => abs_cjmp("absjbe", &mut c, label_lookup, f)?,
        LDI_B => {
            let (r1, _o) = arg_pair(&mut c, ByteRegister, identity)?;
            let b = arg_imm_byte(&mut c)?;

            write!(f, "ldi {r1}, {}", Operand::Byte(b)).unwrap();
        }
        LDI_W => {
            let (r1, o) = arg_pair(&mut c, WideRegister, u8::from)?;
            let w = Operand::Wide(arg_imm_wide(&mut c)?).looked_up(label_lookup);

            match o {
                // ldi
                0 => write!(f, "ldi {r1}, {w}").unwrap(),
                // jmp, jump
                1 => {
                    if r1 == R0 {
                        // jmp imm
                        write!(f, "absjmp {w}").unwrap();
                        ends_block = true;
                    } else {
                        // jmp r
                        write!(f, "absjmp {r1}").unwrap();
                        ends_block = true;
                    }
                }
                n => write!(f, "invalid ldi{n}, {r1}, {w}").unwrap(),
            }
        }
        ADD_B => binop("add", ByteRegister, &mut c, f)?,
        ADD_W => binop("add", WideRegister, &mut c, f)?,
        SUB_B => binop("sub", ByteRegister, &mut c, f)?,
        SUB_W => binop("sub", WideRegister, &mut c, f)?,
        AND_B => binop("and", ByteRegister, &mut c, f)?,
        AND_W => binop("and", WideRegister, &mut c, f)?,
        OR_B => binop("or", ByteRegister, &mut c, f)?,
        OR_W => binop("or", WideRegister, &mut c, f)?,
        XOR_B => binop("xor", ByteRegister, &mut c, f)?,
        XOR_W => binop("xor", WideRegister, &mut c, f)?,
        SHL_B => binop("shl", ByteRegister, &mut c, f)?,
        SHL_W => binop("shl", WideRegister, &mut c, f)?,
        ASR_B => binop("asr", ByteRegister, &mut c, f)?,
        ASR_W => binop("asr", WideRegister, &mut c, f)?,
        LSR_B => binop("lsr", ByteRegister, &mut c, f)?,
        LSR_W => binop("lsr", WideRegister, &mut c, f)?,
        DIV_B => binop2("div", ByteRegister, &mut c, f)?,
        DIV_W => binop2("div", WideRegister, &mut c, f)?,
        MUL_B => binop2("mul", ByteRegister, &mut c, f)?,
        MUL_W => binop2("mul", WideRegister, &mut c, f)?,
        ADC_B => binop2("adc", ByteRegister, &mut c, f)?,
        ADC_W => binop2("adc", WideRegister, &mut c, f)?,
        SBB_B => binop2("sbb", ByteRegister, &mut c, f)?,
        SBB_W => binop2("sbb", WideRegister, &mut c, f)?,
        R_CALL => {
            let w = Operand::RelWide(arg_imm_relative_wide(&mut c)?).looked_up(label_lookup);
            // todo, do relative lookup correctly
            write!(f, "rcall {w}").unwrap();
            nesting_difference = 1;
        }
        R_JUMP => {
            let w = Operand::RelWide(arg_imm_relative_wide(&mut c)?).looked_up(label_lookup);
            // todo, do relative lookup correctly
            write!(f, "rjmp {w}").unwrap();
            ends_block = true;
        }
        R_JEZ => r_cjmp("rjez", &mut c, label_lookup, f)?,
        R_JLT => r_cjmp("rjlt", &mut c, label_lookup, f)?,
        R_JLE => r_cjmp("rjle", &mut c, label_lookup, f)?,
        R_JGT => r_cjmp("rjgt", &mut c, label_lookup, f)?,
        R_JGE => r_cjmp("rjge", &mut c, label_lookup, f)?,
        R_JNZ => r_cjmp("rjnz", &mut c, label_lookup, f)?,
        R_JO => r_cjmp("rjo", &mut c, label_lookup, f)?,
        R_JNO => r_cjmp("rjno", &mut c, label_lookup, f)?,
        R_JA => r_cjmp("rja", &mut c, label_lookup, f)?,
        R_JAE => r_cjmp("rjae", &mut c, label_lookup, f)?,
        R_JB => r_cjmp("rjb", &mut c, label_lookup, f)?,
        R_JBE => r_cjmp("rjbe", &mut c, label_lookup, f)?,
        SET_IF => {
            let (r, o) = arg_pair(&mut c, ByteRegister, u8::from)?;
            write!(f, "set{} {r}", match o {
                0 => "x0",
                1 => "x1",
                2 => "ez",
                3 => "lt",
                4 => "le",
                5 => "gt",
                6 => "ge",
                7 => "nz",
                8 => "o",
                9 => "no",
                0xa => "a",
                0xb => "ae",
                0xc => "b",
                0xd => "be",
                0xe => "xe",
                0xf => "xf",
                0x10..=0xff => unreachable!(),
            }).unwrap();
        }
        b => {
            write!(f, "0x{b:02x}").unwrap();
            ends_block = true;
        }
    }

    let next_instruction_location = c.cpu.program_counter;

    let mut annotated_source = String::with_capacity(op.len() + 21);
    write!(&mut annotated_source, "  {addr:04x}: ").unwrap();

    c.cpu.program_counter = addr;
    for _ in addr..next_instruction_location {
        write!(&mut annotated_source, " {:02x}", c.fetch()?).unwrap();
    }
    // restore rpc
    c.cpu.program_counter = addr;

    for _ in 0..(4 - (next_instruction_location - addr)) {
        write!(&mut annotated_source, "   ").unwrap();
    }
    write!(&mut annotated_source, "    {op}").unwrap();

    Ok(DisassembledInstruction {
        annotated_source,
        next_instruction_location,
        ends_block,
        nesting_difference,
    })
}

fn arg_imm_relative_wide(c: &mut HandlerContext) -> OpRes<(u16, i16)> {
    let res = arg_imm_wide(c)? as i16;
    let pc = c.cpu.program_counter;
    Ok((pc, res))
}

fn abs_cjmp<'a, F: FnOnce(u16) -> Option<&'a str>>(
    name: &str,
    c: &mut HandlerContext,
    label_lookup: F,
    f: &mut String,
) -> Result<(), TrapMode> {
    write!(
        f,
        "{name} {}",
        Operand::Wide(arg_imm_wide(c)?).looked_up(label_lookup)
    )
    .unwrap();

    Ok(())
}
fn r_cjmp<'a, F: FnOnce(u16) -> Option<&'a str>>(
    name: &str,
    c: &mut HandlerContext,
    label_lookup: F,
    f: &mut String,
) -> Result<(), TrapMode> {
    write!(
        f,
        "{name} {}",
        Operand::RelWide(arg_imm_relative_wide(c)?).looked_up(label_lookup)
    )
    .unwrap();

    Ok(())
}

fn binop2<T: Display, RF: Fn(U4) -> T>(
    name: &str,
    rf: RF,
    c: &mut HandlerContext,
    f: &mut String,
) -> Result<(), TrapMode> {
    let (r1, r2) = arg_pair(c, &rf, &rf)?;
    let (r3, r4) = arg_pair(c, &rf, &rf)?;
    write!(f, "{name} {r1}, {r2}, {r3}, {r4}").unwrap();

    Ok(())
}
fn binop<T: Display, RF: Fn(U4) -> T>(
    name: &str,
    rf: RF,
    c: &mut HandlerContext,
    f: &mut String,
) -> Result<(), TrapMode> {
    let (r1, r2) = arg_pair(c, &rf, &rf)?;
    let (r3, o) = arg_pair(c, &rf, identity)?;
    write!(f, "{name} {r1}, {r2}, {r3}").unwrap();
    if o != U4::ZERO {
        write!(f, ", {}", u8::from(o)).unwrap();
    }

    Ok(())
}

enum Operand<'a> {
    Byte(u8),
    Wide(u16),
    Label(&'a str),
    RelativeWide(u16, i16),
    RelativeLabel(&'a str),
}

impl<'a> Operand<'a> {
    #[allow(non_snake_case)]
    pub fn RelWide((pc, off): (u16, i16)) -> Self {
        Self::RelativeWide(pc, off)
    }
    pub fn looked_up<F: FnOnce(u16) -> Option<&'a str>>(mut self, label_lookup: F) -> Self {
        self.convert_wide_to_label(label_lookup);
        self
    }
    pub fn convert_wide_to_label<F: FnOnce(u16) -> Option<&'a str>>(&mut self, label_lookup: F) {
        let (label_fn, loc): (fn(&'a str) -> Operand<'a>, _) = match *self {
            Operand::Wide(w) => (Operand::Label, w),
            Operand::RelativeWide(pc, w) => (Operand::RelativeLabel, pc.wrapping_add_signed(w)),
            _ => return,
        };
        if let Some(lbl) = label_lookup(loc) {
            *self = label_fn(lbl);
        }
    }
}

impl Display for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Byte(b) => write!(f, "0x{b:02x}"),
            Self::Wide(w) => write!(f, "0x{w:03x}"),
            Self::RelativeWide(_, w @ 0..=0x7fff) => write!(f, "+0x{w:03x}"),
            Self::RelativeWide(_, w) => write!(f, "-0x{:03x}", (w.wrapping_neg()) as u16),
            Self::Label(l) => l.fmt(f),
            Self::RelativeLabel(l) => write!(f, "±{l}"),
        }
    }
}
