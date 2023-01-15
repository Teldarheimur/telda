use std::{fmt::{self, Display, Write}, convert::identity};

use crate::{
    U4, mem::{MainMemory, MAIN_IO_MAPPING_CUTOFF}, blf4::{Blf4, ByteRegister, WideRegister, HandlerContext, isa::{arg_pair, arg_imm_wide}, R0, TrapMode},
};

struct StrictMemory<'a, M: MainMemory> {
    inner: &'a mut M,
}

impl<M: MainMemory> MainMemory for StrictMemory<'_, M> {
    fn read(&mut self, addr: u32) -> u8 {
        if addr < MAIN_IO_MAPPING_CUTOFF {
            self.inner.read(addr)
        } else {
            unimplemented!("no I/O for strict memory")
        }
    }
    fn write(&mut self, _addr: u32, _val: u8) {
        unimplemented!("no writing to strict memory")
    }
}

pub struct DisassembledInstruction {
    pub annotated_source: String,
    pub ends_block: bool,
    pub nesting_difference: i32,
    pub next_instruction_location: u16,
}

pub fn disassemble_instruction<'a, M: MainMemory, F: FnOnce(u16) -> Option<&'a str>>(
    location: u16,
    memory: &mut M,
    label_lookup: F,
) -> Result<DisassembledInstruction, TrapMode> {
    use crate::blf4::isa::*;
    let r = &mut Blf4::new(location);
    let m = &mut StrictMemory { inner: memory };
    let mut c = r.context(m);

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
            let (r1, _r2) = arg_pair(&mut c,ByteRegister, identity)?;
            write!(f, "pop {r1}").unwrap();
        }
        POP_W => {
            let (r1, _r2) = arg_pair(&mut c, WideRegister, identity)?;
            write!(f, "pop {r1}").unwrap();
        }
        CALL => {
            let w = Operand::Wide(arg_imm_wide(&mut c)?).looked_up(label_lookup);
            write!(f, "call {w}").unwrap();
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
        JEZ => cjmp("jez", &mut c, label_lookup, f)?,
        JLT => cjmp("jlt", &mut c, label_lookup, f)?,
        JLE => cjmp("jle", &mut c, label_lookup, f)?,
        JGT => cjmp("jgt", &mut c, label_lookup, f)?,
        JGE => cjmp("jge", &mut c, label_lookup, f)?,
        JNZ => cjmp("jnz", &mut c, label_lookup, f)?,
        JO => cjmp("jo", &mut c, label_lookup, f)?,
        JNO => cjmp("jno", &mut c, label_lookup, f)?,
        JB => cjmp("jb", &mut c, label_lookup, f)?,
        JAE => cjmp("jae", &mut c, label_lookup, f)?,
        JA => cjmp("ja", &mut c, label_lookup, f)?,
        JBE => cjmp("jbe", &mut c, label_lookup, f)?,
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
        DIV_B => binop("div", ByteRegister, &mut c, f)?,
        DIV_W => binop("div", WideRegister, &mut c, f)?,
        MUL_B => binop("mul", ByteRegister, &mut c, f)?,
        MUL_W => binop("mul", WideRegister, &mut c, f)?,
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

fn cjmp<'a, F: FnOnce(u16) -> Option<&'a str>>(
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

fn binop<T: Display, RF: Fn(U4) -> T>(
    name: &str,
    rf: RF,
    c: &mut HandlerContext,
    f: &mut String,
) -> Result<(), TrapMode> {
    let (r1, r2) = arg_pair(c, &rf, &rf)?;
    let (r3, _o) = arg_pair(c, &rf, identity)?;
    write!(f, "{name} {r1}, {r2}, {r3}").unwrap();

    Ok(())
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
        if let Operand::Wide(w) = *self {
            if let Some(lbl) = label_lookup(w) {
                *self = Operand::Label(lbl);
            }
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
