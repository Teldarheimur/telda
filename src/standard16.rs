use super::{Machine, Memory, Memory16Bit, Cpu, Signal};
use crate::is::*;
use crate::wrappers::SplittableWord;
use std::io::{Write, Read};
use std::fmt::{self, Debug, Display};
use std::borrow::{Borrow, BorrowMut};

pub const VERSION: u8 = 18;

pub type StandardMachine<I, O> = Machine<u16, Memory16Bit, StandardCpu<I, O>>;

#[derive(Debug, Copy, Clone)]
pub enum SecArgs {
    Byte(u8),
    Wide(u16),
}

impl SecArgs {
    const fn from_reg_with(reg: Reg, byte: u8) -> Self {
        if reg.is_wide() {
            Self::Wide(byte as u16)
        } else {
            Self::Byte(byte)
        }
    }

    fn u8(self) -> u8 {
        use self::SecArgs::*;
        match self {
            Byte(b) => b,
            Wide(_) => panic!("Expected byte but had wide"),
        }
    }
    fn u16(self) -> u16 {
        use self::SecArgs::*;
        match self {
            Wide(w) => w,
            Byte(b) => {
                eprintln!("Implicitly extended a byte into a wide!!");
                b as u16
            }
        }
    }
}

pub type CpuAndMemory<'a, M, I, O> = (&'a mut StandardCpu<I, O>, &'a mut M);

pub mod flags {
    /// Overflow (if signed arithmetic gave the wrong result)
    pub const OF : u8 = 0b1000_0000;
    /// Sign bit
    pub const SB : u8 = 0b0100_0000;
    /// Carry (if unsigned arithmetic gave the wrong result)
    pub const CA : u8 = 0b0010_0000;
    /// Equals zero
    pub const EZ : u8 = 0b0001_0000;
    /// Direction of string operations (1 = incrementing, 0 = decrementing)
    pub const DIR: u8 = 0b0000_1000;
    pub const TRP: u8 = 0b0000_0100;
    pub const LF1: u8 = 0b0000_0010;
    pub const LF2: u8 = 0b0000_0001;

    pub const DEFAULT: u8 = DIR;
}

/// Represents something that can be read from
/// 
/// Can be blocking
pub trait InPort {
    /// This will be used by the read interrupt (int1)
    fn read_port(&mut self) -> u8;
}

impl<T: Read> InPort for T {
    fn read_port(&mut self) -> u8 {
        let mut buf = [0];
        let read = self.read(&mut buf).unwrap();
        if read == 0 {
            // ASCII End of Transmission
            0x04u8
        } else {
            let [b] = buf;
            b
        }
    }
}
/// Represents something that can be written to
pub trait OutPort {
    /// This will be used by the write interrupt (int2)
    fn write_port(&mut self, b: u8);
}
impl<T: Write> OutPort for T {
    fn write_port(&mut self, b: u8) {
        self.write_all(&[b]).unwrap();
    }
}

#[derive(Debug)]
pub struct StandardCpu<I: InPort, O: OutPort> {
    in_port: I,
    out_port: O,
    #[cfg(feature = "ops")]
    pub ops: u32,
    #[cfg(feature = "print_instruction")]
    pub indent: u32,
    pc: u16,
    stack_pointer: u16,
    base_pointer: u16,
    accumulator: SplittableWord,
    b_accum: SplittableWord,
    source_pointer: u16,
    destination_pointer: u16,
    flags: u8,
}

mod instruction_handler;

type BinOp<T> = fn(T, T) -> (T, bool);

impl<I: InPort, O: OutPort> StandardCpu<I, O> {
    pub fn new(start: u16, in_port: I, out_port: O) -> Self {
        StandardCpu {
            in_port,
            out_port,
            #[cfg(feature = "ops")]
            ops: 0,
            #[cfg(feature = "print_instruction")]
            indent: 0,
            pc: start,
            stack_pointer: 0xffff,
            base_pointer: 0xffff,
            source_pointer: 0,
            destination_pointer: 0,
            accumulator: 0.into(),
            b_accum: 0.into(),
            flags: flags::DEFAULT,
        }
    }
    #[inline(always)]
    pub fn display(&self) -> DisplayStandardCpu<I, O> {
        DisplayStandardCpu(self)
    }
    #[inline]
    fn reg(&self, reg: Reg) -> Result<u16, u8> {
        match reg {
            Reg::Sp => Ok(self.stack_pointer),
            Reg::Bp => Ok(self.base_pointer),
            Reg::Sr => Ok(self.source_pointer),
            Reg::Ds => Ok(self.destination_pointer),
            Reg::Ba => Ok(*self.b_accum.borrow()),
            Reg::Bb => Err(*self.b_accum.borrow()),
            Reg::Ac => Ok(*self.accumulator.borrow()),
            Reg::Ab => Err(*self.accumulator.borrow()),
        }
    }
    #[inline]
    fn reg_mut(&mut self, reg: Reg) -> Result<&mut u16, &mut u8> {
        match reg {
            Reg::Sp => Ok(&mut self.stack_pointer),
            Reg::Bp => Ok(&mut self.base_pointer),
            Reg::Sr => Ok(&mut self.source_pointer),
            Reg::Ds => Ok(&mut self.destination_pointer),
            Reg::Ba => Ok(self.b_accum.borrow_mut()),
            Reg::Bb => Err(self.b_accum.borrow_mut()),
            Reg::Ac => Ok(self.accumulator.borrow_mut()),
            Reg::Ab => Err(self.accumulator.borrow_mut()),
        }
    }
    #[inline]
    #[track_caller]
    fn reg_mut_val(&mut self, reg: Reg, val: SecArgs) -> Result<(&mut u16, u16), (&mut u8, u8)> {
        match self.reg_mut(reg) {
            Ok(wide) => Ok((wide, val.u16())),
            Err(acc) => Err((acc, val.u8())),
        }
    }

    fn set_status_flags(&mut self, result: u16, overflow: bool, carry: bool) {
        self.flags &= !(flags::OF | flags::SB | flags::CA | flags::EZ);
        if result == 0 {
            self.flags |= flags::EZ;
        } else if (result as i16).is_negative() {
            self.flags |= flags::SB;
        }
        if overflow {
            self.flags |= flags::OF;
        }
        if carry {
            self.flags |= flags::CA;
        }
    }
    fn set_flag(&mut self, flag: u8, set: bool) {
        if set {
            self.flags |= flag;
        } else {
            self.flags &= !flag;
        }
    }
    #[inline]
    fn get_flag(&self, flag: u8) -> bool {
        self.flags & flag != 0
    }

    #[inline]
    fn binop_overflowing(&mut self, reg: Reg, sec: SecArgs, op: BinOp<u8>, opw: BinOp<u16>, iop: BinOp<i8>, iopw: BinOp<i16>) {
        // Slightly hacky, but probably simplest
        let (res, c, o) = match self.reg_mut_val(reg, sec) {
            Ok((reg, val)) => {
                let (res, c) = opw(*reg, val);
                let (_ir, o) = iopw(*reg as i16, val as i16);

                *reg = res;
                (res, c, o)
            }
            Err((reg, val)) => {
                let (res, c) = op(*reg, val);
                let (_ir, o) = iop(*reg as i8, val as i8);

                *reg = res;
                (res as i8 as i16 as u16, c, o)
            }
        };
        self.set_status_flags(res, o, c);
    }
    #[inline]
    fn binop_no_overflow(&mut self, reg: Reg, sec: SecArgs, op: fn(u8, u8) -> u8, opw: fn(u16, u16) -> u16) {
        match self.reg_mut_val(reg, sec) {
            Ok((reg, val)) => *reg = opw(*reg, val),
            Err((reg, val)) => *reg = op(*reg, val),
        }

        self.set_status_flags(self.reg(reg).unwrap_or_else(|b| b as i8 as i16 as u16), false, false);
    }
}

use std::ops::{BitAnd, BitOr, BitXor};

impl<I: InPort, O: OutPort> Cpu for StandardCpu<I, O> {
    type Index = u16;

    fn run<'a, M: Memory<Self::Index>>(&'a mut self, memory: &'a mut M) -> Option<Signal> {
        let (op_and_arg, len) = read_instruction(memory.read_iter_from(self.pc));
        let len = len as u16;
        self.pc += len;
        #[cfg(feature = "ops")]
        {
            self.ops += 1;
        }

        handle(&mut (self, memory), op_and_arg)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DisplayStandardCpu<'a, I: InPort, O: OutPort>(&'a StandardCpu<I, O>);

impl<I: InPort, O: OutPort> Display for DisplayStandardCpu<'_, I, O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &StandardCpu {
            in_port: _,
            out_port: _,
            #[cfg(feature = "ops")]
            ops: _,
            #[cfg(feature = "print_instruction")]
            indent: _,

            pc,
            stack_pointer,
            base_pointer,
            accumulator,
            b_accum,
            source_pointer,
            destination_pointer,
            flags: _,
        } = self.0;
        let ac: u16 = *accumulator.borrow();
        let ba: u16 = *b_accum.borrow();

        let mut flags = Vec::new();

        if self.0.get_flag(flags::OF) {
            flags.push("OF");
        }
        if self.0.get_flag(flags::SB) {
            flags.push("SB");
        }
        if self.0.get_flag(flags::CA) {
            flags.push("CA");
        }
        if self.0.get_flag(flags::EZ) {
            flags.push("EZ");
        }
        if self.0.get_flag(flags::DIR) {
            flags.push("DIR");
        }
        if self.0.get_flag(flags::TRP) {
            flags.push("TRP");
        }
        if self.0.get_flag(flags::LF1) {
            flags.push("LF1");
        }
        if self.0.get_flag(flags::LF2) {
            flags.push("LF2");
        }

        let flags = flags.join(" | ");
        let flags = if flags.is_empty() { "0" } else { &flags };

        writeln!(f, " $sp = 0x{:04x} $bp = 0x{:04x} $pc = 0x{:04x}", stack_pointer, base_pointer, pc)?;
        writeln!(f, " $ac = 0x{:04x} $ba = 0x{:04x}", ac, ba)?;
        writeln!(f, " $sr = 0x{:04x} $ds = 0x{:04x}", source_pointer, destination_pointer)?;
        write!(f, "  flags = {}", flags)
    }
}
