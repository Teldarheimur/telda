use super::{Machine, Memory, Memory16Bit, Cpu, Signal};
use crate::is::*;
use crate::wrappers::SplittableWord;
use std::io::{Write, Read};
use std::fmt::Debug;
use std::borrow::{Borrow, BorrowMut};

pub const VERSION: u8 = 18;

pub type StandardMachine = Machine<u16, Memory16Bit, StandardCpu>;

#[derive(Debug)]
pub struct StandardCpu {
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

pub type CpuAndMemory<'a, M> = (&'a mut StandardCpu, &'a mut M);

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

impl<M: Memory<<StandardCpu as Cpu>::Index>> InstructionHandler for CpuAndMemory<'_, M> {
    type Fst = Reg;
    type Snd = SecArgs;
    type InterruptSignal = Signal;

    #[inline(always)]
    fn convert_fst(&self, fst: FstArg) -> Self::Fst {
        fst
    }
    fn convert_snd(&self, snd: SndArg) -> Self::Snd {
        let (cpu, memory) = self;

        match snd {
            FullArg::Reg(r) => match cpu.reg(r) {
                Ok(w) => SecArgs::Wide(w),
                Err(b) => SecArgs::Byte(b),
            },
            FullArg::RegRef(is_wide, reg, offset) => {
                let addr = ((cpu.reg(reg).unwrap_or_else(|b| b as u16) as i16).wrapping_add(offset as i16)) as u16;

                if is_wide {
                    SecArgs::Wide(memory.read_index(addr))
                } else {
                    SecArgs::Byte(memory.read(addr))
                }
            }
            FullArg::ImmRef(is_wide, addr) => {
                if is_wide {
                    SecArgs::Wide(memory.read_index(addr))
                } else {
                    SecArgs::Byte(memory.read(addr))
                }
            }
            FullArg::Byte(b) => SecArgs::Byte(b),
            FullArg::Wide(w) => SecArgs::Wide(w),
        }
    }

    fn load(&mut self, reg: Self::Fst, val: Self::Snd) {
        let res = match self.0.reg_mut(reg) {
            Err(b) => {
                *b = val.u8();
                *b as i8 as i16 as u16
            }
            Ok(w) => {
                *w = val.u16();
                *w
            }
        };
        self.0.set_status_flags(res, false, false);
    }
    fn str(&mut self, reg: Self::Fst, location: Self::Snd){
        let (cpu, memory) = self;

        let location = location.u16();

        match cpu.reg(reg) {
            Ok(wide) => memory.write_index(location, wide),
            Err(acc) => memory.write(location, acc),
        }
    }
    #[inline]
    fn cmp(&mut self, reg: Self::Fst, val: Self::Snd) {
        // HACK
        let stash_reg = self.0.reg(reg);
        self.sub(reg, val);
        match self.0.reg_mut(reg) {
            Ok(r) => *r = stash_reg.unwrap(),
            Err(r) => *r = stash_reg.unwrap_err(),
        }
    }

    #[inline(always)]
    fn and(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_no_overflow(reg, val, u8::bitand, u16::bitand)
    }
    #[inline(always)]
    fn or(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_no_overflow(reg, val, u8::bitor, u16::bitor)
    }
    #[inline(always)]
    fn xor(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_no_overflow(reg, val, u8::bitxor, u16::bitxor)
    }
    #[inline(always)]
    fn not(&mut self, reg: Self::Fst) {
        let res = match self.0.reg_mut(reg) {
            Ok(reg) => {
                *reg = !*reg;
                *reg
            }
            Err(reg) => {
                *reg = !*reg;
                *reg as i8 as i16 as u16
            }
        };
        self.0.set_status_flags(res, false, false);
    }
    #[inline(always)]
    fn neg(&mut self, reg: Self::Fst) {
        let carry = self.0
            .reg(reg)
            .map(|x| x == 0)
            .unwrap_or_else(|x| x == 0);

        let (result, overflow) = match self.0.reg_mut(reg) {
            Ok(reg) => {
                let (res, of) = (*reg as i16).overflowing_neg();
                *reg = res as u16;
                (*reg, of)
            }
            Err(reg) => {
                let (res, of) = (*reg as i8).overflowing_neg();
                *reg = res as u8;
                (res as i16 as u16, of)
            }
        };
        self.0.set_status_flags(result, overflow, carry);
    }
    
    #[inline(always)]
    fn inc(&mut self, reg: Self::Fst) {
        self.add(reg, SecArgs::from_reg_with(reg, 1))
    }
    #[inline(always)]
    fn dec(&mut self, reg: Self::Fst) {
        self.sub(reg, SecArgs::from_reg_with(reg, 1))
    }
    fn sub(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_sub, u16::overflowing_sub, i8::overflowing_sub, i16::overflowing_sub)
    }
    #[inline(always)]
    fn add(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_add, u16::overflowing_add, i8::overflowing_add, i16::overflowing_add)
    }
    #[inline(always)]
    fn mul(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_mul, u16::overflowing_mul, i8::overflowing_mul, i16::overflowing_mul)
    }
    #[inline(always)]
    fn div(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_div, u16::overflowing_div, i8::overflowing_div, i16::overflowing_div)
    }
    #[inline(always)]
    fn rem(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_rem, u16::overflowing_rem, i8::overflowing_rem, i16::overflowing_rem)
    }

    fn push(&mut self, val: Self::Snd) {
        match val {
            SecArgs::Byte(b) => {
                self.0.stack_pointer -= 1;
                self.1.write(self.0.stack_pointer, b);
            }
            SecArgs::Wide(w) => {
                self.0.stack_pointer -= 2;
                self.1.write_index(self.0.stack_pointer, w);
            }
        }
    }
    fn pop(&mut self, reg: Self::Fst) {
        let sp = self.0.stack_pointer;

        let width = match self.0.reg_mut(reg) {
            Ok(reg) => {
                *reg = self.1.read_index(sp);
                2
            }
            Err(reg) => {
                *reg = self.1.read(sp);
                1
            }
        };
        self.0.stack_pointer += width;
    }
    fn store_at_stack_offset(&mut self, reg: Self::Fst, val: Self::Snd) {
        let (cpu, memory) = self;

        let offset = match val {
            // sign extend bytes
            SecArgs::Byte(b) => b as i8 as i16,
            SecArgs::Wide(w) => w as i16,
        };

        let location = (cpu.stack_pointer as i16).wrapping_add(offset) as u16;

        match cpu.reg(reg) {
            Ok(wide) => memory.write_index(location, wide),
            Err(acc) => memory.write(location, acc),
        }
    }
    #[inline]
    fn setd(&mut self, direction: bool) {
        self.0.set_flag(flags::DIR, direction);
    }
    fn sload(&mut self, reg: Self::Fst) {
        let (cpu, memory) = self;

        let source_location = cpu.source_pointer;

        let res;

        let inc = match cpu.reg_mut(reg) {
            Err(b) => {
                *b = memory.read(source_location);
                res = *b as i8 as i16 as u16;
                1
            }
            Ok(w) => {
                *w = memory.read_index(source_location);
                res = *w;
                2
            }
        };
        if cpu.flags & flags::DIR != 0 {
            cpu.source_pointer += inc;
        } else {
            cpu.source_pointer -= inc;
        }
        self.0.set_status_flags(res, false, false);
    }

    fn sstore(&mut self, val: Self::Snd) {
        let inc  = match val {
            SecArgs::Byte(b) => {
                self.1.write(self.0.destination_pointer, b);
                1
            }
            SecArgs::Wide(w) => {
                self.1.write_index(self.0.destination_pointer, w);
                2
            }
        };
        if self.0.flags & flags::DIR != 0 {
            self.0.destination_pointer += inc;
        } else {
            self.0.destination_pointer -= inc;
        }
    }
    fn smv(&mut self) {
        let incrementing = self.0.flags & flags::DIR != 0;
        let b = self.1.read(self.0.source_pointer);
        self.1.write(self.0.destination_pointer, b);
        if incrementing {
            self.0.source_pointer += 1;
            self.0.destination_pointer += 1;
        } else {
            self.0.source_pointer -= 1;
            self.0.destination_pointer -= 1;
        }
        self.0.set_status_flags(b as i8 as i16 as u16, false, false);
    }
    #[inline]
    fn call(&mut self, location: Self::Snd) {
        #[cfg(feature = "print_instruction")]
        {
            self.0.indent += 1;
        }

        self.push(SecArgs::Wide(self.0.pc));

        self.0.pc = location.u16();
    }
    #[inline(always)]
    fn ret(&mut self) {
        #[cfg(feature = "print_instruction")]
        {
            self.0.indent -= 1;
        }

        self.0.pc = self.1.read_index(self.0.stack_pointer);
        
        self.0.stack_pointer += 2;
    }
    #[inline(always)]
    fn ret_add_sp(&mut self, bytes_to_deallocate: Self::Snd) {
        self.ret();

        self.0.stack_pointer += match bytes_to_deallocate {
            SecArgs::Byte(b) => b as u16,
            SecArgs::Wide(w) => w,
        };
    }

    #[inline(always)]
    fn jump(&mut self, location: Self::Snd) {
        self.0.pc = location.u16();
    }

    fn jof(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jno(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jsb(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::SB) {
            self.jump(location);
        }
    }
    fn jns(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::SB) {
            self.jump(location);
        }
    }
    fn jez(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::EZ) {
            self.jump(location);
        }
    }
    fn jne(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::EZ) {
            self.jump(location);
        }
    }
    fn jca(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::CA) {
            self.jump(location);
        }
    }
    fn jnc(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::CA) {
            self.jump(location);
        }
    }
    fn jcz(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::CA | flags::EZ) {
            self.jump(location);
        }
    }
    fn jncz(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::CA | flags::EZ) {
            self.jump(location);
        }
    }

    fn jlt(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::SB) != self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jge(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::SB) == self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jle(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::EZ) || (self.0.get_flag(flags::SB) != self.0.get_flag(flags::OF)) {
            self.jump(location);
        }
    }
    fn jgt(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::EZ) && (self.0.get_flag(flags::SB) == self.0.get_flag(flags::OF)) {
            self.jump(location);
        }
    }

    fn int0(&mut self) -> Option<Self::InterruptSignal> {
        Some(Signal::PowerOff)
    }
    fn int1(&mut self) -> Option<Self::InterruptSignal> {
        let mut bytes = [0];
        let read = std::io::stdin().read(&mut bytes).unwrap();
        if read == 0 {
            *self.0.accumulator.borrow_mut() = 0x04u8;
        } else {
            *self.0.accumulator.borrow_mut() = bytes[0];
        }
        None
    }
    fn int2(&mut self) -> Option<Self::InterruptSignal> {
        std::io::stdout().write_all(&[*self.0.accumulator.borrow()]).unwrap();
        None
    }
    fn int3(&mut self) -> Option<Self::InterruptSignal> {
        std::io::stderr().write_all(&[*self.0.accumulator.borrow()]).unwrap();
        None
    }
    #[inline(always)]
    fn int4(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int5(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int6(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int7(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int8(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int9(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int10(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int11(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int12(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int13(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int14(&mut self) -> Option<Self::InterruptSignal> {
        let sr = self.1.read(self.0.source_pointer);
        let ds = self.1.read(self.0.destination_pointer);
        eprintln!("[$sr]: {:02x} {:?} {0:3}   [$ds]: {:02x} {:?} {2:3}",
            sr, sr as char, ds, ds as char);

        None
    }
    fn int15(&mut self) -> Option<Self::InterruptSignal> {
        eprintln!("{:#x?}", self.0);
        let mut stack = Vec::with_capacity((0xffff - self.0.stack_pointer) as usize);
        for i in self.0.stack_pointer..0xffff {
            stack.push(self.1.read(i));
        }
        eprintln!("Stack: {:x?}", stack);

        None
    }

    #[cfg(feature = "print_instruction")]
    fn print_instruction(&self, op_and_arg: OpAndArg) {
        for _ in 0..self.0.indent {
            eprint!("    ");
        }
        eprintln!("    {}", op_and_arg);
    }
}

type BinOp<T> = fn(T, T) -> (T, bool);

impl StandardCpu {
    pub fn new(start: u16) -> Self {
        StandardCpu {
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
    const fn get_flag(&self, flag: u8) -> bool {
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

impl Cpu for StandardCpu {
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
