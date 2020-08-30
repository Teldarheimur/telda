use super::{Machine, Memory, Memory16Bit, Cpu, Signal};
use crate::is::*;
use std::io::{Write, Read};
use std::fmt::Debug;

pub const VERSION: u8 = 17;

pub type StandardMachine = Machine<u16, Memory16Bit, StandardCpu>;

#[derive(Debug)]
pub struct StandardCpu {
    pc: u16,
    stack_pointer: u16,
    accumulator: u16,
    b_counter: u16,
    flags: u8,
}

#[derive(Debug, Copy, Clone)]
pub enum SecArgs {
    Byte(u8),
    Wide(u16),
    Both(u8, u16),
}

impl SecArgs {
    fn u8(self) -> Result<u8, u16> {
        use self::SecArgs::*;
        match self {
            Byte(b) => Ok(b),
            Both(b, _) => Ok(b),
            Wide(w) => Err(w),
        }
    }
    fn u16(self) -> u16 {
        use self::SecArgs::*;
        match self {
            Wide(w) | Both(_, w) => w,
            Byte(b) => b as u16,
        }
    }
}

pub type CpuAndMemory<'a, M> = (&'a mut StandardCpu, &'a mut M);

pub mod flags {
    /// Overflow
    pub const OF: u8 = 0b1000;
    pub const GT: u8 = 0b0100;
    pub const LT: u8 = 0b0010;
    pub const EZ: u8 = 0b0001;
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
            FullArg::OffsetReg(r, o) => match cpu.reg(r) {
                Ok(w) => SecArgs::Wide((w as i16 + o) as u16),
                Err(b) => SecArgs::Byte((b as i8 + o as i8) as u8),
            },
            FullArg::Ref(Reference::Val(w)) => SecArgs::Both(memory.read(w), memory.read_index(w)),
            FullArg::Ref(Reference::Reg{reg, offset}) => {
                let addr = (cpu.reg(reg).unwrap_or_else(|b| b as u16) as i16 + offset) as u16;

                SecArgs::Both(memory.read(addr), memory.read_index(addr))
            }
            FullArg::Byte(b) => SecArgs::Byte(b),
            FullArg::Wide(w) => SecArgs::Wide(w),
        }
    }

    fn load(&mut self, reg: Self::Fst, val: Self::Snd) {
        match self.0.reg_mut_val(reg, val) {
            Ok((wide, val)) => *wide = val,
            Err((acc, val)) => *acc = val,
        }
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
    fn cmp(&mut self, reg: Self::Fst, rhs: Self::Snd) {
        let res = match self.0.reg_val(reg, rhs) {
            Ok((wide, rhs)) => wide.cmp(&rhs),
            Err((acc, rhs)) => acc.cmp(&rhs),
        };

        use std::cmp::Ordering::*;

        self.0.flags &= 0b1111_0000;
        self.0.flags |= match res {
            Greater => flags::GT,
            Less => flags::LT,
            Equal => flags::EZ,
        };
    }

    #[inline(always)]
    fn and(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop(reg, val, u8::bitand, u16::bitand)
    }
    #[inline(always)]
    fn or(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop(reg, val, u8::bitor, u16::bitor)
    }
    #[inline(always)]
    fn xor(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop(reg, val, u8::bitxor, u16::bitxor)
    }
    #[inline(always)]
    fn not(&mut self, reg: Self::Fst, val: Self::Snd) {
        match self.0.reg_mut_val(reg, val) {
            Ok((reg, val)) => *reg = !val,
            Err((reg, val)) => *reg = !val,
        }
    }
    
    #[inline(always)]
    fn inc(&mut self, reg: Self::Fst) {
        self.add(reg, SecArgs::Both(1, 1))
    }
    #[inline(always)]
    fn dec(&mut self, reg: Self::Fst) {
        self.sub(reg, SecArgs::Both(1, 1))
    }
    fn sub(&mut self, reg: Self::Fst, val: Self::Snd) {
        let (is_zero, o) = match self.0.reg_mut_val(reg, val) {
            Ok((reg, val)) => {
                let (val, o) = reg.overflowing_sub(val);
                *reg = val;
                (val == 0, o) 
            }
            Err((reg, val)) => {
                let (val, o) = reg.overflowing_sub(val);
                *reg = val;
                (val == 0, o) 
            }
        };
        self.0.flags &= 0b1111_0000;
        self.0.flags |= if o {
            flags::OF | flags::GT
        } else if is_zero {
            flags::EZ
        } else {
            flags::LT
        };
    }
    #[inline(always)]
    fn add(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_add, u16::overflowing_add)
    }
    #[inline(always)]
    fn mul(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_mul, u16::overflowing_mul)
    }
    #[inline(always)]
    fn div(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_div, u16::overflowing_div)
    }
    #[inline(always)]
    fn rem(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_rem, u16::overflowing_rem)
    }

    fn push(&mut self, val: Self::Snd) {
        match val.u8() {
            Ok(b) => {
                self.0.stack_pointer -= 1;
                self.1.write(self.0.stack_pointer, b);
            }
            Err(w) => {
                self.0.stack_pointer -= 2;
                self.1.write_index(self.0.stack_pointer, w);
            }
        }
    }
    fn pushw(&mut self, val: Self::Snd) {
        let val = val.u16();
        self.0.stack_pointer -= 2;
        self.1.write_index(self.0.stack_pointer, val);
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
    fn popw(&mut self, reg: Self::Fst) {
        let popped = self.1.read_index(self.0.stack_pointer);
        
        match self.0.reg_mut(reg) {
            Ok(reg) => {
                *reg = popped;
            },
            Err(reg) => {
                *reg = popped as u8;
            }
        }
        self.0.stack_pointer += 2;
    }
    #[inline]
    fn call(&mut self, location: Self::Snd) {
        self.pushw(SecArgs::Wide(self.0.pc));

        self.0.pc = location.u16();
    }
    #[inline(always)]
    fn ret(&mut self) {
        self.0.pc = self.1.read_index(self.0.stack_pointer);
        
        self.0.stack_pointer += 2;
    }

    #[inline(always)]
    fn jump(&mut self, location: Self::Snd) {
        self.0.pc = location.u16();
    }
    #[inline(always)]
    fn jumpr(&mut self, location: Self::Snd) {
        self.0.pc = (self.0.pc as i16).wrapping_add(location.u16() as i16) as u16
    }

    fn jez(&mut self, location: Self::Snd) {
        if self.0.flags & flags::EZ != 0 {
            self.jump(location);
        }
    }
    fn jezr(&mut self, location: Self::Snd) {
        if self.0.flags & flags::EZ != 0 {
            self.jumpr(location);
        }
    }
    fn jlt(&mut self, location: Self::Snd) {
        if self.0.flags & flags::LT != 0 {
            self.jump(location);
        }
    }
    fn jltr(&mut self, location: Self::Snd) {
        if self.0.flags & flags::LT != 0 {
            self.jumpr(location);
        }
    }
    fn jle(&mut self, location: Self::Snd) {
        if self.0.flags & (flags::LT | flags::EZ) != 0 {
            self.jump(location);
        }
    }
    fn jler(&mut self, location: Self::Snd) {
        if self.0.flags & (flags::LT | flags::EZ) != 0 {
            self.jumpr(location);
        }
    }
    fn jgt(&mut self, location: Self::Snd) {
        if self.0.flags & flags::GT != 0 {
            self.jump(location);
        }
    }
    fn jgtr(&mut self, location: Self::Snd) {
        if self.0.flags & flags::GT != 0 {
            self.jumpr(location);
        }
    }
    fn jge(&mut self, location: Self::Snd) {
        if self.0.flags & (flags::GT | flags::EZ) != 0 {
            self.jump(location);
        }
    }
    fn jger(&mut self, location: Self::Snd) {
        if self.0.flags & (flags::GT | flags::EZ) != 0 {
            self.jumpr(location);
        }
    }
    fn jne(&mut self, location: Self::Snd) {
        if self.0.flags & (flags::GT | flags::LT) != 0 {
            self.jump(location);
        }
    }
    fn jner(&mut self, location: Self::Snd) {
        if self.0.flags & (flags::GT | flags::LT) != 0 {
            self.jumpr(location);
        }
    }
    fn jio(&mut self, location: Self::Snd) {
        if self.0.flags & flags::OF != 0 {
            self.jump(location);
        }
    }
    fn jior(&mut self, location: Self::Snd) {
        if self.0.flags & flags::OF != 0 {
            self.jumpr(location);
        }
    }

    fn int0(&mut self) -> Option<Self::InterruptSignal> {
        Some(Signal::PowerOff)
    }
    fn int1(&mut self) -> Option<Self::InterruptSignal> {
        let mut bytes = [0];
        std::io::stdin().read_exact(&mut bytes).unwrap();
        *self.0.acc_u8_mut() = bytes[0];
        None
    }
    fn int2(&mut self) -> Option<Self::InterruptSignal> {
        std::io::stdout().write_all(&[self.0.acc_u8()]).unwrap();
        None
    }
    fn int3(&mut self) -> Option<Self::InterruptSignal> {
        std::io::stderr().write_all(&[self.0.acc_u8()]).unwrap();
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
    fn int14(&mut self) -> Option<Self::InterruptSignal> { None }
    fn int15(&mut self) -> Option<Self::InterruptSignal> {
        eprintln!("{:x?}", self.0);
        let mut stack = Vec::new();
        let mut i = self.0.stack_pointer;
        while i < 0xffff {
            stack.push(self.1.read(i));
            i += 1;
        }
        eprintln!("Stack: {:x?}", stack);

        None
    }
}

impl StandardCpu {
    pub fn new(start: u16) -> Self {
        StandardCpu {
            pc: start,
            stack_pointer: 0xffff,
            accumulator: 0,
            b_counter: 0,
            flags: 0,
        }
    }
    #[inline]
    fn acc_u8(&self) -> u8 {
        let b: &u8 = unsafe {
            std::mem::transmute(&self.accumulator)
        };
        *b
    }
    #[inline]
    fn acc_u8_mut(&mut self) -> &mut u8 {
        unsafe {
            std::mem::transmute(&mut self.accumulator)
        }
    }
    #[inline]
    fn reg(&self, reg: Reg) -> Result<u16, u8> {
        match reg {
            Reg::Sp => Ok(self.stack_pointer),
            Reg::Bc => Ok(self.b_counter),
            Reg::AccW => Ok(self.accumulator),
            Reg::Acc => Err(self.acc_u8())
        }
    }
    #[inline]
    fn reg_mut(&mut self, reg: Reg) -> Result<&mut u16, &mut u8> {
        match reg {
            Reg::Sp => Ok(&mut self.stack_pointer),
            Reg::Bc => Ok(&mut self.b_counter),
            Reg::AccW => Ok(&mut self.accumulator),
            Reg::Acc => Err(self.acc_u8_mut())
        }
    }
    #[inline]
    fn reg_val(&self, reg: Reg, val: SecArgs) -> Result<(u16, u16), (u8, u8)> {
        match self.reg(reg) {
            Ok(wide) => Ok((wide, val.u16())),
            Err(acc) => Err((acc, val.u8().expect(":("))),
        }
    }
    #[inline]
    #[track_caller]
    fn reg_mut_val(&mut self, reg: Reg, val: SecArgs) -> Result<(&mut u16, u16), (&mut u8, u8)> {
        match self.reg_mut(reg) {
            Ok(wide) => Ok((wide, val.u16())),
            Err(acc) => Err((acc, val.u8().expect(":("))),
        }
    }

    #[inline]
    fn binop_overflowing(&mut self, reg: Reg, sec: SecArgs, op: fn(u8, u8) -> (u8, bool), opw: fn(u16, u16) -> (u16, bool)) {
        let (o, zero) = match self.reg_mut_val(reg, sec) {
            Ok((reg, val)) => {
                let (val, o) = opw(*reg, val);
                *reg = val;
                (o, val == 0)
            }
            Err((reg, val)) => {
                let (val, o) = op(*reg, val);
                *reg = val;
                (o, val == 0)
            }
        };

        self.flags &= 0b1111_0000;
        if zero {
            self.flags |= flags::EZ;
        }
        if o {
            self.flags |= flags::OF;
        }
    }
    #[inline]
    fn binop(&mut self, reg: Reg, sec: SecArgs, op: fn(u8, u8) -> u8, opw: fn(u16, u16) -> u16) {
        match self.reg_mut_val(reg, sec) {
            Ok((reg, val)) => *reg = opw(*reg, val),
            Err((reg, val)) => *reg = op(*reg, val),
        }

        // TODO set zero flag?

        self.flags &= 0b1111_0000;
    }
}

use std::ops::{BitAnd, BitOr, BitXor};

impl Cpu for StandardCpu {
    type Index = u16;

    fn run<'a, M: Memory<Self::Index>>(&'a mut self, memory: &'a mut M) -> Option<Signal> {
        let (op_and_arg, len) = read_instruction(memory.read_iter_from(self.pc));
        let len = len as u16;
        self.pc += len;

        handle(&mut (self, memory), op_and_arg)
    }
}
