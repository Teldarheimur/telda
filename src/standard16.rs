
use super::{Machine, Memory, Memory16Bit, Cpu, Signal};
use crate::is::*;
use std::io::{Write, Read};

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
enum SecArgs {
    Byte(u8),
    Wide(u16),
    Both(u8, u16),
}

impl SecArgs {
    fn u8(self) -> Result<u8, u16> {
        use self::SecArgs::*;
        match self {
            Byte(b) => Ok(b),
            Both(b, _) => Ok(b as u8),
            Wide(w) => Err(w),
        }
    }
    fn u16(self) -> Result<u16, u8> {
        use self::SecArgs::*;
        match self {
            Wide(w) | Both(_, w) => Ok(w),
            Byte(b) => Ok(b as u16),
        }
    }
}

type InterpretedArgs = (Option<Reg>, Option<SecArgs>);

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
    fn reg(&self, reg: Reg) -> Result<u16, u8> {
        match reg {
            Reg::Sp => Ok(self.stack_pointer),
            Reg::Bc => Ok(self.b_counter),
            Reg::AccW => Ok(self.accumulator),
            Reg::Acc => Err(self.acc_u8())
        }
    }
    fn reg_mut(&mut self, reg: Reg) -> Result<&mut u16, &mut u8> {
        match reg {
            Reg::Sp => Ok(&mut self.stack_pointer),
            Reg::Bc => Ok(&mut self.b_counter),
            Reg::AccW => Ok(&mut self.accumulator),
            Reg::Acc => Err(self.acc_u8_mut())
        }
    }
    fn interpret_args<M: Memory<<Self as Cpu>::Index>>(&self, args: Args, memory: &M) -> InterpretedArgs {
        (
            args.fst,
            args.snd.map(|s| match s {
                FullArg::Reg(r) => match self.reg(r) {
                    Ok(w) => SecArgs::Wide(w),
                    Err(b) => SecArgs::Byte(b),
                },
                FullArg::OffsetReg(r, o) => match self.reg(r) {
                    Ok(w) => SecArgs::Wide((w as i16 + o) as u16),
                    Err(b) => SecArgs::Byte((b as i8 + o as i8) as u8),
                },
                FullArg::Ref(Reference::Val(w)) => SecArgs::Both(memory.read(w), memory.read_index(w)),
                FullArg::Ref(Reference::Reg{reg, offset}) => {
                    let addr = (self.reg(reg).unwrap_or_else(|b| b as u16) as i16 + offset as i16) as u16;

                    SecArgs::Both(memory.read(addr), memory.read_index(addr))
                }
                FullArg::Byte(b) => SecArgs::Byte(b),
                FullArg::Wide(w) => SecArgs::Wide(w),
            })
        )
    }
    fn arg_val_mut(&mut self, arg: InterpretedArgs, def: u16) -> Result<(&mut u16, u16), (&mut u8, u8)> {
        match (arg.0).unwrap_or(Reg::AccW) {
            Reg::Acc => {
                let val = (arg.1).map(|ar| ar.u8().expect(":'(")).unwrap_or(def as u8);
                Err((self.acc_u8_mut(), val))
            }
            reg => {
                let val = (arg.1).map(|ar| ar.u16().expect(":(")).unwrap_or(def);
                Ok((self.reg_mut(reg).unwrap(), val))
            }
        }
    }
    fn arg_val_reg(&mut self, arg: InterpretedArgs, def: u16) -> Result<(u16, u16), (u8, u8)> {
        let reg = self.reg((arg.0).unwrap_or(Reg::AccW));

        match reg {
            Ok(reg) => Ok((reg, (arg.1).map(|ar| ar.u16().expect(":(")).unwrap_or(def))),
            Err(reg) => Err((reg, (arg.1).map(|ar| ar.u8().expect(":'(")).unwrap_or(def as u8))),
        }
    }
    fn arg_val16_reg(&mut self, arg: InterpretedArgs, def: u16) -> Result<(u16, u16), (u8, u16)> {
        let reg = self.reg((arg.0).unwrap_or(Reg::AccW));

        match reg {
            Ok(reg) => Ok((reg, (arg.1).map(|ar| ar.u16().expect(":(")).unwrap_or(def))),
            Err(reg) => Err((reg, (arg.1).map(|ar| ar.u16().expect(":(")).unwrap_or(def))),
        }
    }
    fn sarg_u16(&self, arg: InterpretedArgs) -> Result<u16, u8> {
        match arg {
            (None, None) => Ok(self.accumulator),
            (None, Some(ar)) => ar.u16(),
            (Some(_), _) => panic!("Invalid args"),
        }
    }
    fn sarg_u8(&self, arg: InterpretedArgs) -> Result<u8, u16> {
        match arg {
            (None, None) => Ok(self.acc_u8()),
            (None, Some(ar)) => ar.u8(),
            (Some(_), _) => panic!("Invalid args"),
        }
    }
    fn farg_mut(&mut self, arg: InterpretedArgs) -> Result<&mut u16, &mut u8> {
        match arg {
            (None, None) => Ok(&mut self.accumulator),
            (Some(r), None) => self.reg_mut(r),
            _ => panic!("Invalid args"),
        }
    }
    #[inline]
    fn sub(&mut self, arg: InterpretedArgs) {
        let (work, o) = match self.arg_val_mut(arg, 1) {
            Ok((reg, val)) => {
                let (val, o) = reg.overflowing_sub(val);
                *reg = val;
                (val, o) 
            }
            Err((reg, val)) => {
                let (val, o) = reg.overflowing_sub(val);
                *reg = val;
                (val as u16, o) 
            }
        };
        self.flags &= 0b1111_0000;
        self.flags |= if o {
            0b1100
        } else if work == 0 {
            0b0001
        } else {
            0b0010
        };
    }
    #[inline]
    fn binop_overflowing(&mut self, arg: InterpretedArgs, op: fn(u8, u8) -> (u8, bool), opw: fn(u16, u16) -> (u16, bool)) {
        let o = match self.arg_val_mut(arg, self.accumulator) {
            Ok((reg, val)) => {
                let (val, o) = opw(*reg, val);
                *reg = val;
                o
            }
            Err((reg, val)) => {
                let (val, o) = op(*reg, val);
                *reg = val;
                o
            }
        };

        self.flags &= 0b1111_0000;
        if o {
            self.flags |= 0b1000;
        }
    }
    #[inline]
    fn binop(&mut self, arg: InterpretedArgs, op: fn(u8, u8) -> u8, opw: fn(u16, u16) -> u16) {
        match self.arg_val_mut(arg, self.accumulator) {
            Ok((reg, val)) => *reg = opw(*reg, val),
            Err((reg, val)) => *reg = op(*reg, val),
        }

        self.flags &= 0b1111_0000;
    }
    #[inline]
    fn cmp(&mut self, arg: InterpretedArgs) {
        let res = match self.arg_val_reg(arg, 0) {
            Ok((reg, val)) => reg.cmp(&val),
            Err((reg, val)) => reg.cmp(&val),
        };

        use std::cmp::Ordering::*;

        self.flags &= 0b1111_0000;
        self.flags |= match res {
            Greater => 0b0100,
            Less => 0b0010,
            Equal => 0b0001,
        };
    }
    #[inline]
    fn jmp(&mut self, arg: InterpretedArgs, relative: bool) {
        let location = self.sarg_u16(arg).unwrap_or_else(|b| b as u16);

        self.pc = if relative {
            (self.pc as i16).wrapping_add(location as i16) as u16
        } else {
            location
        }
    }
}

use std::ops::{BitAnd, BitOr, BitXor};

impl Cpu for StandardCpu {
    type Index = u16;

    fn run<M: Memory<Self::Index>>(&mut self, memory: &mut M) -> Option<Signal> {
        let (opcode, args, len) = read_instruction(memory.read_iter_from(self.pc));
        let len = len as u16;
        self.pc += len;

        let cur_ins = opcode as u8;
        
        let arg = self.interpret_args(args, &*memory);

        match cur_ins {
            0b0100_0000..=0xff => unreachable!(),
            NOP => (),
            DEPLDL | DEPSTL => panic!("DEPRECATED"),
            RES6..=RESF => panic!("RESERVED"),
            INVALID => panic!("Invalid instruction call {:2x}!\n{:?}", cur_ins, self),
            LOAD => {
                match self.arg_val_mut(arg, 0) {
                    Ok((reg, val)) => *reg = val,
                    Err((reg, val)) => *reg = val,
                }
            }
            STR => {
                match self.arg_val16_reg(arg, 0) {
                    Ok((reg, val)) => memory.write_index(val, reg),
                    Err((reg, val)) => memory.write(val, reg),
                }
            }
            COMPARE => self.cmp(arg),
            SUB => self.sub(arg),
            ADD => if let None = arg.1 {
                match self.farg_mut(arg) {
                    Ok(reg) => *reg += 1,
                    Err(reg) => *reg += 1,
                }
            } else {
                self.binop_overflowing(arg, u8::overflowing_add, u16::overflowing_add);
            },
            MUL => self.binop_overflowing(arg, u8::overflowing_mul, u16::overflowing_mul),
            DIV => self.binop_overflowing(arg, u8::overflowing_div, u16::overflowing_div),
            REM => self.binop_overflowing(arg, u8::overflowing_rem, u16::overflowing_rem),
            AND => self.binop(arg, u8::bitand, u16::bitand),
            OR => self.binop(arg, u8::bitor, u16::bitor),
            XOR => self.binop(arg, u8::bitxor, u16::bitxor),
            NOT => match self.arg_val_mut(arg, 0) {
                Ok((reg, val)) => *reg = !val,
                Err((reg, val)) => *reg = !val,
            },
            JUMP | JMPR => self.jmp(arg, cur_ins & 0b1000 == 0b1000),
            JIO | JIOR => if self.flags & 0b1000 == 0b1000 {
                self.jmp(arg, cur_ins & 0b1000 == 0b1000)
            }
            JEZ..=JNE | JEZR..= JNER => {
                let mask = cur_ins & 0b0111;

                if self.flags & mask != 0 {
                    self.jmp(arg, cur_ins & 0b1000 == 0b1000);
                }
            }
            PUSH => {
                match self.sarg_u8(arg) {
                    Ok(b) => {
                        self.stack_pointer -= 1;
                        memory.write(self.stack_pointer, b);
                    }
                    Err(w) => {
                        self.stack_pointer -= 2;
                        memory.write_index(self.stack_pointer, w);
                    }
                }
            }
            PUSHW => {
                let val = self.sarg_u16(arg).expect("wide");
                self.stack_pointer -= 2;
                memory.write_index(self.stack_pointer, val);
            }
            POP => {
                let sp = self.stack_pointer;

                let width = match self.farg_mut(arg) {
                    Ok(reg) => {
                        *reg = memory.read_index(sp);
                        2
                    }
                    Err(reg) => {
                        *reg = memory.read(sp);
                        1
                    }
                };
                self.stack_pointer += width;
            }
            POPW => {
                let sp = self.stack_pointer;
                self.stack_pointer += 2;

                match self.farg_mut(arg) {
                    Ok(reg) => {
                        *reg = memory.read_index(sp);
                    },
                    Err(reg) => {
                        *reg = memory.read_index(sp) as u8;
                    }
                }
            }
            RET => {
                assert!(arg.0.is_none() && arg.1.is_none(), "ret takes no args");
                self.pc = memory.read_index(self.stack_pointer);
                self.stack_pointer += 2;
            }
            CALL => {
                let call_location = self.sarg_u16(arg).unwrap_or_else(|b| b as u16);

                self.stack_pointer -= 2;
                memory.write_index(self.stack_pointer, self.pc);

                self.pc = call_location;
            }

            INT1 => {
                let mut bytes = [0];
                std::io::stdin().read_exact(&mut bytes).unwrap();
                *self.acc_u8_mut() = bytes[0];
            }
            INT2 => {
                std::io::stdout().write_all(&[self.sarg_u8(arg).expect("byte")]).unwrap();
            }
            INT3 => {
                std::io::stderr().write_all(&[self.sarg_u8(arg).expect("byte")]).unwrap();
            }
            INT15 => {
                eprintln!("{:x?}", self);
                let mut stack = Vec::new();
                let mut i = self.stack_pointer;
                while i < 0xffff {
                    stack.push(memory.read(i));
                    i += 1;
                }
                eprintln!("Stack: {:x?}", stack);
            }
            i @ INT4 ..= INT14 => eprintln!("Interrupt{:X} called", i&0xf),
            HALT => return Some(Signal::PowerOff),
        }

        None
    }
}
