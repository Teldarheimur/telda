
use super::{Machine, Memory, Memory16Bit, Cpu, Signal};
use crate::is::*;
use std::io::{Write, Read};

use byteorder::{NativeEndian, ByteOrder};

pub type StandardMachine = Machine<u16, Memory16Bit, StandardCpu>;

#[derive(Debug)]
pub struct StandardCpu {
    pc: u16,
    stack_pointer: u16,
    // base_pointer: u16,
    accumulator: u16,
    flags: u8,
}

impl StandardCpu {
    pub fn new(start: u16) -> Self {
        StandardCpu {
            pc: start,
            stack_pointer: 0xffff,
            accumulator: 0,
            flags: 0,
        }
    }
    #[inline]
    fn acc_u8(&mut self) -> &mut u8 {
        unsafe {
            std::mem::transmute(&mut self.accumulator)
        }
    }
    #[inline]
    fn sub(&mut self, a: Args) {
        let (work, o) = match a {
            Null => {
                let (acc, o) = self.accumulator.overflowing_sub(1);
                self.accumulator = acc;
                (acc, o)
            }
            Byte(b) => {
                let (acc, o) = (*self.acc_u8()).overflowing_sub(b);
                *self.acc_u8() = acc;
                (acc as u16, o)
            }
            Wide(c) => {
                let (acc, o) = self.accumulator.overflowing_sub(c);
                self.accumulator = acc;
                (acc, o)
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
    fn binop_overflowing(&mut self, a: Args, op: fn(u8, u8) -> (u8, bool), opw: fn(u16, u16) -> (u16, bool)) {
        let o = match a {
            Null => {
                let (acc, o) = opw(self.accumulator, self.accumulator);
                self.accumulator = acc;
                o
            }
            Byte(b) => {
                let (acc, o) = op(*self.acc_u8(), b);
                *self.acc_u8() = acc;
                o
            }
            Wide(c) => {
                let (acc, o) = opw(self.accumulator, c);
                self.accumulator = acc;
                o
            }
        };

        self.flags &= 0b1111_0000;
        if o {
            self.flags |= 0b1000;
        }
    }
    #[inline]
    fn binop(&mut self, a: Args, op: fn(u8, u8) -> u8, opw: fn(u16, u16) -> u16) {
        match a {
            Null => self.accumulator = opw(self.accumulator, self.accumulator),
            Byte(b) => *self.acc_u8() = op(*self.acc_u8(), b),
            Wide(c) => self.accumulator = opw(self.accumulator, c),
        }

        self.flags &= 0b1111_0000;
    }
    #[inline]
    fn cmp(&mut self, a: Args) {
        let res = match a {
            Null => (*self.acc_u8()).cmp(&0),
            Byte(b) => (*self.acc_u8()).cmp(&b),
            Wide(c) => self.accumulator.cmp(&c)
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
    fn jmp(&mut self, a: Args, relative: bool) {
        let location = a.as_u16().unwrap_or(self.accumulator);

        self.pc = if relative {
            (self.pc as i16).wrapping_add(location as i16) as u16
        } else {
            location
        };
    }
}

use std::ops::{BitAnd, BitOr, BitXor};

#[derive(Debug)]
pub enum Args {
    Null,
    Byte(u8),
    Wide(u16),
}

use self::Args::*;

impl Args {
    fn new(args: &[u8]) -> Args {
        match args.len() {
            0 => Null,
            1 => Byte(args[0]),
            2 => Wide(NativeEndian::read_u16(args)),
            3 => unimplemented!(),
            _ => unreachable!(),
        }
    }
    #[inline]
    fn as_u16(&self) -> Option<u16> {
        match *self {
            Null => None,
            Byte(b) => Some(b as u16),
            Wide(c) => Some(c),
        }
    }
    #[inline]
    fn as_u8(&self) -> Option<u8> {
        match *self {
            Null => None,
            Byte(b) => Some(b),
            Wide(c) => Some(c as u8),
        }
    }
}

impl Cpu for StandardCpu {
    type Index = u16;

    fn run<M: Memory<Self::Index>>(&mut self, memory: &mut M) -> Option<Signal> {
        let cur_ins = memory.read(self.pc);

        let args_length = (cur_ins >> 6) as u16;
        let args = Args::new(memory.read_to_slice(self.pc+1, args_length));

        self.pc += 1 + args_length;

        match cur_ins & 0b0011_1111 {
            0b0100_0000..=0xff => unreachable!(),
            NOP => (),
            RES6..=RESF => panic!("RESERVED"),
            INVALID => panic!("Invalid instruction call {:2x}!\n{:?}", cur_ins, self),
            LOAD => {
                match args {
                    Null => self.accumulator = 0,
                    Byte(b) => *self.acc_u8() = b,
                    Wide(c) => self.accumulator = c,
                }
            }
            LDL => {
                self.accumulator = memory.read_index(args.as_u16().unwrap_or_else(|| self.accumulator));
            }
            STR => {
                let location = args.as_u16().unwrap_or(0);
                match args {
                    Null | Byte(_) => memory.write(location, *self.acc_u8()),
                    Wide(_) => memory.write_index(location, self.accumulator), 
                }
            }
            STL => {
                let location = memory.read_index(args.as_u16().unwrap_or(0));
                match args {
                    Null | Byte(_) => memory.write(location, *self.acc_u8()),
                    Wide(_) => memory.write_index(location, self.accumulator), 
                }
            }
            COMPARE => self.cmp(args),
            SUB => self.sub(args),
            ADD => {
                if let Null = args {
                    self.accumulator += 1;
                } else {
                    self.binop_overflowing(args, u8::overflowing_add, u16::overflowing_add);
                }
            },
            MUL => self.binop_overflowing(args, u8::overflowing_mul, u16::overflowing_mul),
            DIV => self.binop_overflowing(args, u8::overflowing_div, u16::overflowing_div),
            REM => self.binop_overflowing(args, u8::overflowing_rem, u16::overflowing_rem),
            AND => self.binop(args, u8::bitand, u16::bitand),
            OR => self.binop(args, u8::bitor, u16::bitor),
            XOR => self.binop(args, u8::bitxor, u16::bitxor),
            NOT => match args {
                Null => self.accumulator = 0xffff,
                Byte(b) => *self.acc_u8() = !b,
                Wide(c) => self.accumulator = !c,
            },
            JUMP | JMPR => self.jmp(args, cur_ins & 0b1000 == 0b1000),
            JIO | JIOR => if self.flags & 0b1000 == 0b1000 {
                self.jmp(args, cur_ins & 0b1000 == 0b1000)
            }
            JEZ..=JNE | JEZR..= JNER => {
                let mask = cur_ins & 0b0111;

                if self.flags & mask != 0 {
                    self.jmp(args, cur_ins & 0b1000 == 0b1000);
                }
            }
            PUSH => {
                memory.write(self.stack_pointer, args.as_u8().unwrap_or(*self.acc_u8()));
                self.stack_pointer -= 1;
            }
            PUSHW => {
                self.stack_pointer -= 1;
                memory.write_index(self.stack_pointer, args.as_u16().unwrap_or(self.accumulator));
                self.stack_pointer -= 1;
            }
            POP => {
                self.stack_pointer += 1;
                *self.acc_u8() = memory.read(self.stack_pointer);
            }
            POPW => {
                self.stack_pointer += 1;
                self.accumulator = memory.read_index(self.stack_pointer);
                self.stack_pointer += 1;
            }
            RET => {
                self.stack_pointer += 2;
                self.pc = memory.read_index(self.stack_pointer+1);
            }
            CALL => {
                let call_location = args.as_u16().unwrap_or(self.accumulator);
                
                memory.write_index(self.stack_pointer-1, self.pc);
                self.stack_pointer -= 2;

                self.pc = call_location;
            }

            INT1 => {
                let mut bytes = [0];
                std::io::stdin().read_exact(&mut bytes).unwrap();
                *self.acc_u8() = bytes[0];
            }
            INT2 => {
                std::io::stdout().write_all(&[args.as_u8().unwrap_or(*self.acc_u8())]).unwrap();
            }
            INT3 => {
                std::io::stderr().write_all(&[args.as_u8().unwrap_or(*self.acc_u8())]).unwrap();
            }
            INT15 => {
                eprintln!("{:x?}", self);
                let mut stack = Vec::new();
                let mut i = self.stack_pointer;
                while i < 255 {
                    i += 1;
                    stack.push(memory.read(i));
                }
                eprintln!("Stack: {:x?}", stack);
            }
            i @ INT4 ..= INT14 => eprintln!("Interrupt{:X} called", i&0xf),
            HALT => return Some(Signal::PowerOff),
        }

        None
    }
}
