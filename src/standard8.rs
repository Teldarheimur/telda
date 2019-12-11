use super::{Machine, Memory, Memory8Bit, Cpu, Signal};
use std::io::{Write, Read};

pub type StandardMachine = Machine<u8, Memory8Bit, StandardCpu>;

#[derive(Debug)]
pub struct StandardCpu {
    pc: u8,
    stack_pointer: u8,
    work: u8,
    flags: u8,
}

impl StandardCpu {
    pub fn new() -> Self {
        StandardCpu {
            pc: 0,
            stack_pointer: 0xff,
            work: 0,
            flags: 0,
        }
    }
    fn read_arg<M: Memory<u8>>(&mut self, m: &M, indirection: bool) -> u8 {
        let ret;
        if indirection {
            ret = m.read(m.read(self.pc));
        } else {
            ret = m.read(self.pc);
        }
        self.pc += 1;

        ret
    }
    #[inline]
    fn sub<M: Memory<u8>>(&mut self, m: &M, indirection: bool) {
        let v = self.read_arg(m, indirection);

        let (work, o) = self.work.overflowing_sub(v);
        self.work = work;
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
    fn binop_overflowing<M: Memory<u8>>(&mut self, m: &M, indirection: bool, op: fn(u8, u8) -> (u8, bool)) {
        let v = self.read_arg(m, indirection);

        let (work, o) = op(self.work, v);
        self.work = work;
        self.flags &= 0b1111_0000;
        if o {
            self.flags |= 0b1000;
        }
    }
    #[inline]
    fn binop<M: Memory<u8>>(&mut self, m: &M, indirection: bool, op: fn(u8, u8) -> u8) {
        let v = self.read_arg(m, indirection);

        self.work = op(self.work, v);
        self.flags &= 0b1111_0000;
    }
    #[inline]
    fn cmp<M: Memory<u8>>(&mut self, m: &M, indirection: bool) {
        let v = self.read_arg(m, indirection);

        use std::cmp::Ordering::*;

        self.flags &= 0b1111_0000;
        self.flags |= match self.work.cmp(&v) {
            Greater => 0b0100,
            Less => 0b0010,
            Equal => 0b0001,
        };
    }
    #[inline]
    fn jmp<M: Memory<u8>>(&mut self, m: &M, indirection: bool, relative: bool) {
        let location = self.read_arg(m, indirection);

        self.pc = if relative {
            (self.pc as i8).wrapping_add(location as i8) as u8
        } else {
            location
        };
    }
}

macro_rules! instructions {
    ($enum_name:ident, $($name:ident = $opcode:expr;)*) => {
        $(
            pub const $name: u8 = $opcode;
        )*
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
        #[repr(u8)]
        pub enum $enum_name {
            $(
                $name = $name
            ),*
        }
        impl $enum_name {
            pub fn from_str(s: &str) -> Option<Self> {
                match s {
                    $( stringify!($name) => Some(Self::$name),)*
                    _ => None,
                }
            }
        }
    };
}

instructions!{Opcode,
    INVALID = 0x00;
    MOVE = 0x01;
    LOAD = 0x03;
    LEA = 0x04;
    STORE = 0x05;
    ADD = 0x0a;
    SUB = 0x0b;
    MUL = 0x0c;
    DIV = 0x0d;
    REM = 0x0e;
    NOP = 0x0f;
    AND = 0x06;
    OR = 0x07;
    XOR = 0x08;
    NOT = 0x09;
    COMPARE = 0x02;
    JUMP = 0x10;
    JMPR = 0x18;
    // RGLZ (all = overflow)
    // relative greater less zero
    JEZ = 0x11;
    JEZR = 0x19;
    JLT = 0x12;
    JLTR = 0x1a;
    JLE = 0x13;
    JLER = 0x1b;
    JGT = 0x14;
    JGTR = 0x1c;
    JGE = 0x15;
    JGER = 0x1d;
    JNE = 0x16;
    JNER = 0x1e;
    JIO = 0x17;
    JIOR = 0x1f;

    PUSH = 0x20;
    POP = 0x21;
    CALL = 0x22;
    RET = 0x23;
    INC = 0x24;
    DEC = 0x25;
    LSV = 0x26;
    MSP = 0x27;


    HALT = 0x70;
    INT1 = 0x71;
    INT2 = 0x72;
    INT3 = 0x73;
    INT4 = 0x74;
    INT5 = 0x75;
    INT6 = 0x76;
    INT7 = 0x77;
    INT8 = 0x78;
    INT9 = 0x79;
    INT10 = 0x7a;
    INT11 = 0x7b;
    INT12 = 0x7c;
    INT13 = 0x7d;
    INT14 = 0x7e;
    INT15 = 0x7f;
}

use std::ops::{BitAnd, BitOr, BitXor};

impl Cpu for StandardCpu {
    type Index = u8;

    fn run<M: Memory<Self::Index>>(&mut self, memory: &mut M) -> Option<Signal> {
        if self.pc == 0 {
            self.pc = memory.read(0);

            return None;
        }

        let cur_ins = memory.read(self.pc);
        self.pc += 1;

        let indirection = cur_ins & 0b1000_0000 == 0b1000_0000;

        match cur_ins & 0b0111_1111 {
            NOP => (),
            INVALID | 0x28..=0x6f | 0x80..= 0xff => panic!("Invalid instruction call {:02x}!\t{:x?}", cur_ins, self),
            MOVE => {
                let to_write = self.read_arg(memory, indirection);
                memory.write(memory.read(self.pc), to_write);
                self.pc += 1;
            }
            LEA => self.work = memory.read(self.read_arg(memory, indirection)),
            LOAD => self.work = self.read_arg(memory, indirection),
            STORE => memory.write(self.read_arg(memory, indirection), self.work),
            COMPARE => self.cmp(memory, indirection),
            SUB => self.sub(memory, indirection),
            ADD => self.binop_overflowing(memory, indirection, u8::overflowing_add),
            MUL => self.binop_overflowing(memory, indirection, u8::overflowing_mul),
            DIV => self.binop_overflowing(memory, indirection, u8::overflowing_div),
            REM => self.binop_overflowing(memory, indirection, u8::overflowing_rem),
            AND => self.binop(memory, indirection, u8::bitand),
            OR => self.binop(memory, indirection, u8::bitor),
            XOR => self.binop(memory, indirection, u8::bitxor),
            NOT => self.work = !self.read_arg(memory, indirection),
            JUMP | JMPR => self.jmp(memory, indirection, cur_ins & 0b1000 == 0b1000),
            JIO | JIOR => if self.flags & 0b1000 == 0b1000 {
                self.jmp(memory, indirection, cur_ins & 0b1000 == 0b1000)
            } else {
                self.pc += 2;
            }
            JEZ..=JNE | JEZR..= JNER => {
                let mask = cur_ins & 0b0111;

                if self.flags & mask != 0 {
                    self.jmp(memory, indirection, cur_ins & 0b1000 == 0b1000);
                } else {
                    self.pc += 1;
                }
            }
            PUSH => {
                memory.write(self.stack_pointer, self.work);
                self.stack_pointer -= 1;
            }
            POP => {
                self.stack_pointer += 1;
                self.work = memory.read(self.stack_pointer);
            }
            RET => {
                self.stack_pointer += 1;
                self.pc = memory.read(self.stack_pointer);
            }
            CALL => {
                let call_location = self.read_arg(memory, indirection);
                memory.write(self.stack_pointer, self.pc);
                self.stack_pointer -= 1;

                self.pc = call_location;
            }
            INC => self.work += 1,
            DEC => self.work -= 1,
            LSV => {
                let base = self.stack_pointer + 1;

                let location_of_data_to_load = base.wrapping_add(self.read_arg(memory, indirection));
                self.work = memory.read(location_of_data_to_load);
            }
            MSP => {
                self.stack_pointer = self.stack_pointer.wrapping_add(self.read_arg(memory, indirection));
            }

            INT1 => {
                let mut bytes = [0];
                std::io::stdin().read_exact(&mut bytes).unwrap();
                self.work = bytes[0];
            }
            INT2 => {
                std::io::stdout().write_all(&[self.work]).unwrap();
            }
            INT3 => {
                eprintln!("{:x?}", self);
                let mut stack = Vec::new();
                let mut i = self.stack_pointer;
                while i < 255 {
                    i += 1;
                    stack.push(memory.read(i));
                }
                eprintln!("Stack: {:x?}", stack);
            }
            HALT | INT4 ..= INT15 => return Some(Signal::PowerOff),
        }

        None
    }
}
