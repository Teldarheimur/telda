
use super::{Machine, Memory, Memory16Bit, Cpu, Signal};
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
    pub fn new<M: Memory<u16>>(m: &M) -> Self {
        StandardCpu {
            pc: m.read_index(0),
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
        let v = a.as_u16().unwrap_or_default();

        let (work, o) = self.accumulator.overflowing_sub(v);
        self.accumulator = work;
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
                let (acc, o) = opw(self.accumulator, 0);
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
    fn binop(&mut self, a: Args, op: fn(u16, u16) -> u16) {
        let v = a.as_u16().unwrap_or_default();

        self.accumulator = op(self.accumulator, v);
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

        // This is here to make sure all opcodes are defined
        fn _dummy(n: u8) {
            match n {
                0b0100_0000..=0xff => (),
                $($opcode => ()),*
            }
        }

        impl $enum_name {
            pub fn from_str(s: &str) -> Option<Self> {
                match s {
                    $( stringify!($name) => Some(Self::$name),)*
                    _ => None,
                }
            }
            pub fn from_u8(i: u8) -> Option<Self> {
                if i & 0xc0 == 0 {
                    unsafe {
                        Some(std::mem::transmute(i))
                    }
                } else {
                    None
                }
            }
        }
    };
}

// OPCODE
// llgg oooo
// l: length of args 
// g: instruction group
// i: instruction

instructions!{Opcode,
    INVALID = 0x00;
    LOAD = 0x01;
    LDL = 0x02;
    STR = 0x03;
    STL = 0x04;
    COMPARE = 0x05;
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
    PUSHW = 0x21;
    POP = 0x22;
    POPW = 0x23;
    CALL = 0x24;
    RET = 0x25;
    INC = 0x26;
    DEC = 0x27;

    RES8 = 0x28;
    RES9 = 0x29;
    RESA = 0x2a;
    RESB = 0x2b;
    RESC = 0x2c;
    RESD = 0x2d;
    RESE = 0x2e;
    RESF = 0x2f;

    HALT = 0x30;
    INT1 = 0x31;
    INT2 = 0x32;
    INT3 = 0x33;
    INT4 = 0x34;
    INT5 = 0x35;
    INT6 = 0x36;
    INT7 = 0x37;
    INT8 = 0x38;
    INT9 = 0x39;
    INT10 = 0x3a;
    INT11 = 0x3b;
    INT12 = 0x3c;
    INT13 = 0x3d;
    INT14 = 0x3e;
    INT15 = 0x3f;
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
            RES8..=RESF => panic!("RESERVED"),
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
            ADD => self.binop_overflowing(args, u8::overflowing_add, u16::overflowing_add),
            MUL => self.binop_overflowing(args, u8::overflowing_mul, u16::overflowing_mul),
            DIV => self.binop_overflowing(args, u8::overflowing_div, u16::overflowing_div),
            REM => self.binop_overflowing(args, u8::overflowing_rem, u16::overflowing_rem),
            AND => self.binop(args, u16::bitand),
            OR => self.binop(args, u16::bitor),
            XOR => self.binop(args, u16::bitxor),
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
                memory.write(self.stack_pointer, *self.acc_u8());
                self.stack_pointer -= 1;
            }
            PUSHW => {
                self.stack_pointer -= 1;
                memory.write_index(self.stack_pointer, self.accumulator);
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
                let call_location = args.as_u16().unwrap_or_default();
                
                memory.write_index(self.stack_pointer-1, self.pc);
                self.stack_pointer -= 2;

                self.pc = call_location;
            }
            INC => self.accumulator += 1,
            DEC => self.accumulator -= 1,

            INT1 => {
                let mut bytes = [0];
                std::io::stdin().read_exact(&mut bytes).unwrap();
                *self.acc_u8() = bytes[0];
            }
            INT2 => {
                std::io::stdout().write_all(&[args.as_u8().unwrap_or(*self.acc_u8())]).unwrap();
            }
            INT3 => {
                eprintln!("{:?}", self);
            }
            HALT | INT4 ..= INT15 => return Some(Signal::PowerOff),
        }

        None
    }
}
