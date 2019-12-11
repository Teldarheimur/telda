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
    pub fn new<M: Memory<u8>>(m: &M) -> Self {
        StandardCpu {
            pc: m.read_index(0),
            stack_pointer: 0xff,
            work: 0,
            flags: 0,
        }
    }
    #[inline]
    fn sub(&mut self, arg: Args) {
        let (work, o) = self.work.overflowing_sub(arg.as_u8().unwrap_or(self.work));
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
    fn binop_overflowing(&mut self, arg: Args, op: fn(u8, u8) -> (u8, bool)) {
        let (work, o) = op(self.work, arg.as_u8().unwrap_or(self.work));
        self.work = work;
        self.flags &= 0b1111_0000;
        if o {
            self.flags |= 0b1000;
        }
    }
    #[inline]
    fn binop(&mut self, arg: Args, op: fn(u8, u8) -> u8) {
        self.work = op(self.work, arg.as_u8().unwrap_or(self.work));
        self.flags &= 0b1111_0000;
    }
    #[inline]
    fn cmp(&mut self, arg: Args) {
        use std::cmp::Ordering::*;

        self.flags &= 0b1111_0000;
        self.flags |= match self.work.cmp(&arg.as_u8().unwrap_or(self.work)) {
            Greater => 0b0100,
            Less => 0b0010,
            Equal => 0b0001,
        };
    }
    #[inline]
    fn jmp(&mut self, arg: Args, relative: bool) {
        let location = arg.as_u8().unwrap_or(self.work);

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
    RESPUSH = 0x21;
    POP = 0x22;
    RESPOP = 0x23;
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

#[derive(Debug)]
pub enum Args {
    Null,
    Byte(u8),
}

use self::Args::*;

impl Args {
    fn new(args: &[u8]) -> Args {
        match args.len() {
            0 => Null,
            1 => Byte(args[0]),
            2 | 3 => unimplemented!(),
            _ => unreachable!(),
        }
    }
    #[inline]
    fn as_u8(&self) -> Option<u8> {
        match *self {
            Null => None,
            Byte(b) => Some(b),
        }
    }
}

use std::ops::{BitAnd, BitOr, BitXor};

impl Cpu for StandardCpu {
    type Index = u8;

    fn run<M: Memory<Self::Index>>(&mut self, memory: &mut M) -> Option<Signal> {
        let cur_ins = memory.read(self.pc);

        let args_length = cur_ins >> 6;
        let args = Args::new(memory.read_to_slice(self.pc+1, args_length));

        self.pc += 1 + args_length;

        match cur_ins & 0b0011_1111 {
            0b0100_0000..=0xff => unreachable!(),
            NOP => (),
            RES8..=RESF => panic!("RESERVED"),
            INVALID | RESPUSH | RESPOP => panic!("Invalid instruction call {:02x}!\t{:x?}", cur_ins, self),
            LOAD => {
                match args {
                    Null => self.work = 0,
                    Byte(b) => self.work = b,
                }
            }
            LDL => {
                self.work = memory.read_index(args.as_u8().unwrap_or_else(|| self.work));
            }
            STR => {
                let location = args.as_u8().unwrap_or(0);
                memory.write(location, self.work);
            }
            STL => {
                let location = memory.read_index(args.as_u8().unwrap_or(0));
                memory.write(location, self.work);
            }
            COMPARE => self.cmp(args),
            SUB => self.sub(args),
            ADD => self.binop_overflowing(args, u8::overflowing_add),
            MUL => self.binop_overflowing(args, u8::overflowing_mul),
            DIV => self.binop_overflowing(args, u8::overflowing_div),
            REM => self.binop_overflowing(args, u8::overflowing_rem),
            AND => self.binop(args, u8::bitand),
            OR => self.binop(args, u8::bitor),
            XOR => self.binop(args, u8::bitxor),
            NOT => self.work = !args.as_u8().unwrap_or(self.work),
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
                let call_location = args.as_u8().unwrap_or(self.work);
                memory.write(self.stack_pointer, self.pc);
                self.stack_pointer -= 1;

                self.pc = call_location;
            }
            INC => self.work += 1,
            DEC => self.work -= 1,

            INT1 => {
                let mut bytes = [0];
                std::io::stdin().read_exact(&mut bytes).unwrap();
                self.work = bytes[0];
            }
            INT2 => {
                std::io::stdout().write_all(&[args.as_u8().unwrap_or(self.work)]).unwrap();
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
