use super::{Machine, Memory, Memory16Bit, Cpu, Signal};
use std::io::{Write, Read};

pub type StandardMachine = Machine<u8, u16, Memory16Bit, StandardCpu>;

#[derive(Debug)]
pub struct StandardCpu {
    pc: u16,
    stack_pointer: u16,
    base_pointer: u16,
    counter: u16,
    accumulator: u8,
    flags: u8,
}

impl StandardCpu {
    pub fn new<M: Memory<u16, Cell = u8>>(m: &M) -> Self {
        StandardCpu {
            pc: m.read_index(0),
            stack_pointer: m.read_index(2),
            base_pointer: m.read_index(4),
            counter: m.read_index(6),
            accumulator: m.read(7),
            flags: m.read(8),
        }
    }
    fn read_arg<M: Memory<u16, Cell = u8>>(&mut self, m: &M, indirection: bool) -> u8 {
        let ret;
        if indirection {
            ret = m.read(m.read_index(self.pc));
            self.pc += 2;
        } else {
            ret = m.read(self.pc);
            self.pc += 1;
        }

        ret
    }
    fn read_arg_index<M: Memory<u16, Cell = u8>>(&mut self, m: &M, indirection: bool) -> u16 {
        let ret = if indirection {
            m.read_index(m.read_index(self.pc))
        } else {
            m.read_index(self.pc)
        };
        self.pc += 2;

        ret
    }
    #[inline]
    fn sub<M: Memory<u16, Cell = u8>>(&mut self, m: &M, indirection: bool) {
        let v = self.read_arg(m, indirection);

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
    fn binop_overflowing<M: Memory<u16, Cell = u8>>(&mut self, m: &M, indirection: bool, op: fn(u8, u8) -> (u8, bool)) {
        let v = self.read_arg(m, indirection);

        let (work, o) = op(self.accumulator, v);
        self.accumulator = work;
        self.flags &= 0b1111_0000;
        if o {
            self.flags |= 0b1000;
        }
    }
    #[inline]
    fn binop<M: Memory<u16, Cell = u8>>(&mut self, m: &M, indirection: bool, op: fn(u8, u8) -> u8) {
        let v = self.read_arg(m, indirection);

        self.accumulator = op(self.accumulator, v);
        self.flags &= 0b1111_0000;
    }
    #[inline]
    fn cmp<M: Memory<u16, Cell = u8>>(&mut self, m: &M, indirection: bool) {
        let v = self.read_arg(m, indirection);

        use std::cmp::Ordering::*;

        self.flags &= 0b1111_0000;
        self.flags |= match self.accumulator.cmp(&v) {
            Greater => 0b0100,
            Less => 0b0010,
            Equal => 0b0001,
        };
    }
    #[inline]
    fn jmp<M: Memory<u16, Cell = u8>>(&mut self, m: &M, indirection: bool, relative: bool) {
        let location = self.read_arg_index(m, indirection);

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

// TODO stack pointer points one beside the top value, having lead to some off-by-one errors
// LEA and LOAD and MOVE should be merged, LEA doesn't do what LEA does in x86 and is therefore a confusing name
// MOVE is the other way around rn
// Fix handling of u16s vs u8 since currently the register can only hold a u8
// Add enter and leave instructions for stack frames

// OPCODE
// llgg oooo
// l: length of args 
// g: instruction group
// i: instruction

instructions!{Opcode,
    INVALID = 0x00;
    // MOVE reg, immediate
    // MOVE reg, reg
    // MOVE reg, [addr]
    MOVR = 0x01;
    // MOVE [reg], immediate
    // MOVE [reg], reg
    MOVT = 0x03;
    // MOVE [addr], immediate
    // MOVE [addr], reg
    STORE = 0x04;
    RESERVED5 = 0x05;
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

    RES7 = 0x27;
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

impl Cpu for StandardCpu {
    type Cell = u8;
    type Index = u16;

    fn run<M: Memory<Self::Index, Cell = Self::Cell>>(&mut self, memory: &mut M) -> Option<Signal> {
        let cur_ins = memory.read(self.pc);

        // TODO use this
        // load the args into an array and read them from there instead of directly from memory
        let args_length = cur_ins >> 6;
        
        self.pc += 1 + args_length as u16;

        let indirection = args_length > 1;

        match cur_ins & 0b0011_1111 {
            0b0100_0000..=0xff => unreachable!(),
            NOP => (),
            RES7..=RESF => panic!("RESERVED"),
            INVALID => panic!("Invalid instruction call {:2x}!\n{:?}", cur_ins, self),
            MOVR => {
                let to_write = self.read_arg(memory, indirection);
                memory.write(memory.read_index(self.pc), to_write);
                self.pc += 2;
            }
            MOVT => self.accumulator = memory.read(self.read_arg_index(memory, indirection)),
            RESERVED5 => self.accumulator = self.read_arg(memory, indirection),
            STORE => memory.write(self.read_arg_index(memory, indirection), self.accumulator),
            COMPARE => self.cmp(memory, indirection),
            SUB => self.sub(memory, indirection),
            ADD => self.binop_overflowing(memory, indirection, u8::overflowing_add),
            MUL => self.binop_overflowing(memory, indirection, u8::overflowing_mul),
            DIV => self.binop_overflowing(memory, indirection, u8::overflowing_div),
            REM => self.binop_overflowing(memory, indirection, u8::overflowing_rem),
            AND => self.binop(memory, indirection, u8::bitand),
            OR => self.binop(memory, indirection, u8::bitor),
            XOR => self.binop(memory, indirection, u8::bitxor),
            NOT => self.accumulator = !self.read_arg(memory, indirection),
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
                    self.pc += 2;
                }
            }
            PUSH => {
                memory.write(self.stack_pointer, self.accumulator);
                self.stack_pointer -= 1;
            }
            POP => {
                self.stack_pointer += 1;
                self.accumulator = memory.read(self.stack_pointer);
            }
            RET => {
                self.stack_pointer += 2;
                self.pc = memory.read_index(self.stack_pointer+1);
            }
            CALL => {
                let call_location = self.read_arg_index(memory, indirection);
                memory.write_index(self.stack_pointer-1, self.pc);
                self.stack_pointer -= 2;

                self.pc = call_location;
            }
            INC => self.accumulator += 1,
            DEC => self.accumulator -= 1,
            LSV => {
                let base = self.stack_pointer + 1;

                let location_of_data_to_load = base.wrapping_add(self.read_arg_index(memory, indirection));
                self.accumulator = memory.read(location_of_data_to_load);
            }

            INT1 => {
                let mut bytes = [0];
                std::io::stdin().read_exact(&mut bytes).unwrap();
                self.accumulator = bytes[0];
            }
            INT2 => {
                std::io::stdout().write_all(&[self.accumulator]).unwrap();
            }
            INT3 => {
                eprintln!("{:?}", self);
            }
            HALT | INT4 ..= INT15 => return Some(Signal::PowerOff),
        }

        None
    }
}
