use std::{fmt::{Display, self}, io::{Read, Write}};

use crate::{mem::Memory, isa::OP_HANDLERS};

pub struct Cpu {
    pub registers: Registers,
}

impl Cpu {
    pub fn new(pc: u16) -> Self {
        Cpu {
            registers: Registers {
                pc,
                sp: 0x7f_ff,
                .. Registers::default()
            }
        }
    }
    pub fn run_instruction(&mut self, mem: &mut dyn Memory) -> Result<(), TrapMode> {
        let opcode = mem.read(self.registers.pc);
        self.registers.pc += 1;

        OP_HANDLERS[opcode as usize](&mut self.registers, mem);

        if self.registers.trap {
            Err(self.registers.trap_mode)
        } else {
            Ok(())
        }
    }
    pub fn run_until_trap(&mut self, mem: &mut dyn Memory) -> TrapMode {
        loop {
            match self.run_instruction(mem) {
                Ok(()) => (),
                Err(tm) => break tm,
            }
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum TrapMode {
    Halt = 0,
    #[default]
    Invalid = 1,
    ZeroDiv = 2,
}

#[derive(Debug, Default, Clone)]
pub struct Registers {
    pub a: u16,
    pub b: u16,
    pub c: u16,
    pub x: u16,
    pub y: u16,
    pub z: u16,

    pub pc: u16,
    pub(crate) sp: u16,
    pub(crate) trap: bool,
    pub(crate) trap_mode: TrapMode,
    pub(crate) zero: bool,
    pub(crate) sign: bool,
    pub(crate) overflow: bool,
    pub(crate) carry: bool,
}

impl Registers {
    pub fn read_byte(&self, r: ByteRegister) -> u8 {
        match r.0 {
            0 => 0,
            1 => self.a.to_le_bytes()[0],
            2 => self.a.to_le_bytes()[1],
            3 => self.b.to_le_bytes()[0],
            4 => self.b.to_le_bytes()[1],
            5 => self.c.to_le_bytes()[0],
            6 => self.c.to_le_bytes()[1],
            7 => {
                let mut buf = [0];
                std::io::stdin().read(&mut buf).unwrap();
                buf[0]
            }
            _ => unimplemented!("no such register"),
        }
    }
    pub fn write_byte(&mut self, r: ByteRegister, val: u8) {
        match r.0 {
            0 => (),
            1 => {
                let [_low, high] = self.a.to_le_bytes();
                self.a = u16::from_le_bytes([val, high]);
            },
            2 => {
                let [low, _high] = self.a.to_le_bytes();
                self.a = u16::from_le_bytes([low, val]);
            },
            3 => {
                let [_low, high] = self.b.to_le_bytes();
                self.b = u16::from_le_bytes([val, high]);
            },
            4 => {
                let [low, _high] = self.b.to_le_bytes();
                self.b = u16::from_le_bytes([low, val]);
            },
            5 => {
                let [_low, high] = self.c.to_le_bytes();
                self.c = u16::from_le_bytes([val, high]);
            },
            6 => {
                let [low, _high] = self.c.to_le_bytes();
                self.c = u16::from_le_bytes([low, val]);
            },
            7 => {
                std::io::stdout().write_all(&[val]).unwrap();
            }
            _ => unimplemented!("no such register"),
        }
    }
    pub fn read_wide(&self, r: WideRegister) -> u16 {
        match r.0 {
            0 => 0,
            1 => self.a,
            2 => self.b,
            3 => self.c,
            4 => self.x,
            5 => self.y,
            6 => self.z,
            7 => self.sp,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn write_wide(&mut self, r: WideRegister, val: u16) {
        match r.0 {
            0 => (),
            1 => self.a = val,
            2 => self.b = val,
            3 => self.c = val,
            4 => self.x = val,
            5 => self.y = val,
            6 => self.z = val,
            7 => self.sp = val,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn trap(&mut self, trap_mode: TrapMode) {
        self.trap = true;
        self.trap_mode = trap_mode;
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ByteRegister(u8);
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct WideRegister(u8);

impl ByteRegister {
    pub const fn new(r: u8) -> Self {
        debug_assert!(r < 8);
        Self(r)
    }
}

impl WideRegister {
    pub const fn new(r: u8) -> Self {
        debug_assert!(r < 8);
        Self(r)
    }
}

impl Display for ByteRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            0 => write!(f, "rb0"),
            1 => write!(f, "al"),
            2 => write!(f, "ah"),
            3 => write!(f, "bl"),
            4 => write!(f, "bh"),
            5 => write!(f, "cl"),
            6 => write!(f, "ch"),
            7 => write!(f, "io"),
            _ => unimplemented!("no such register")
        }
    }
}
impl Display for WideRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            0 => write!(f, "r0"),
            1 => write!(f, "a"),
            2 => write!(f, "b"),
            3 => write!(f, "c"),
            4 => write!(f, "x"),
            5 => write!(f, "y"),
            6 => write!(f, "z"),
            7 => write!(f, "s"),
            _ => unimplemented!("no such register")
        }
    }
}
