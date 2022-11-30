use std::fmt::{Display, self};

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
    pub x: u16,
    pub y: u16,

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
    fn read_a(&self) -> [u8; 2] {
        self.a.to_le_bytes()
    }
    fn read_b(&self) -> [u8; 2] {
        self.b.to_le_bytes()
    }
    pub fn read_al(&self) -> u8 {
        let [l, _h] = self.read_a();
        l
    }
    pub fn read_ah(&self) -> u8 {
        let [_l, h] = self.read_a();
        h
    }
    pub fn read_bl(&self) -> u8 {
        let [l, _h] = self.read_b();
        l
    }
    pub fn write_al(&mut self, val: u8) {
        let [_l, h] = self.read_a();
        self.a = u16::from_le_bytes([val, h])
    }
    pub fn write_ah(&mut self, val: u8) {
        let [l, _h] = self.read_a();
        self.a = u16::from_le_bytes([l, val])
    }
    pub fn write_bl(&mut self, val: u8) {
        let [_l, h] = self.read_b();
        self.b = u16::from_le_bytes([val, h])
    }
    pub fn read(&self, r: Register) -> u16 {
        match r.0 {
            0 => 0,
            1 => self.read_al() as u16,
            2 => self.read_ah() as u16,
            3 => self.read_bl() as u16,
            4 => self.a,
            5 => self.b,
            6 => self.x,
            7 => self.y,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn write(&mut self, r: Register, val: u16) {
        match r.0 {
            0 => (),
            1 => self.write_al(val as u8),
            2 => self.write_ah(val as u8),
            3 => self.write_bl(val as u8),
            4 => self.a = val,
            5 => self.b = val,
            6 => self.x = val,
            7 => self.y = val,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn trap(&mut self, trap_mode: TrapMode) {
        self.trap = true;
        self.trap_mode = trap_mode;
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Register(u8);

impl Register {
    pub const fn new(r: u8) -> Self {
        debug_assert!(r < 8);
        Register(r)
    }
    #[inline(always)]
    pub const fn is_byte(self) -> bool {
        self.0 < 4
    }
    #[inline(always)]
    pub const fn is_wide(self) -> bool {
        self.0 == 0 || self.0 >= 4
    }
    #[inline(always)]
    pub const fn is_zero(self) -> bool {
        self.0 == 0
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            0 => write!(f, "z"),
            1 => write!(f, "al"),
            2 => write!(f, "ah"),
            3 => write!(f, "bl"),
            4 => write!(f, "a"),
            5 => write!(f, "b"),
            6 => write!(f, "x"),
            7 => write!(f, "y"),
            _ => unimplemented!("no such register")
        }
    }
}
