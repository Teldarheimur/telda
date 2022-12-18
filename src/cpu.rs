use std::{fmt::{Display, self}, io::{Read, Write}};

use crate::{mem::Memory, isa::OP_HANDLERS};

struct StdPort;

impl IoPort for StdPort {
    fn read(&mut self) -> u8 {
        let mut buf = [0];
        std::io::stdin().read(&mut buf).unwrap();
        buf[0]
    }
    fn write(&mut self, val: u8) {
        std::io::stdout().write_all(&[val]).unwrap()
    }
}

pub struct Cpu {
    pub registers: Registers,
}

impl Cpu {
    pub fn new(pc: u16, io_port: Box<dyn IoPort>) -> Self {
        Cpu {
            registers: Registers::new(pc, io_port)
        }
    }
    pub fn new_with_stdport(pc: u16) -> Self {
        Self::new(pc, Box::new(StdPort))
    }
    pub fn run_instruction(&mut self, mem: &mut dyn Memory) -> Result<(), TrapMode> {
        let opcode = mem.read(self.registers.pc);
        self.registers.pc += 1;

        OP_HANDLERS[opcode as usize](&mut self.registers, mem);

        if self.registers.trap {
            if self.registers.trap_handler == 0 {
                return Err(self.registers.trap_mode);
            } else {
                Self::push_registers(&mut self.registers, mem);
                let trap_handler_addr = mem.read_wide(self.registers.trap_handler as u16);
                self.registers.pc = trap_handler_addr;
                self.registers.a = self.registers.trap_mode as u8 as u16;
            }
        }

        Ok(())
    }
    /// Until unhandled trap
    pub fn run_until_abort(&mut self, mem: &mut dyn Memory) -> TrapMode {
        loop {
            match self.run_instruction(mem) {
                Ok(()) => (),
                Err(tm) => break tm,
            }
        }
    }


    pub fn pushw<M: ?Sized + Memory>(registers: &mut Registers, w: u16, mem: &mut M) {
        registers.sp -= 2;
        mem.write_wide(registers.sp, w);
    }
    pub fn pushb<M: ?Sized + Memory>(registers: &mut Registers, b: u8, mem: &mut M) {
        registers.sp -= 1;
        mem.write(registers.sp, b);
    }
    pub fn popw<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) -> u16 {
        let w = mem.read_wide(registers.sp);
        registers.sp += 2;
        w
    }
    pub fn popb<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) -> u8 {
        let b = mem.read(registers.sp);
        registers.sp += 1;
        b
    }
    pub fn push_registers<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) {
        let Registers {
            a, b, c, x, y, z, io_port: _, pc,
            sp: _, trap_handler: _, trap_mode: _, trap: _, zero, sign, overflow, carry
        } = *registers;

        Self::pushw(registers, a, mem);
        Self::pushw(registers, b, mem);
        Self::pushw(registers, c, mem);
        Self::pushw(registers, x, mem);
        Self::pushw(registers, y, mem);
        Self::pushw(registers, z, mem);
        let flags = ((zero as u16) << 7) | ((overflow as u16) << 6) | ((sign as u16) << 5) | ((carry as u16) << 4);
        Self::pushw(registers, flags, mem);
        Self::pushw(registers, pc, mem);
    }
    pub fn pop_registers<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) {
        let pc = Self::popw(registers, mem);
        let flags = Self::popw(registers, mem);
        let z = Self::popw(registers, mem);
        let y = Self::popw(registers, mem);
        let x = Self::popw(registers, mem);
        let c = Self::popw(registers, mem);
        let b = Self::popw(registers, mem);
        let a = Self::popw(registers, mem);

        registers.a = a;
        registers.b = b;
        registers.c = c;
        registers.x = x;
        registers.y = y;
        registers.z = z;
        registers.pc = pc;
        registers.zero = flags & 0b1000_0000 != 0;
        registers.overflow = flags & 0b0100_0000 != 0;
        registers.sign = flags & 0b0010_0000 != 0;
        registers.carry = flags & 0b0001_0000 != 0;
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum TrapMode {
    Halt = 0,
    #[default]
    Invalid = 1,
    ZeroDiv = 2,
    InvalidHandlerReturn = 3,
}

pub trait IoPort {
    fn read(&mut self) -> u8;
    fn write(&mut self, val: u8);
}

pub struct Registers {
    pub a: u16,
    pub b: u16,
    pub c: u16,
    pub x: u16,
    pub y: u16,
    pub z: u16,
    io_port: Box<dyn IoPort>,

    pub pc: u16,
    pub sp: u16,
    /// Address in memory where the location of the trap handler is located
    /// 
    /// 0 means no handler is set, 0x01-0xff are allowed.
    pub trap_handler: u8,
    pub trap_mode: TrapMode,
    pub trap: bool,
    pub zero: bool,
    pub sign: bool,
    pub overflow: bool,
    pub carry: bool,
}

impl Registers {
    pub fn new(pc: u16, io_port: Box<dyn IoPort>) -> Self {
        let seed = (pc << 8) | (!pc);
        // Pseudo-randomise starting registers so that they cannot be relied on
        Registers {
            a: seed ^ 0x47f4,
            b: seed ^ 0xe657,
            c: seed ^ 0x3933,
            x: seed ^ 0xa2be,
            y: seed ^ 0x2caf,
            z: seed ^ 0x5661,
            io_port,
            pc,
            sp: 0x7f_ff,
            trap: false,
            trap_handler: 0,
            trap_mode: TrapMode::default(),
            zero: false,
            sign: false,
            overflow: false,
            carry: false,
        }
    }
    pub fn read_byte(&mut self, r: ByteRegister) -> u8 {
        match r.0 {
            0 => 0,
            1 => self.a.to_le_bytes()[0],
            2 => self.a.to_le_bytes()[1],
            3 => self.b.to_le_bytes()[0],
            4 => self.b.to_le_bytes()[1],
            5 => self.c.to_le_bytes()[0],
            6 => self.c.to_le_bytes()[1],
            7 => self.io_port.read(),
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
                self.io_port.write(val);
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
