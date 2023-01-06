use crate::{
    isa::OP_HANDLERS,
    mem::{Memory, IO_MAPPING_CUTOFF},
    U4,
};

mod register_type;

pub use self::register_type::*;

pub struct Cpu {
    pub registers: Registers,
}

impl Cpu {
    pub fn new(pc: u16) -> Self {
        Cpu {
            registers: Registers::new(pc),
        }
    }
    pub fn run_instruction(&mut self, mem: &mut dyn Memory) -> Result<(), TrapMode> {
        let opcode = mem.read(self.registers.program_counter);
        self.registers.program_counter += 1;

        OP_HANDLERS[opcode as usize](&mut self.registers, mem);

        if self.registers.trap {
            if self.registers.trap_handler == 0 {
                return Err(self.registers.trap_mode);
            } else {
                Self::push_registers(&mut self.registers, mem);
                self.registers.program_counter = self.registers.trap_handler;
                self.registers
                    .write_wide(R1, self.registers.trap_mode as u8 as u16);
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
        registers.stack -= 2;
        mem.write_wide(registers.stack, w);
    }
    pub fn pushb<M: ?Sized + Memory>(registers: &mut Registers, b: u8, mem: &mut M) {
        registers.stack -= 1;
        mem.write(registers.stack, b);
    }
    pub fn popw<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) -> u16 {
        let w = mem.read_wide(registers.stack);
        registers.stack += 2;
        w
    }
    pub fn popb<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) -> u8 {
        let b = mem.read(registers.stack);
        registers.stack += 1;
        b
    }
    pub fn push_registers<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) {
        let Registers {
            zero,
            sign,
            overflow,
            carry,
            ..
        } = *registers;

        let flags = ((zero as u16) << 7)
            | ((overflow as u16) << 6)
            | ((sign as u16) << 5)
            | ((carry as u16) << 4);
        Self::pushw(registers, flags, mem);
        for r in 1..=15 {
            let w = registers.read_wide(WideRegister(U4::new(r)));
            Self::pushw(registers, w, mem);
        }
    }
    pub fn pop_registers<M: ?Sized + Memory>(registers: &mut Registers, mem: &mut M) {
        for r in (1..=15).rev() {
            let w = Self::popw(registers, mem);
            registers.write_wide(WideRegister(U4::new(r)), w);
        }
        let flags = Self::popw(registers, mem);

        registers.zero = flags & 0b1000_0000 != 0;
        registers.overflow = flags & 0b0100_0000 != 0;
        registers.sign = flags & 0b0010_0000 != 0;
        registers.carry = flags & 0b0001_0000 != 0;
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum TrapMode {
    #[default]
    Invalid = 0,
    SysCall = 0x5,
    ZeroDiv = 0x8,
    Halt = 0xa,
    IllegalOperation = 0x10,
    IllegalRead = 0x11,
    IllegalWrite = 0x12,
    IllegalExecute = 0x13,
    IllegalHandlerReturn = 0x1f,
}

pub struct Registers {
    general_purposes: [u8; 20],

    pub stack: u16,
    pub link: u16,
    pub frame: u16,
    pub page: u16,
    pub program_counter: u16,
    /// Zero means no trap handler
    pub trap_handler: u16,
    pub trap_mode: TrapMode,
    pub trap: bool,
    pub zero: bool,
    pub sign: bool,
    pub overflow: bool,
    pub carry: bool,
}

impl Registers {
    pub fn new(start: u16) -> Self {
        let seed = (0b1001_1100 ^ (!start)) as u8;
        // Pseudo-randomise starting registers so that they cannot be relied on
        Registers {
            general_purposes: [seed; 20],
            program_counter: start,
            link: 0,
            page: 0,
            frame: IO_MAPPING_CUTOFF,
            stack: IO_MAPPING_CUTOFF,
            trap: false,
            trap_handler: 0,
            trap_mode: TrapMode::default(),
            zero: false,
            sign: false,
            overflow: false,
            carry: false,
        }
    }
    pub fn read_byte(&self, r: ByteRegister) -> u8 {
        match r.0.into() {
            0 => 0,
            n @ 1..=10 => {
                let index = n as usize - 1;

                self.general_purposes[index]
            }
            n @ 11..=15 => {
                let index = 10 + ((n as usize - 11) << 1);
                let wreg = &self.general_purposes[index..index + 2];
                wreg[0]
            }
            _ => unimplemented!("no such register"),
        }
    }
    pub fn write_byte(&mut self, r: ByteRegister, val: u8) {
        match r.0.into() {
            0 => (),
            n @ 1..=10 => {
                let index = n as usize - 1;

                self.general_purposes[index] = val;
            }
            n @ 11..=15 => {
                let index = 10 + ((n as usize - 11) << 1);
                let wreg = &mut self.general_purposes[index..index + 2];
                wreg[0] = val;
                wreg[1] = 0;
            }
            _ => unimplemented!("no such register"),
        }
    }
    pub fn read_wide(&self, r: WideRegister) -> u16 {
        match r.0.into() {
            0 => 0,
            n @ 1..=10 => {
                let index = (n as usize - 1) << 1;
                let wreg = &self.general_purposes[index..index + 2];
                u16::from_le_bytes([wreg[0], wreg[1]])
            }
            11 => self.stack,
            12 => self.link,
            13 => self.frame,
            14 => self.page,
            15 => self.trap_handler,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn write_wide(&mut self, r: WideRegister, val: u16) {
        match r.0.into() {
            0 => (),
            n @ 1..=10 => {
                let index = (n as usize - 1) << 1;
                let wreg = &mut self.general_purposes[index..index + 2];
                let [l, h] = val.to_le_bytes();
                wreg[0] = l;
                wreg[1] = h;
            }
            11 => self.stack = val,
            12 => self.link = val,
            13 => self.frame = val,
            14 => self.page = val,
            15 => self.trap_handler = val,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn trap(&mut self, trap_mode: TrapMode) {
        self.trap = true;
        self.trap_mode = trap_mode;
    }
}
