use crate::{
    mem::{MainMemory, VIRTUAL_IO_MAPPING_CUTOFF},
    U4, machine::Cpu,
};

pub mod isa;

mod register_type;

pub use self::register_type::*;
use isa::OP_HANDLERS;

#[derive(Debug, Clone)]
pub struct Blf4 {
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

impl Blf4 {
    pub fn new(start: u16) -> Self {
        let seed = (0b1001_1100 ^ (!start)) as u8;
        // Pseudo-randomise starting registers so that they cannot be relied on
        Blf4 {
            general_purposes: [seed; 20],
            program_counter: start,
            link: 0,
            page: 0,
            frame: VIRTUAL_IO_MAPPING_CUTOFF,
            stack: VIRTUAL_IO_MAPPING_CUTOFF,
            trap: false,
            trap_handler: 0,
            trap_mode: TrapMode::default(),
            zero: false,
            sign: false,
            overflow: false,
            carry: false,
        }
    }
    pub fn context<'a, M: MainMemory>(&'a mut self, mem: &'a mut M) -> HandlerContext<'a> {
        HandlerContext {
            cpu: self,
            mem,
        }
    }

    pub fn read_br(&self, r: ByteRegister) -> u8 {
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
    pub fn write_br(&mut self, r: ByteRegister, val: u8) {
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
    pub fn read_wr(&self, r: WideRegister) -> u16 {
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
    pub fn write_wr(&mut self, r: WideRegister, val: u16) {
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

impl Cpu for Blf4 {
    type TrapMode = TrapMode;
    fn execute_instruction<M: MainMemory>(&mut self, mem: &mut M) -> Result<(), Self::TrapMode> {
        let mut ctx = HandlerContext {
            cpu: self,
            mem,
        };

        let opcode = ctx.fetch();

        OP_HANDLERS[opcode as usize](&mut ctx);

        if ctx.cpu.trap {
            if ctx.cpu.trap_handler == 0 {
                return Err(self.trap_mode);
            } else {
                ctx.push_registers();
                self.program_counter = self.trap_handler;
                self.write_wr(R1, self.trap_mode as u8 as u16);
            }
        }

        Ok(())
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

#[derive(Debug)]
enum AccessMode {
    Read,
    Write,
    Execute,
}

pub struct HandlerContext<'a> {
    pub cpu: &'a mut Blf4,
    mem: &'a mut dyn MainMemory,
}

impl HandlerContext<'_> {
    fn addr_resolve(&self, addr: u16, mode: AccessMode) -> Option<u32> {
        if self.cpu.page == 0 {
            Some(if addr >= VIRTUAL_IO_MAPPING_CUTOFF {
                0xff_fff0 | (addr as u32 & 0xf)
            } else {
                addr as u32
            })
        } else {
            todo!("mode: {mode:?}")
        }
    }

    pub fn fetch(&mut self) -> u8 {
        let addr = self.cpu.program_counter;
        self.cpu.program_counter += 1;
        let Some(addr) = self.addr_resolve(addr, AccessMode::Execute) else {
            self.cpu.trap(TrapMode::IllegalExecute);
            return 0;
        };
        self.mem.read(addr)
    }
    pub fn read(&mut self, addr: u16) -> u8 {
        let Some(addr) = self.addr_resolve(addr, AccessMode::Read) else {
            self.cpu.trap(TrapMode::IllegalRead);
            return 0;
        };
        self.mem.read(addr)
    }
    pub fn write(&mut self, addr: u16, val: u8) {
        let Some(addr) = self.addr_resolve(addr, AccessMode::Write) else {
            self.cpu.trap(TrapMode::IllegalWrite);
            return;
        };
        self.mem.write(addr, val);
    }
    pub fn read_wide(&mut self, addr: u16) -> u16 {
        let lower = self.read(addr);
        let higher = self.read(addr + 1);

        u16::from_le_bytes([lower, higher])
    }
    pub fn write_wide(&mut self, addr: u16, val: u16) {
        let [lower, higher] = val.to_le_bytes();

        self.write(addr, lower);
        self.write(addr + 1, higher);
    }


    pub fn pushw(&mut self, w: u16) {
        self.cpu.stack -= 2;
        self.write_wide(self.cpu.stack, w);
    }
    pub fn pushb(&mut self, b: u8) {
        self.cpu.stack -= 1;
        self.write(self.cpu.stack, b);
    }
    pub fn popw(&mut self) -> u16 {
        let w = self.read_wide(self.cpu.stack);
        self.cpu.stack += 2;
        w
    }
    pub fn popb(&mut self) -> u8 {
        let b = self.read(self.cpu.stack);
        self.cpu.stack += 1;
        b
    }
    pub fn push_registers(&mut self) {
        let Blf4 {
            zero,
            sign,
            overflow,
            carry,
            ..
        } = *self.cpu;

        let flags = ((zero as u16) << 7)
            | ((overflow as u16) << 6)
            | ((sign as u16) << 5)
            | ((carry as u16) << 4);
        self.pushw(flags);
        for r in 1..=15 {
            let w = self.cpu.read_wr(WideRegister(U4::new(r)));
            self.pushw(w);
        }
    }
    pub fn pop_registers(&mut self) {
        for r in (1..=15).rev() {
            let w = self.popw();
            self.cpu.write_wr(WideRegister(U4::new(r)), w);
        }
        let flags = self.popw();

        self.cpu.zero = flags & 0b1000_0000 != 0;
        self.cpu.overflow = flags & 0b0100_0000 != 0;
        self.cpu.sign = flags & 0b0010_0000 != 0;
        self.cpu.carry = flags & 0b0001_0000 != 0;
    }
}
