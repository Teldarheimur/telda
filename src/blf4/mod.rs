use std::fmt::{self, Display};

use crate::{
    machine::Cpu,
    mem::{MainMemory, VIRTUAL_IO_MAPPING_CUTOFF},
    U4,
};

pub mod isa;

mod register_type;

pub use self::register_type::*;
use isa::OP_HANDLERS;

pub type OpRes<T, E = TrapMode> = Result<T, E>;

#[derive(Debug, Clone, Copy, Default)]
pub struct Blf4Flags {
    pub user_mode: bool,
    pub trap: bool,
    pub reserved: bool,
    pub virtual_mode: bool,

    pub carry: bool,
    pub sign: bool,
    pub overflow: bool,
    pub zero: bool,
}

impl From<Blf4Flags> for u16 {
    fn from(flags: Blf4Flags) -> Self {
        let Blf4Flags {
            user_mode,
            trap,
            reserved,
            virtual_mode,
            zero,
            sign,
            overflow,
            carry,
        } = flags;

        ((zero as u16) << 7)
            | ((overflow as u16) << 6)
            | ((sign as u16) << 5)
            | ((carry as u16) << 4)
            | ((virtual_mode as u16) << 3)
            | ((reserved as u16) << 2)
            | ((trap as u16) << 1)
            | ((user_mode as u16) << 0)
    }
}

impl From<u16> for Blf4Flags {
    fn from(n: u16) -> Self {
        Blf4Flags {
            zero: n & (1 << 7) != 0,
            overflow: n & (1 << 6) != 0,
            sign: n & (1 << 5) != 0,
            carry: n & (1 << 4) != 0,
            virtual_mode: n & (1 << 3) != 0,
            reserved: n & (1 << 2) != 0,
            trap: n & (1 << 1) != 0,
            user_mode: n & (1 << 0) != 0,
        }
    }
}

impl Display for Blf4Flags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut just_zero = true;

        fn os(jz: &mut bool) -> &'static str {
            if *jz {
                *jz = false;
                ""
            } else {
                " | "
            }
        }

        if self.zero {
            write!(f, "zero")?;
            just_zero = false;
        }
        if self.overflow {
            write!(f, "{}overflow", os(&mut just_zero))?;
        }
        if self.sign {
            write!(f, "{}sign", os(&mut just_zero))?;
        }
        if self.carry {
            write!(f, "{}carry", os(&mut just_zero))?;
        }
        if self.virtual_mode {
            write!(f, "{}vm", os(&mut just_zero))?;
        }
        if self.user_mode {
            write!(f, "{}user", os(&mut just_zero))?;
        }

        if just_zero {
            write!(f, "0")
        } else {
            Ok(())
        }
    }
}

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
    pub flags: Blf4Flags,
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
            trap_handler: 0,
            flags: Blf4Flags::default(),
        }
    }
    pub fn context<'a, M: MainMemory>(&'a mut self, mem: &'a mut M) -> HandlerContext<'a> {
        HandlerContext { cpu: self, mem }
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
    pub fn read_wr(&self, r: WideRegister) -> OpRes<u16> {
        Ok(match r.0.into() {
            0 => 0,
            n @ 1..=10 => {
                let index = (n as usize - 1) << 1;
                let wreg = &self.general_purposes[index..index + 2];
                u16::from_le_bytes([wreg[0], wreg[1]])
            }
            11 => self.stack,
            12 => self.link,
            13 => self.frame,

            14 | 15 if self.flags.user_mode => return Err(TrapMode::IllegalOperation),
            14 => self.page,
            15 => self.trap_handler,
            _ => unimplemented!("no such register"),
        })
    }
    pub fn write_wr(&mut self, r: WideRegister, val: u16) -> OpRes<()> {
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
            14 | 15 if self.flags.user_mode => return Err(TrapMode::IllegalOperation),
            14 => self.page = val,
            15 => self.trap_handler = val,
            _ => unimplemented!("no such register"),
        }
        Ok(())
    }
}

impl Cpu for Blf4 {
    type TrapMode = TrapMode;
    fn execute_instruction<M: MainMemory>(&mut self, mem: &mut M) -> OpRes<(), Self::TrapMode> {
        let mut ctx = HandlerContext { cpu: self, mem };

        let opcode = ctx.fetch()?;

        match OP_HANDLERS[opcode as usize](&mut ctx) {
            Ok(()) => (),
            Err(tm) => {
                ctx.cpu.flags.trap = true;
                ctx.cpu.flags.user_mode = false;
                if ctx.cpu.trap_handler == 0 {
                    return Err(tm);
                } else {
                    ctx.push_registers()?;
                    self.program_counter = self.trap_handler;
                    self.write_wr(R1, tm as u8 as u16)?;
                }
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
    Level1PageFault = 0xe,
    Level2PageFault = 0xf,
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
    #[must_use = "error must be handled"]
    fn addr_resolve(&mut self, addr: u16, mode: AccessMode) -> OpRes<u32> {
        if !self.cpu.flags.virtual_mode {
            let offset = (self.cpu.page as u32) << 8;

            return Ok(if addr >= VIRTUAL_IO_MAPPING_CUTOFF {
                0xff_fff0 | (addr as u32 & 0xf)
            } else {
                offset | addr as u32
            });
        }

        let page_offset = (addr & 0x3f) as u32;
        let vpn2 = ((addr >> 6) & 0x1f) as u32;
        let vpn1 = (addr >> 11) as u32;

        let table1_start = self.cpu.page as u32;

        let lvl1_addr = table1_start + (vpn1 << 1);
        let lvl1_entry = u32::from_le_bytes([
            self.mem.read(lvl1_addr),
            self.mem.read(lvl1_addr + 1),
            self.mem.read(lvl1_addr + 2),
            0,
        ]);

        let lvl2_present = lvl1_entry & 1 == 1;
        if !lvl2_present {
            return Err(TrapMode::Level1PageFault);
        }
        let lvl2_addr = (lvl1_entry >> 1) + (3 * vpn2);
        let lvl2_entry = u32::from_le_bytes([
            self.mem.read(lvl2_addr),
            self.mem.read(lvl2_addr + 1),
            self.mem.read(lvl2_addr + 2),
            0,
        ]);

        let ppn = lvl2_entry >> 6;
        let flags = lvl2_entry & 0x3f;
        let page_present = flags & 1 != 0;
        let dirty = flags & 2 != 0;
        let supervisor_mode = flags & 4 != 0;
        let execute = flags & 8 != 0;
        let write = flags & 16 != 0;
        let read = flags & 32 != 0;

        if !page_present {
            return Err(TrapMode::Level2PageFault);
        }

        if self.cpu.flags.user_mode {
            if supervisor_mode {
                return Err(TrapMode::IllegalOperation);
            }

            match mode {
                AccessMode::Execute if !execute => return Err(TrapMode::IllegalExecute),
                AccessMode::Write if !write => return Err(TrapMode::IllegalWrite),
                AccessMode::Read if !read => return Err(TrapMode::IllegalRead),
                _ => ()
            }
        }

        if matches!(mode, AccessMode::Write if !dirty) {
            // set dirty flag
            let new_entry = lvl2_entry | 2;
            // convert to byte because the dirty flag is in the first byte
            // and so we can ignore the rest
            self.mem.write(lvl2_addr, new_entry as u8);
        }

        let physical_address = (ppn << 6) | page_offset;

        Ok(physical_address)
    }

    #[must_use = "error must be handled"]
    pub fn fetch(&mut self) -> OpRes<u8> {
        let addr = self.cpu.program_counter;
        self.cpu.program_counter += 1;
        let addr = self.addr_resolve(addr, AccessMode::Execute)?;
        Ok(self.mem.read(addr))
    }
    #[must_use = "error must be handled"]
    pub fn read(&mut self, addr: u16) -> OpRes<u8> {
        let addr = self.addr_resolve(addr, AccessMode::Read)?;
        Ok(self.mem.read(addr))
    }
    #[must_use = "error must be handled"]
    pub fn write(&mut self, addr: u16, val: u8) -> OpRes<()> {
        let addr = self.addr_resolve(addr, AccessMode::Write)?;
        self.mem.write(addr, val);
        Ok(())
    }
    #[must_use = "error must be handled"]
    pub fn read_wide(&mut self, addr: u16) -> OpRes<u16> {
        let lower = self.read(addr)?;
        let higher = self.read(addr + 1)?;

        Ok(u16::from_le_bytes([lower, higher]))
    }
    #[must_use = "error must be handled"]
    pub fn write_wide(&mut self, addr: u16, val: u16) -> OpRes<()> {
        let [lower, higher] = val.to_le_bytes();

        self.write(addr, lower)?;
        self.write(addr + 1, higher)?;

        Ok(())
    }

    #[must_use = "error must be handled"]
    pub fn pushw(&mut self, w: u16) -> OpRes<()> {
        self.cpu.stack -= 2;
        self.write_wide(self.cpu.stack, w)
    }
    #[must_use = "error must be handled"]
    pub fn pushb(&mut self, b: u8) -> OpRes<()> {
        self.cpu.stack -= 1;
        self.write(self.cpu.stack, b)
    }
    #[must_use = "error must be handled"]
    pub fn popw(&mut self) -> OpRes<u16> {
        let w = self.read_wide(self.cpu.stack)?;
        self.cpu.stack += 2;
        Ok(w)
    }
    #[must_use = "error must be handled"]
    pub fn popb(&mut self) -> OpRes<u8> {
        let b = self.read(self.cpu.stack)?;
        self.cpu.stack += 1;
        Ok(b)
    }
    #[must_use = "error must be handled"]
    pub fn push_registers(&mut self) -> OpRes<()> {
        self.pushw(self.cpu.flags.into())?;
        for r in 1..=15 {
            let w = self.cpu.read_wr(WideRegister(U4::new(r)))?;
            self.pushw(w)?;
        }

        Ok(())
    }
    #[must_use = "error must be handled"]
    pub fn pop_registers(&mut self) -> OpRes<()> {
        for r in (1..=15).rev() {
            let w = self.popw()?;
            self.cpu.write_wr(WideRegister(U4::new(r)), w)?;
        }
        self.cpu.flags = self.popw()?.into();

        Ok(())
    }
}
