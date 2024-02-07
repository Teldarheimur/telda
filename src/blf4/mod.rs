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

#[derive(Debug, Clone, Copy)]
enum AccessMode {
    Read,
    Write,
    Execute,
}

pub struct HandlerContext<'a> {
    pub cpu: &'a mut Blf4,
    mem: &'a mut dyn MainMemory,
}

struct Entry {
    /// PPN shifted so the last 7 bits are zero
    addr: u32,
    /// If the entry was not dirty, this is used to set the dirty bit.
    byte_with_dirty_flag: Option<u8>,
}

impl HandlerContext<'_> {
    fn read_entry(&mut self, addr: u32, in_user_mode: bool, mode: AccessMode) -> OpRes<Entry> {
        let raw_entry = u32::from_le_bytes([
            self.mem.read(addr),
            self.mem.read(addr + 1),
            self.mem.read(addr + 2),
            self.mem.read(addr + 3),
        ]);

        let present = raw_entry & 1 == 1;
        if !present {
            return Err(TrapMode::Level2PageFault);
        }

        let _reserved = (raw_entry >> 6) & 0x100;

        let f_user_mode = raw_entry & 0b0010_0000 != 0;
        let f_dirty = raw_entry & 0b0001_0000 != 0;
        let f_execute = raw_entry & 0b0000_1000 != 0;
        let f_write = raw_entry & 0b0000_0100 != 0;
        let f_read = raw_entry & 0b0000_0010 != 0;

        // if the user_mode flag is NOT set, this page is
        // illegal if we are in user mode
        if in_user_mode && !f_user_mode {
            return Err(TrapMode::IllegalOperation);
        }

        match mode {
            AccessMode::Execute if !f_execute => return Err(TrapMode::IllegalExecute),
            AccessMode::Write if !f_write => return Err(TrapMode::IllegalWrite),
            AccessMode::Read if !f_read => return Err(TrapMode::IllegalRead),
            _ => ()
        }

        Ok(Entry {
            // the first 17 bits are the PPN (32-17 = 15)
            // shift 7 bits back to make room for the offset
            // and make it a physical address at the beginning of the page
            addr: (raw_entry >> 15) << 7,
            byte_with_dirty_flag: (!f_dirty).then_some((raw_entry & 0xff) as u8),
        })
    }

    #[must_use = "error must be handled"]
    fn addr_resolve(&mut self, addr: u16, mode: AccessMode) -> OpRes<u32> {
        if !self.cpu.flags.virtual_mode {
            // set all bits in the high byte when in direct addressing mode
            // note that this also puts the I/O mapped area inside the addressable space
            return Ok(0xff_0000 | addr as u32);
        }

        const fn entry_addr(table_start: u32, number: u32) -> u32 {
            table_start + number<<2
        }

        // split address into the two levels of virtual page numbers and the offset
        let page_offset = (addr & 0x7f) as u32;
        let vpn2 = ((addr >> 7) & 0x1f) as u32;
        let vpn1 = (addr >> 12) as u32;

        // coerce into u32 meaning the page table pointer points to 0x00_xxxx
        let table1_start = self.cpu.page as u32;

        let in_user_mode = self.cpu.flags.user_mode;

        // The level one entry is located at this address
        let lvl1_entry_addr = entry_addr(table1_start, vpn1);
        let lvl1_entry = self.read_entry(lvl1_entry_addr, in_user_mode, mode)
            .map_err(|tm| if let TrapMode::Level2PageFault = tm { TrapMode::Level1PageFault } else { tm })?;

        let table2_start = lvl1_entry.addr;

        let lvl2_entry_addr = entry_addr(table2_start, vpn2);
        let lvl2_entry = self.read_entry(lvl2_entry_addr, in_user_mode, mode)?;

        let ppn_shifted = lvl2_entry.addr;

        // set dirty bit to any entry that isn't marked as dirty, if the access mode is write
        if matches!(mode, AccessMode::Write) {
            let dirty_iter = [lvl1_entry_addr, lvl2_entry_addr]
                .into_iter()
                .zip(
                    lvl1_entry
                        .byte_with_dirty_flag
                        .into_iter()
                        .chain(lvl2_entry.byte_with_dirty_flag)
                );

            for (addr, undirty_byte) in dirty_iter {
                // set dirty flag
                let new_dirty_byte = undirty_byte | 0b0001_0000;
                // convert to byte because the dirty flag is in the first byte
                // and so we can ignore the rest
                self.mem.write(addr, new_dirty_byte as u8);
            }
        }

        let physical_address = ppn_shifted | page_offset;

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

    pub fn physical_read(&mut self, physical_addr: u32) -> OpRes<u8> {
        Ok(self.mem.read(physical_addr))
    }
    pub fn physical_write(&mut self, physical_addr: u32, val: u8) -> OpRes<()> {
        Ok(self.mem.write(physical_addr, val))
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
