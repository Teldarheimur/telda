use std::io::{stdin, stdout, Read, Write};

/// Memory below this address is used for IO mapping
pub const IO_MAPPING_CUTOFF: u16 = 0xffe0;

/// A memory request handler
///
/// Addresses up to `IO_MAPPING_CUTOFF` should be guaranteed to stay consistent between reads and writes with no side-effects.
///
/// Addresses from `IO_MAPPING_CUTOFF` have no such guarantees and should be used to map to I/O (such as to peripherals or just STDIO).
pub trait Memory {
    fn read(&mut self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);

    fn read_wide(&mut self, addr: u16) -> u16 {
        let lower = self.read(addr);
        let higher = self.read(addr + 1);

        u16::from_le_bytes([lower, higher])
    }
    fn write_wide(&mut self, addr: u16, val: u16) {
        let [lower, higher] = val.to_le_bytes();

        self.write(addr, lower);
        self.write(addr + 1, higher);
    }
}

pub trait Io {
    fn read(&mut self, addr: u8) -> u8;
    fn write(&mut self, addr: u8, val: u8);
}

pub struct PanickingIO;
impl Io for PanickingIO {
    fn read(&mut self, _addr: u8) -> u8 {
        unimplemented!("I/O cannot not be used with this memory")
    }
    fn write(&mut self, addr: u8, _val: u8) {
        self.read(addr);
    }
}
pub struct StdIo;
impl Io for StdIo {
    fn read(&mut self, _addr: u8) -> u8 {
        // TODO: use the address
        let mut buf = [0];
        stdin().read_exact(&mut buf).expect("stdin failed");
        buf[0]
    }
    fn write(&mut self, _addr: u8, val: u8) {
        stdout().write_all(&[val]).expect("stdout failed")
    }
}

#[derive(Debug, Clone)]
pub struct Lazy<I> {
    pub mem: Vec<u8>,
    pub io: I,
}

impl Lazy<PanickingIO> {
    pub fn new_panicking(mem: Vec<u8>) -> Self {
        Self {
            mem,
            io: PanickingIO,
        }
    }
}
impl Lazy<StdIo> {
    pub fn new_stdio(mem: Vec<u8>) -> Self {
        Self { mem, io: StdIo }
    }
}

impl<I: Io> Memory for Lazy<I> {
    fn read(&mut self, addr: u16) -> u8 {
        if addr < IO_MAPPING_CUTOFF {
            self.mem.get(addr as usize).copied().unwrap_or(0)
        } else {
            self.io.read(addr as u8)
        }
    }
    fn write(&mut self, addr: u16, val: u8) {
        if addr < IO_MAPPING_CUTOFF {
            if self.mem.len() <= addr as usize {
                self.mem.resize(addr as usize + 1, 0);
            }
            self.mem[addr as usize] = val;
        } else {
            self.io.write(addr as u8, val);
        }
    }
}

impl Memory for [u8] {
    // No I/O
    fn read(&mut self, addr: u16) -> u8 {
        self[addr as usize]
    }
    // No I/O
    fn write(&mut self, addr: u16, val: u8) {
        self[addr as usize] = val;
    }
}
