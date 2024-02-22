use std::io::{stdin, stdout, Read, Write};

use crate::PAGE_SIZE_P;

pub trait MainMemory {
    fn read(&mut self, addr: u32) -> u8;
    fn write(&mut self, addr: u32, byte: u8);
}

pub fn read_n<M: MainMemory + ?Sized, const N: usize>(m: &mut M, addr: u32) -> [u8; N] {
    std::array::from_fn(|i| {
        m.read(addr+i as u32)
    })
}
pub fn write_n<M: MainMemory + ?Sized>(m: &mut M, addr: u32, data: &[u8]) {
    data.into_iter()
        .enumerate()
        .for_each(|(i, &b)| {
            m.write(addr+i as u32, b)
        });
}

pub const ROM_SIZE: usize = HALF_CELL - PAGE_SIZE_P as usize;
pub const HALF_CELL: usize = 0x01_0000 / 2;

#[derive(Debug, Clone)]
pub struct LazyMain<P> {
    rom: Option<[u8; ROM_SIZE]>,
    ram0: [u8; HALF_CELL],
    cells: [Option<Box<[u8; 256*256]>>; 255],
    ports: P,
}

impl<P: Io> MainMemory for LazyMain<P> {
    fn read(&mut self, addr: u32) -> u8 {
        let cell_index = (addr >> 16) as usize;
        if cell_index != 0 {
            let Some(cell) = &self.cells[cell_index-1] else {
                return 0;
            };

            let index = (addr & 0xffff) as usize;
            cell[index]
        } else if addr < PAGE_SIZE_P {
            self.ports.read(addr as u8)
        } else if addr < HALF_CELL as u32 {
            self.rom
                .map(|a| a[(addr - PAGE_SIZE_P) as usize])
                .unwrap_or(0)
        } else {
            self.ram0[addr as usize - HALF_CELL]
        }
    }
    fn write(&mut self, addr: u32, byte: u8) {
        if addr < PAGE_SIZE_P {
            self.ports.write(addr as u8, byte);
            return;
        } else if addr < HALF_CELL as u32 {
            // ROM, cannot write (error?)
            return;
        } else if addr < 0x01_0000 {
            self.ram0[addr as usize - HALF_CELL] = byte;
            return;
        }

        let cell_index = (addr >> 16) as usize - 1;
        let index = (addr & 0xffff) as usize;
        match &mut self.cells[cell_index] {
            Some(cell) => cell[index] = byte,
            opt @ None => {
                let mut cell = Box::new([0; 256*256]);
                cell[index] = byte;
                *opt = Some(cell);
            }
        }
    }
}

impl<P> LazyMain<P> {
    pub fn new(ports: P) -> Self {
        Self {
            rom: None,
            ram0: [0; HALF_CELL],
            ports,
            cells: ([(); 255]).map(|()| None),
        }
    }
    pub fn with_rom(mut self, bytes: &[u8]) -> Self {
        assert!(bytes.len() <= ROM_SIZE, "bytes cannot be bigger than ROM");
        self.rom = Some(
            std::array::from_fn(
                |i| bytes[i] 
            )
        );
        self
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
