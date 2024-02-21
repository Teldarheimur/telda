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

#[derive(Debug, Clone)]
pub struct LazyMain<P> {
    cells: [Option<Box<[u8; 256*256]>>; 256],
    ports: P,
}

impl<P: Io> MainMemory for LazyMain<P> {
    fn read(&mut self, addr: u32) -> u8 {
        if addr < PAGE_SIZE_P {
            return self.ports.read((addr & 0xf) as u8);
        }

        let cell_index = (addr >> 16) as usize;
        if let Some(cell) = &self.cells[cell_index] {
            let index = (addr & 0xffff) as usize;
            cell[index]
        } else {
            0
        }
    }
    fn write(&mut self, addr: u32, byte: u8) {
        if addr < PAGE_SIZE_P {
            self.ports.write((addr & 0xf) as u8, byte);
            return;
        }

        let cell_index = (addr >> 16) as usize;
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
            cells: ([(); 256]).map(|()| None),
            ports,
        }
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
