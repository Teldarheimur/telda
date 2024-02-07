use std::io::{stdin, stdout, Read, Write};

/// Memory below this address is used for IO mapping
pub const VIRTUAL_IO_MAPPING_CUTOFF: u16 = 0xfff0;
pub const MAIN_IO_MAPPING_CUTOFF: u32 = 0xfffff0;

pub trait MainMemory {
    fn read(&mut self, addr: u32) -> u8;
    fn write(&mut self, addr: u32, byte: u8);
}

#[derive(Debug, Clone)]
pub struct LazyMain<P> {
    cells: [Option<Box<[u8; 256*256]>>; 256],
    ports: P,
}

impl<P: Io> MainMemory for LazyMain<P> {
    fn read(&mut self, addr: u32) -> u8 {
        if addr >= MAIN_IO_MAPPING_CUTOFF {
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
        if addr >= MAIN_IO_MAPPING_CUTOFF {
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
    fn new(ports: P) -> Self {
        Self {
            cells: ([(); 256]).map(|()| None),
            ports,
        }
    }
    /// Loads `flat_bytes` into memory at 0xff_0000 (where direct mode addresses)
    pub fn new_with(ports: P, flat_bytes: Vec<u8>) -> Self {
        let mut new = Self::new(ports);

        let mut new_cell = Box::new([0; 256*256]);
        // if `flat_bytes` is bigger that 64K, this will panic
        new_cell[..flat_bytes.len()].copy_from_slice(&flat_bytes);
        new.cells[255] = Some(new_cell);

        new
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
