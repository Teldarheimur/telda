use std::io::{stdin, stdout, Read, Write};

use crate::{align_start, PAGE_SIZE};

/// Memory below this address is used for IO mapping
pub const VIRTUAL_IO_MAPPING_CUTOFF: u16 = 0xfff0;
pub const MAIN_IO_MAPPING_CUTOFF: u32 = 0xfffff0;

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
    pub fn new(ports: P) -> Self {
        Self {
            cells: ([(); 256]).map(|()| None),
            ports,
        }
    }
    /// Loads `flat_bytes` into memory at 0xff_0000 (where direct mode addresses)
    pub fn with_ff_seg(mut self, flat_bytes: Vec<u8>) -> Self {
        let mut new_cell = Box::new([0; 256*256]);
        // if `flat_bytes` is bigger that 64K, this will panic
        new_cell[..flat_bytes.len()].copy_from_slice(&flat_bytes);
        self.cells[255] = Some(new_cell);

        self
    }
    /// Write page tables for segments
    pub fn with_memory_mapping(&mut self) -> MmapBuilder<P> where P: Io {
        assert_eq!(self.cells[0], None, "cell 0 should have been written to yet");
        MmapBuilder {
            next_page: PAGE_SIZE as u32,
            inner: self,
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

pub struct MmapBuilder<'a, P> {
    // page table 1 is at 0
    next_page: u32,
    inner: &'a mut LazyMain<P>,
}

impl<P: Io> MmapBuilder<'_, P> {
    pub fn add_segment(&mut self, permissions: u8, offset: u16, bytes: &[u8]) {
        // 0bUD_XWRP
        assert_eq!(permissions & 0b1100_0000, 0, "reserved bits should not be set");
        // set user and present bit (they might've been set by the caller too)
        let perm_bits = permissions | 0b10_0001;

        // map all pages the segment overlaps with
        let range = align_start(offset, PAGE_SIZE) .. offset+bytes.len() as u16;
        for p in range.step_by(PAGE_SIZE as usize) {
            self.map(p, perm_bits, 0xff_0000 | p as u32);
        }

        write_n(self.inner, 0xff_0000 | offset as u32, &bytes);
    }
    fn map(&mut self, virt: u16, flags_byte: u8, paddr: u32) {
        assert_eq!(virt & (PAGE_SIZE - 1), 0, "virtual address should be page aligned");
        assert_eq!(flags_byte & 1, 1, "page should be present");
        assert_eq!(flags_byte & 0xc0, 0, "reserved flag bits should be 0");
        assert_eq!(paddr & (PAGE_SIZE as u32 - 1), 0, "physical address should be page aligned");
        let vpn1 = virt >> 12;
        let vpn2 = (virt >> 7) & 0b1_1111;

        let pte1_addr = (vpn1 << 2) as u32;
        let mut pte1 = u32::from_le_bytes(read_n(self.inner, pte1_addr));
        if pte1 & 1 == 0 {
            // entry not present, allocate a page for table
            let page = self.new_page();
            pte1 = (page << 8) | (flags_byte as u32);

            write_n(self.inner, pte1_addr, &pte1.to_le_bytes());
        } else {
            let perm_byte = pte1 as u8;
            if perm_byte & flags_byte != flags_byte {
                self.inner.write(pte1_addr, perm_byte | flags_byte);
            }
        }
        // clear reserved bits, although they should be zero already
        let pte2_addr = ((pte1 >> 8) & 0xff_ff80) + (vpn2 << 2) as u32;
        let mut pte2 = u32::from_le_bytes(read_n(self.inner, pte2_addr));
        if pte2 & 1 == 0 {
            // entry not present, write it
            pte2 = (paddr<<8) | (flags_byte as u32);

            write_n(self.inner, pte2_addr, &pte2.to_le_bytes());
        } else {
            assert_eq!((pte2>>8) & 0xff_ff80, paddr, "remapping of existing mapping should have same address");
            let perm_byte = pte2 as u8;
            if perm_byte & flags_byte != flags_byte {
                // add extra permissions
                self.inner.write(pte2_addr, perm_byte | flags_byte);
            }
        }
    }
    #[inline]
    pub fn map_wr_pages(&mut self, virt: u16, size: u16, paddr: u32) {
        assert_eq!(virt & (PAGE_SIZE - 1), 0, "virtual address should be page aligned");
        assert_eq!(paddr & (PAGE_SIZE as u32 - 1), 0, "physical address should be page aligned");
        for p in (virt.. virt.saturating_add(size)).step_by(PAGE_SIZE as usize) {
            self.map(p, 0b11_0111, paddr | p as u32);
        }
    }
    fn new_page(&mut self) -> u32 {
        let next_page = self.next_page;
        self.next_page += PAGE_SIZE as u32;

        next_page
    }
}
