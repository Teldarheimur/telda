use crate::{
    align_start, machine::EmulatedKernel, mem::{read_n, write_n, MainMemory, HALF_CELL}, PAGE_SIZE, PAGE_SIZE_P
};
#[cfg(feature = "monitor")]
use crate::monitor::cables::{MONITOR_DRAW_C_ADDR, MONITOR_X_ADDR, MONITOR_Y_ADDR};
#[cfg(feature = "monitor")]
use super::{R2H, R3H, R4L};

use super::{Blf4, TrapMode, R1, R1L, R2, R2L, R3L};

mod load_user_binary;

/// Standard emulated kernel
///
/// syscall R1=15 to set error handler with R2 as address
/// error handler cannot return and should either halt or run a new program
pub struct EKernel {
    error_handler: u16,
    page_bumper: u32,
    next_free: Option<u32>,
}

impl EKernel {
    pub fn new() -> Self {
        Self {
            error_handler: 0,
            page_bumper: HALF_CELL as u32,
            next_free: None,
        }
    }
    fn mmapper<'a, M: ?Sized + MainMemory>(
        &'a mut self,
        mem: &'a mut M,
        page_table1: u32,
    ) -> MmapBuilder<M> {
        MmapBuilder {
            page_table1,
            kernel: self,
            mem,
        }
    }
    pub fn allocate_page<M: ?Sized + MainMemory>(&mut self, mem: &mut M) -> u32 {
        // Use page_bumper if free list is empty
        if let Some(next_free) = self.next_free.take() {
            let ret = next_free;
            let next_free = u32::from_le_bytes(read_n(mem, ret));
            // The special value `1` means the free list is empty
            if next_free != 1 {
                self.next_free = Some(next_free);
            }

            write_n(mem, ret, &[0; 128]);

            ret
        } else {
            let ret = self.page_bumper;
            // TODO: check if this overflows indicating that the bumper is empty
            // on debug mode it'll panic rn, but release mode will wrap around which would lead to weird behaviour
            self.page_bumper += PAGE_SIZE_P;
            ret
        }
    }
    #[allow(dead_code)]
    pub fn free_page<M: ?Sized + MainMemory>(&mut self, addr: u32, mem: &mut M) {
        assert_eq!(addr & (PAGE_SIZE_P - 1), 0, "page addr should be aligned");
        let old_free;
        if let Some(next_free) = self.next_free.take() {
            old_free = next_free;
        } else {
            old_free = 1;
        }
        // Point to the last head of the free list
        write_n(mem, addr, &old_free.to_le_bytes());

        // Put freed page at top of free list
        self.next_free = Some(addr);
    }
}

impl EmulatedKernel<Blf4> for EKernel {
    fn handle_trap(
        &mut self,
        tm: TrapMode,
        cpu: &mut Blf4,
        mem: &mut dyn MainMemory,
    ) -> Result<(), TrapMode> {
        match tm {
            TrapMode::SysCall => {
                let mut ctx = cpu.context(mem);
                let sys_n = ctx.cpu.read_wr(R1)?;

                match sys_n {
                    // show mmap (various debug syscalls)
                    0 => {
                        ctx.print_mmap();
                    }
                    #[cfg(feature = "monitor")]
                    // setp
                    2 => {
                        let xh = ctx.cpu.read_br(R2H);
                        let xl = ctx.cpu.read_br(R2L);
                        let yh = ctx.cpu.read_br(R3H);
                        let yl = ctx.cpu.read_br(R3L);
                        let c = ctx.cpu.read_br(R4L);
                        ctx.physical_write(MONITOR_X_ADDR as u32, xh).unwrap();
                        ctx.physical_write(MONITOR_X_ADDR as u32, xl).unwrap();
                        ctx.physical_write(MONITOR_Y_ADDR as u32, yh).unwrap();
                        ctx.physical_write(MONITOR_Y_ADDR as u32, yl).unwrap();
                        ctx.physical_write(MONITOR_DRAW_C_ADDR as u32, c).unwrap();
                    }
                    // cin
                    3 => {
                        let b = ctx.physical_read(1)?;
                        ctx.cpu.write_br(R1L, b);
                    }
                    // cout
                    4 => {
                        let b = ctx.cpu.read_br(R2L);
                        ctx.physical_write(1, b)?;
                    }
                    // TODO: use a better mechanism for these
                    // reserve page
                    5 => {
                        let addr = ctx.cpu.read_wr(R2).unwrap();
                        let flags = ctx.cpu.read_br(R3L);
                        let mut mmapper = self.mmapper(&mut *ctx.mem, ctx.cpu.page as u32);
                        let phys_addr = mmapper.map(addr, flags);
                        ctx.cpu.write_wr(R1, phys_addr as u16).unwrap();
                        ctx.cpu.write_wr(R2, (phys_addr >> 16) as u16).unwrap();
                    }
                    // free page
                    6 => {
                        let addr = ctx.cpu.read_wr(R2).unwrap();
                        let mut mmapper = self.mmapper(&mut *ctx.mem, ctx.cpu.page as u32);
                        mmapper.unmap(addr);
                    }
                    // error handler vector
                    15 => {
                        self.error_handler = ctx.cpu.read_wr(R2)?;
                    }
                    _ => return Err(TrapMode::SysCall),
                }
            }
            TrapMode::Halt => return Err(TrapMode::Halt),
            TrapMode::PowerOff => return Err(TrapMode::PowerOff),
            e if self.error_handler != 0 => {
                cpu.write_wr(R1, e as u8 as u16)?;
                cpu.program_counter = self.error_handler;
            }
            e => return Err(e),
        }

        cpu.flags.trap = false;
        cpu.flags.user_mode = true;
        Ok(())
    }
}

pub struct MmapBuilder<'a, M: ?Sized + MainMemory> {
    page_table1: u32,
    kernel: &'a mut EKernel,
    mem: &'a mut M,
}

impl<M: ?Sized + MainMemory> MmapBuilder<'_, M> {
    pub fn add_segment(&mut self, permissions: u8, offset: u16, bytes: &[u8]) {
        // 0bUD_XWRP
        assert_eq!(permissions & 0b1100_0000, 0, "reserved bits should not be set");
        // set user and present bit (they might've been set by the caller too)
        let perm_bits = permissions | 0b10_0001;

        // let mut bytes = bytes;
        // map all pages the segment overlaps with
        let start_vpage = align_start(offset, PAGE_SIZE);
        let range = start_vpage..offset + bytes.len() as u16;
        for vaddr in range.step_by(PAGE_SIZE as usize) {
            let paddr = self.map(vaddr, perm_bits);

            // write page to memory
            if vaddr < offset {
                let page_offset = offset - vaddr;
                let end = ((PAGE_SIZE - page_offset) as usize).min(bytes.len());
                write_n(self.mem, paddr + page_offset as u32, &bytes[..end]);
            } else {
                let start = (vaddr - offset) as usize;
                let end = (start + PAGE_SIZE as usize).min(bytes.len());
                write_n(self.mem, paddr, &bytes[start..end]);
            }
        }
    }
    fn unmap(&mut self, virt: u16) {
        // TODO: check permissions and fail if page already doesn't exist
        assert_eq!(virt & (PAGE_SIZE - 1), 0, "address should be page aligned");
        let vpn1 = virt >> 12;
        let vpn2 = (virt >> 7) & 0b1_1111;

        let pte1_addr = self.page_table1 + (vpn1 << 2) as u32;
        let pte1 = u32::from_le_bytes(read_n(self.mem, pte1_addr));
        if pte1 & 1 == 0 {
            // entry not present, bad unmap
            panic!("Tried to unmap non-existant virtual page");
        } else {
            let perm_byte = pte1 as u8;
            let _ = perm_byte;
        }
        // clear reserved bits, although they should be zero already
        let pte2_addr = ((pte1 >> 8) & 0xff_ff80) + (vpn2 << 2) as u32;
        let pte2 = u32::from_le_bytes(read_n(self.mem, pte2_addr));
        if pte2 & 1 == 0 {
            // entry not present, bad unmap
            panic!("Tried to unmap non-existant virtual page");
        } else {
            let perm_byte = pte2 as u8;
            if perm_byte & 1 == 0 {
                panic!("Tried to unnmap non-existant virtual page");
            } else {
                // clear the page
                self.mem.write(pte2_addr, 0);
                let paddr = (pte2 >> 8) & 0xff_ff80;

                self.release_page(paddr);
            }
        }
    }
    #[must_use]
    fn map(&mut self, virt: u16, flags_byte: u8) -> u32 {
        assert_eq!(virt & (PAGE_SIZE - 1), 0, "virtual address should be page aligned");
        assert_eq!(flags_byte & 1, 1, "page should be present");
        assert_eq!(flags_byte & 0xc0, 0, "reserved flag bits should be 0");
        let vpn1 = virt >> 12;
        let vpn2 = (virt >> 7) & 0b1_1111;

        let pte1_addr = self.page_table1 + (vpn1 << 2) as u32;
        let mut pte1 = u32::from_le_bytes(read_n(self.mem, pte1_addr));
        if pte1 & 1 == 0 {
            // entry not present, allocate a page for table
            let page = self.new_page();
            pte1 = (page << 8) | (flags_byte as u32);

            write_n(self.mem, pte1_addr, &pte1.to_le_bytes());
        } else {
            let perm_byte = pte1 as u8;
            if perm_byte & flags_byte != flags_byte {
                self.mem.write(pte1_addr, perm_byte | flags_byte);
            }
        }
        // clear reserved bits, although they should be zero already
        let pte2_addr = ((pte1 >> 8) & 0xff_ff80) + (vpn2 << 2) as u32;
        let mut pte2 = u32::from_le_bytes(read_n(self.mem, pte2_addr));
        if pte2 & 1 == 0 {
            let paddr = self.new_page();
            // entry not present, write it
            pte2 = (paddr << 8) | (flags_byte as u32);

            write_n(self.mem, pte2_addr, &pte2.to_le_bytes());
            paddr
        } else {
            let perm_byte = pte2 as u8;
            if perm_byte & flags_byte != flags_byte {
                // add extra permissions
                self.mem.write(pte2_addr, perm_byte | flags_byte);
            }

            (pte2 >> 8) & 0xff_ff80
        }
    }
    #[inline]
    pub fn map_wr_pages(&mut self, virt: u16, size: u16) {
        assert_eq!(virt & (PAGE_SIZE - 1), 0, "virtual address should be page aligned");
        for p in (virt.. virt.saturating_add(size)).step_by(PAGE_SIZE as usize) {
            let _ = self.map(p, 0b11_0111);
        }
    }
    #[inline]
    fn new_page(&mut self) -> u32 {
        self.kernel.allocate_page(self.mem)
    }
    #[inline]
    fn release_page(&mut self, addr: u32) {
        self.kernel.free_page(addr, self.mem)
    }
}
