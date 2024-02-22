use crate::{
    aalv::obj::{Flags, Object, SegmentType},
    align_end,
    align_start,
    mem::MainMemory,
    machine::Machine,
    PAGE_SIZE,
};

use super::{super::{Blf4, PERM_R, PERM_W, PERM_X}, EKernel};

impl<M: MainMemory> Machine<M, Blf4> {
    pub fn load_user_binary(&mut self, obj: &Object) {
        let mut ekernel = EKernel::new();

        let page_table1 = ekernel.allocate_page(&mut self.memory);
        assert_eq!(page_table1 as u16 as u32, page_table1, "page_table1 needs to be within block 0");

        let mut mmbuilder = ekernel.mmapper(&mut self.memory, page_table1);

        let Flags {
            readable_text,
        } = obj.flags.unwrap_or_default();
        let heap_size = obj.heap_size.unwrap_or_default().0;
        let stack_size = obj.stack_size.unwrap_or_default().0;

        for (&seg, &(offset, ref bytes)) in &obj.segs {
            let mut heap = false;
            use self::SegmentType::*;
            let permissions = match seg {
                Data => PERM_W | PERM_R,
                RoData => PERM_R,
                Text => if readable_text {
                    PERM_X | PERM_R
                } else {
                    PERM_X
                },
                Heap => {
                    heap = true;
                    PERM_R | PERM_W
                }
                // zero segment and unknown segments should have all permissions just in case
                Zero | Unknown => PERM_X | PERM_W | PERM_R
            };

            mmbuilder.add_segment(permissions, offset, bytes);
            if heap {
                let heap = align_end(offset, PAGE_SIZE);
                let size = heap_size - bytes.len() as u16;
                mmbuilder.map_wr_pages(heap, size);
            }
        }

        // map space for stack
        let stack_start = align_start(0xffff-stack_size.saturating_sub(1), PAGE_SIZE);
        mmbuilder.map_wr_pages(stack_start, stack_size);

        self.cpu.flags.virtual_mode = true;
        self.cpu.flags.user_mode = true;
        self.cpu.stack = 0xffff;
        self.cpu.frame = 0xffff;
        if let Some(entry) = obj.entry {
            self.cpu.program_counter = entry.1;
        }
        self.cpu.page = page_table1 as u16;
        self.install_emulated_kernel(ekernel);
    }
}
