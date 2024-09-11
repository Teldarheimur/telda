pub mod aalv;
pub mod blf4;
pub mod disassemble;
pub mod machine;
pub mod mem;
pub mod source;
pub mod u4;
#[cfg(feature="monitor")]
pub mod monitor;

pub use self::u4::U4;

/// The size of a page
pub const PAGE_SIZE: u16 = 128;
pub const PAGE_SIZE_P: u32 = PAGE_SIZE as u32;

pub const fn align_start(addr: u16, alignment: u16) -> u16 {
    let mask = !(alignment - 1);
    addr & mask
}
pub const fn align_end(addr: u16, alignment: u16) -> u16 {
    let mask = alignment - 1;
    let next_aligned = (addr + alignment) & !mask;

    // If the current end is already aligned, do nothing
    if addr & mask == 0 {
        addr
    } else {
        next_aligned
    }
}

#[test]
fn test_align() {
    assert_eq!(align_end(0x124, PAGE_SIZE), 0x180);
    assert_eq!(align_end(0x180, PAGE_SIZE), 0x180);
    assert_eq!(align_end(0x000, PAGE_SIZE), 0x000);
    assert_eq!(align_end(0x479, PAGE_SIZE), 0x480);
    assert_eq!(align_end(0x401, PAGE_SIZE), 0x480);
    assert_eq!(align_end(0x63f, PAGE_SIZE), 0x680);
    assert_eq!(align_end(0x684, PAGE_SIZE), 0x700);

    assert_eq!(align_start(0x124, PAGE_SIZE), 0x100);
    assert_eq!(align_start(0x180, PAGE_SIZE), 0x180);
    assert_eq!(align_start(0x000, PAGE_SIZE), 0x000);
    assert_eq!(align_start(0x479, PAGE_SIZE), 0x400);
    assert_eq!(align_start(0x401, PAGE_SIZE), 0x400);
    assert_eq!(align_start(0x63f, PAGE_SIZE), 0x600);
    assert_eq!(align_start(0x684, PAGE_SIZE), 0x680);
}
