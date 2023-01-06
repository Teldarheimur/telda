pub mod aalv;
pub mod cpu;
pub mod disassemble;
pub mod isa;
pub mod mem;
pub mod source;
pub mod u4;

pub use self::u4::U4;

pub const SEGMENT_ALIGNMENT: u16 = 0x04;

pub const fn align(addr: u16, alignment: u16) -> u16 {
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
    assert_eq!(align(0x124, SEGMENT_ALIGNMENT), 0x130);
    assert_eq!(align(0x120, SEGMENT_ALIGNMENT), 0x120);
    assert_eq!(align(0x000, SEGMENT_ALIGNMENT), 0x000);
    assert_eq!(align(0x411, SEGMENT_ALIGNMENT), 0x420);
    assert_eq!(align(0x456, SEGMENT_ALIGNMENT), 0x460);
    assert_eq!(align(0x63f, SEGMENT_ALIGNMENT), 0x640);
    assert_eq!(align(0x630, SEGMENT_ALIGNMENT), 0x630);
}
