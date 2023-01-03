use std::fmt::{self, Display};

use crate::U4;

pub const R0B: ByteRegister = ByteRegister(U4::new_unchecked(0));
pub const R1L: ByteRegister = ByteRegister(U4::new_unchecked(1));
pub const R1H: ByteRegister = ByteRegister(U4::new_unchecked(2));
pub const R2L: ByteRegister = ByteRegister(U4::new_unchecked(3));
pub const R2H: ByteRegister = ByteRegister(U4::new_unchecked(4));
pub const R3L: ByteRegister = ByteRegister(U4::new_unchecked(5));
pub const R3H: ByteRegister = ByteRegister(U4::new_unchecked(6));
pub const R4L: ByteRegister = ByteRegister(U4::new_unchecked(7));
pub const R4H: ByteRegister = ByteRegister(U4::new_unchecked(8));
pub const R5L: ByteRegister = ByteRegister(U4::new_unchecked(9));
pub const R5H: ByteRegister = ByteRegister(U4::new_unchecked(10));
pub const R6B: ByteRegister = ByteRegister(U4::new_unchecked(11));
pub const R7B: ByteRegister = ByteRegister(U4::new_unchecked(12));
pub const R8B: ByteRegister = ByteRegister(U4::new_unchecked(13));
pub const R9B: ByteRegister = ByteRegister(U4::new_unchecked(14));
pub const R10B: ByteRegister = ByteRegister(U4::new_unchecked(15));

pub const R0: WideRegister = WideRegister(U4::new_unchecked(0));
pub const R1: WideRegister = WideRegister(U4::new_unchecked(1));
pub const R2: WideRegister = WideRegister(U4::new_unchecked(2));
pub const R3: WideRegister = WideRegister(U4::new_unchecked(3));
pub const R4: WideRegister = WideRegister(U4::new_unchecked(4));
pub const R5: WideRegister = WideRegister(U4::new_unchecked(5));
pub const R6: WideRegister = WideRegister(U4::new_unchecked(6));
pub const R7: WideRegister = WideRegister(U4::new_unchecked(7));
pub const R8: WideRegister = WideRegister(U4::new_unchecked(8));
pub const R9: WideRegister = WideRegister(U4::new_unchecked(9));
pub const R10: WideRegister = WideRegister(U4::new_unchecked(10));
pub const RS: WideRegister = WideRegister(U4::new_unchecked(11));
pub const RL: WideRegister = WideRegister(U4::new_unchecked(12));
pub const RB: WideRegister = WideRegister(U4::new_unchecked(13));
pub const RP: WideRegister = WideRegister(U4::new_unchecked(14));
pub const RH: WideRegister = WideRegister(U4::new_unchecked(15));

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct ByteRegister(pub U4);
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct WideRegister(pub U4);

impl Display for ByteRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.into() {
            0 => write!(f, "r0b"),
            n @ 1..=10 => write!(f, "r{}{}", (n + 1) >> 1, if n & 1 == 0 { "h" } else { "l" }),
            n @ 11..=15 => write!(f, "r{}b", n + 6 - 11),
            _ => unreachable!("no such register")
        }
    }
}
impl Display for WideRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.into() {
            n @ 0..=10 => write!(f, "r{n}"),
            11 => write!(f, "rs"),
            12 => write!(f, "rl"),
            13 => write!(f, "rb"),
            14 => write!(f, "rp"),
            15 => write!(f, "rh"),
            _ => unreachable!("no such register")
        }
    }
}
