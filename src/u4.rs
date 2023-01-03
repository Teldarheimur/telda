#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct U4(u8);

impl U4 {
    pub const ZERO: Self = Self::new_unchecked(0);
    pub fn new(n: u8) -> Self {
        if n <= 0xf {
            U4(n)
        } else {
            panic!("value was too big for u4")
        }
    }
    pub const fn new_unchecked(n: u8) -> Self {
        debug_assert!(n <= 0xf);
        Self(n)
    }
    pub const fn paired(n: u8) -> (U4, U4) {
        (Self::new_unchecked(n >> 4), Self::new_unchecked(n & 0xf))
    }
    pub const fn pair(self, other: Self) -> u8 {
        (self.0 << 4) | other.0
    }
}

impl From<U4> for u8 {
    fn from(U4(n): U4) -> Self {
        n
    }
}
