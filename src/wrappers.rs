use std::cmp::{Ordering, Eq, PartialEq, PartialOrd, Ord};
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug, Display};

#[derive(Copy, Clone)]
pub struct SplittableWord {
    inner: InnerSplittableWord
}

#[derive(Copy, Clone)]
union InnerSplittableWord {
    word: u16,
    bytes: (u8, u8)
}

impl From<u16> for SplittableWord {
    #[inline(always)]
    fn from(word: u16) -> SplittableWord {
        SplittableWord {
            inner: InnerSplittableWord {
                word,
            }
        }
    }
}
impl From<(u8, u8)> for SplittableWord {
    #[inline(always)]
    #[cfg(target_endian = "little")]
    fn from(bytes: (u8, u8)) -> SplittableWord {
        SplittableWord {
            inner: InnerSplittableWord {
                bytes,
            }
        }
    }
    #[inline(always)]
    #[cfg(target_endian = "big")]
    fn from((a, b): (u8, u8)) -> SplittableWord {
        SplittableWord {
            inner: InnerSplittableWord {
                bytes: (b, a),
            }
        }
    }
}

impl From<SplittableWord> for u16 {
    #[inline(always)]
    fn from(sw: SplittableWord) -> u16 {
        unsafe { sw.inner.word }
    }
}
impl From<SplittableWord> for (u8, u8) {
    #[inline(always)]
    #[cfg(target_endian = "little")]
    fn from(sw: SplittableWord) -> (u8, u8) {
        unsafe { sw.inner.bytes }
    }
    #[inline(always)]
    #[cfg(target_endian = "big")]
    fn from(sw: SplittableWord) -> (u8, u8) {
        let (a, b) = unsafe { sw.inner.bytes };
        (b, a)
    }
}


#[cfg(target_endian = "little")]
impl SplittableWord {
    /// Ignores anything about endianness, assumes they're ordered in little endian
    #[inline(always)]
    pub fn from_raw(bytes: (u8, u8)) -> Self {
        SplittableWord {
            inner: InnerSplittableWord {
                bytes
            }
        }
    }
    #[inline(always)]
    pub fn left(&self) -> &u8 {
        unsafe {&self.inner.bytes.0}
    }
    #[inline(always)]
    pub fn left_mut(&mut self) -> &mut u8 {
        unsafe {&mut self.inner.bytes.0}
    }
    #[inline(always)]
    pub fn right(&self) -> &u8 {
        unsafe {&self.inner.bytes.1}
    }
    #[inline(always)]
    pub fn right_mut(&mut self) -> &mut u8 {
        unsafe {&mut self.inner.bytes.1}
    }
}
#[cfg(target_endian = "big")]
impl SplittableWord {
    #[inline(always)]
    pub fn left(&self) -> &u8 {
        unsafe {&self.inner.bytes.1}
    }
    #[inline(always)]
    pub fn left_mut(&mut self) -> &mut u8 {
        unsafe {&mut self.inner.bytes.1}
    }
    #[inline(always)]
    pub fn right(&self) -> &u8 {
        unsafe {&self.inner.bytes.0}
    }
    #[inline(always)]
    pub fn right_mut(&mut self) -> &mut u8 {
        unsafe {&mut self.inner.bytes.0}
    }
}

impl Debug for SplittableWord {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(unsafe { &self.inner.word }, f)
    }
}
impl Display for SplittableWord {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(unsafe { &self.inner.word }, f)
    }
}

impl Borrow<u16> for SplittableWord {
    #[inline(always)]
    fn borrow(&self) -> &u16 {
        unsafe { &self.inner.word }
    }
}
impl BorrowMut<u16> for SplittableWord {
    #[inline(always)]
    fn borrow_mut(&mut self) -> &mut u16 {
        unsafe { &mut self.inner.word }
    }
}
impl Borrow<u8> for SplittableWord {
    #[inline(always)]
    fn borrow(&self) -> &u8 {
        self.left()
    }
}
impl BorrowMut<u8> for SplittableWord {
    #[inline(always)]
    fn borrow_mut(&mut self) -> &mut u8 {
        self.left_mut()
    }
}

impl PartialEq for SplittableWord {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            self.inner.word.eq(&other.inner.word)
        }
    }
}

impl Eq for SplittableWord { }

impl PartialOrd for SplittableWord {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unsafe {
            self.inner.word.partial_cmp(&other.inner.word)
        }
    }

    #[inline(always)]
    fn lt(&self, other: &Self) -> bool {
        unsafe {
            self.inner.word.lt(&other.inner.word)
        }
    }
    #[inline(always)]
    fn le(&self, other: &Self) -> bool {
        unsafe {
            self.inner.word.le(&other.inner.word)
        }
    }
    #[inline(always)]
    fn gt(&self, other: &Self) -> bool {
        unsafe {
            self.inner.word.gt(&other.inner.word)
        }
    }
    #[inline(always)]
    fn ge(&self, other: &Self) -> bool {
        unsafe {
            self.inner.word.ge(&other.inner.word)
        }
    }
}

impl Ord for SplittableWord {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        unsafe {
            self.inner.word.cmp(&other.inner.word)
        }
    }

    #[inline(always)]
    fn max(self, other: Self) -> Self {
        SplittableWord {
            inner: InnerSplittableWord {
                word: unsafe {
                    self.inner.word.max(other.inner.word)
                }
            }
        }
    }
    #[inline(always)]
    fn min(self, other: Self) -> Self {
        SplittableWord {
            inner: InnerSplittableWord {
                word: unsafe {
                    self.inner.word.min(other.inner.word)
                }
            }
        }
    }
}
