use byteorder::{LittleEndian, ByteOrder};

pub trait Memory<I> {
    fn read(&self, r: I) -> u8;
    fn read_index(&self, r: I) -> I;
    fn read_to_slice(&self, r: I, length: I) -> &[u8];
    fn write(&mut self, r: I, c: u8);
    fn write_index(&mut self, r: I, c: I);
    fn size(&self) -> usize;
}

macro_rules! impl_prim {
    ($($collective_type:ident, $t:ty, $n:expr, $read_index:path, $write_index:path)*) => {
        $(
        pub type $collective_type = [u8; $n];
        impl Memory<$t> for [u8; $n] {
            #[inline]
            fn read(&self, i: $t) -> u8 {
                * unsafe { self.get_unchecked(i as usize) }
            }
            #[inline]
            fn read_index(&self, i: $t) -> $t {
                let reference: &[u8] = unsafe { self.get_unchecked(i as usize ..) };
                $read_index(reference)
            }
            #[inline]
            fn read_to_slice(&self, r: $t, length: $t) -> &[u8] {
                &self[r as usize..(r+length) as usize]
            }
            #[inline]
            fn write(&mut self, i: $t, c: u8) {
                * unsafe { self.get_unchecked_mut(i as usize) } = c;
            }
            #[inline]
            fn write_index(&mut self, i: $t, c: $t) {
                let reference: &mut [u8] = unsafe { self.get_unchecked_mut(i as usize ..) };
                $write_index(reference, c);
            }
            #[inline]
            fn size(&self) -> usize {
                self.len()
            }
        }
        )*
    };
}

#[inline(always)]
fn first(b: &[u8]) -> u8 {
    *unsafe{b.get_unchecked(0)}
}
#[inline(always)]
fn replace_first(b: &mut [u8], n: u8) {
    unsafe{*b.get_unchecked_mut(0) = n};
}

impl_prim!{
    Memory8Bit, u8, 256, self::first, self::replace_first
    Memory16Bit, u16, 0x10000, TeldaEndian::read_u16, TeldaEndian::write_u16
    Memory32Bit, u32, 0x100000000, TeldaEndian::read_u32, TeldaEndian::write_u32
}

pub trait Cpu {
    type Index;

    fn run<M: Memory<Self::Index>>(&mut self, mem: &mut M) -> Option<Signal>;
}

pub enum Signal {
    PowerOff,
    Restart,
}

pub struct Machine<I, M: Memory<I>, Cp: Cpu<Index = I>> {
    pub memory: M,
    pub cpu: Cp,
}

impl<I, M: Memory<I>, Cp: Cpu<Index = I>> Machine<I, M, Cp> {
    pub fn new(memory: M, cpu: Cp) -> Self {
        Machine {
            memory,
            cpu
        }
    }
    pub fn run(&mut self) {
        let signal = loop {
            if let Some(s) = self.cpu.run(&mut self.memory) {
                break s;
            }
        };

        match signal {
            Signal::PowerOff => (),
            Signal::Restart => unimplemented!(),
        }
    }
}

pub type TeldaEndian = LittleEndian;

pub mod standard8;
pub mod standard16;
pub mod is;