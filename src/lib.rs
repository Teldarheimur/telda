use std::marker::PhantomData;

pub trait Memory<I> {
    type Cell;

    fn read(&self, r: I) -> Self::Cell;
    fn read_index(&self, r: I) -> I;
    fn write(&mut self, r: I, c: Self::Cell);
    fn write_index(&mut self, r: I, c: I);
    fn size(&self) -> usize;
}

pub trait ArrayMemory<I>: Memory<I> {
    fn slice(&self) -> &[<Self as Memory<I>>::Cell];
}

pub struct SmartMemory<I, M: Memory<I>> {
    buffer: M,
    length: usize,
    _phantom: PhantomData<I>,
}

impl<I, M: Memory<I>> SmartMemory<I, M> {
    pub fn new(buffer: M) -> Self {
        SmartMemory {
            buffer,
            length: 0,
            _phantom: PhantomData,
        }
    }
}

impl<I: Copy, M: Memory<I>> Memory<I> for SmartMemory<I, M>
where usize: From<I> {
    type Cell = M::Cell;

    #[inline]
    fn read(&self, r: I) -> Self::Cell {
        self.buffer.read(r)
    }
    #[inline]
    fn read_index(&self, r: I) -> I {
        self.buffer.read_index(r)
    }
    #[inline]
    fn write(&mut self, r: I, c: Self::Cell) {
        let new_length = usize::from(r) + 1;
        if new_length > self.length {
            self.length = new_length;
        }
        self.buffer.write(r, c)
    }
    #[inline]
    fn write_index(&mut self, r: I, c: I) {
        let new_length = usize::from(r) + 3;
        if new_length > self.length {
            self.length = new_length;
        }
        self.buffer.write_index(r, c)
    }
    #[inline]
    fn size(&self) -> usize {
        self.buffer.size()
    }
}

impl<I: Copy, M: Memory<I> + ArrayMemory<I>> ArrayMemory<I> for SmartMemory<I, M>
where usize: From<I> {
    #[inline]
    fn slice(&self) -> &[Self::Cell] {
        &self.buffer.slice()[..self.length]
    }
}

macro_rules! impl_prim {
    ($($collective_type:ident, $t:ty, $n:expr,)*) => {
        $(
        pub type $collective_type = [u8; $n];
        impl<T: Copy> Memory<$t> for [T; $n] {
            type Cell = T;

            #[inline]
            fn read(&self, i: $t) -> Self::Cell {
                * unsafe { self.get_unchecked(i as usize) }
            }
            #[inline]
            fn read_index(&self, i: $t) -> $t {
                let reference: &Self::Cell = unsafe { self.get_unchecked(i as usize) };
                * unsafe {
                    let index_reference: &$t = std::mem::transmute(reference);
                    index_reference
                }
            }
            #[inline]
            fn write(&mut self, i: $t, c: Self::Cell) {
                * unsafe { self.get_unchecked_mut(i as usize) } = c;
            }
            #[inline]
            fn write_index(&mut self, i: $t, c: $t) {
                let reference: &mut Self::Cell = unsafe { self.get_unchecked_mut(i as usize) };
                unsafe {
                    let index_reference: &mut $t = std::mem::transmute(reference);
                    *index_reference = c;
                }
            }
            #[inline]
            fn size(&self) -> usize {
                self.len()
            }
        }
        impl<T: Copy> ArrayMemory<$t> for [T; $n] {
            #[inline(always)]
            fn slice(&self) -> &[Self::Cell] {
                self
            }
        }
        )*
    };
}

impl_prim!{
    Memory8Bit, u8, 256,
    Memory16Bit, u16, 0x10000,
    Memory32Bit, u32, 0x100000000,
}

pub trait Cpu {
    type Cell;
    type Index;

    fn run<M: Memory<Self::Index, Cell = Self::Cell>>(&mut self, mem: &mut M) -> Option<Signal>;
}

pub enum Signal {
    PowerOff,
    Restart,
}

pub struct Machine<C, I, M: Memory<I, Cell = C>, Cp: Cpu<Cell = C, Index = I>> {
    pub memory: M,
    pub cpu: Cp,
}

impl<C, I, M: Memory<I, Cell = C>, Cp: Cpu<Cell = C, Index = I>> Machine<C, I, M, Cp> {
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

pub mod standard8;
pub mod standard16;