use std::io::{Read, Write, stdin, stdout};
use std::thread::spawn;
use std::sync::{Arc, atomic::{AtomicBool, Ordering}, mpsc::{sync_channel, TryRecvError}};

use byteorder::{LittleEndian, ByteOrder};

pub trait Memory<I> {
    const INDEX_WIDTH: I;

    fn read(&self, r: I) -> u8;
    fn read_index(&self, r: I) -> I;
    #[inline(always)]
    fn read_iter_from(&self, r: I) -> MemoryIter<Self, I> {
        MemoryIter {
            index: r,
            inner: &self
        }
    }
    fn write(&mut self, r: I, c: u8);
    fn write_index(&mut self, r: I, c: I);
    fn size(&self) -> usize;
}

pub struct MemoryIter<'a, T: 'a + Memory<I> + ?Sized, I> {
    pub index: I,
    pub inner: &'a T,
}

pub trait NextIndex<I> {
    fn next_index(&mut self) -> I;
}

macro_rules! impl_mem_iter {
    ($($t:ty)+) => {$(
        impl<'a, T: 'a + Memory<$t> + ?Sized> NextIndex<$t> for MemoryIter<'a, T, $t> {
            fn next_index(&mut self) -> $t {
                let item = self.inner.read_index(self.index);
                self.index += T::INDEX_WIDTH;
                item
            }
        }

        impl<'a, T: 'a + Memory<$t> + ?Sized> Iterator for MemoryIter<'a, T, $t> {
            type Item = u8;
            fn next(&mut self) -> Option<Self::Item> {
                let item = self.inner.read(self.index);
                self.index += 1;
                Some(item)
            }
        }
    )+};
}

impl_mem_iter! { u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 }

macro_rules! impl_prim {
    ($($collective_type:ident, $t:ty, $read_index:path, $write_index:path)*) => {
        $(
        pub type $collective_type = [u8; (1 << 8 * ::std::mem::size_of::<$t>())];
        impl Memory<$t> for $collective_type {
            const INDEX_WIDTH: $t = ::std::mem::size_of::<$t>() as $t;
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

impl_prim!{
    Memory16Bit, u16, TeldaEndian::read_u16, TeldaEndian::write_u16
    Memory32Bit, u32, TeldaEndian::read_u32, TeldaEndian::write_u32
}

pub trait Cpu {
    type Index;
    type Interrupt;

    /// Run for one cycle
    fn run<'a, M: Memory<Self::Index>>(&'a mut self, mem: &'a mut M) -> Option<Signal>;
    /// Whether the CPU is not currently blocking interrupts
    /// and is therefore ready to receive an interrupt
    fn ready_for_interrupt(&mut self) -> bool;
    /// Trigger the given interrupt
    fn trigger_interrupt<'a, M: Memory<Self::Index>>(&'a mut self, mem: &'a mut M, interrupt: Self::Interrupt);
    /// Read from the out port
    /// 
    /// This is probably garbage if `run` hasn't sent a `Signal::Read`
    fn read_out_port(&mut self) -> u8;
}

pub enum Signal {
    PowerOff,
    Restart,
    Read,
}

#[derive(Debug, Copy, Clone)]
pub enum MachineInterrupt {
    In(u8),
    Eof,
}

#[derive(Debug, Clone)]
pub struct Machine<I, M: Memory<I>, Cp: Cpu<Index = I>> {
    pub memory: M,
    pub cpu: Cp,
}

impl<I, M: Memory<I>, Cp: Cpu<Index = I, Interrupt = MachineInterrupt>> Machine<I, M, Cp> {
    pub fn new(memory: M, cpu: Cp) -> Self {
        Machine {
            memory,
            cpu
        }
    }
    pub fn run(&mut self) {
        let running = Arc::new(AtomicBool::new(true));
        let (tx, rx) = sync_channel(4);

        let thread_running = running.clone();

        let thr = spawn(move || {
            while thread_running.load(Ordering::SeqCst) {
                let mut buf = [0];

                match stdin().read(&mut buf) {
                    Ok(0) => tx.send(MachineInterrupt::Eof).unwrap(),
                    Ok(_) => tx.send(MachineInterrupt::In(buf[0])).unwrap(),
                    Err(e) => panic!("{:?}", e),
                }
            }
        });

        loop {
            if let Some(s) = self.cpu.run(&mut self.memory) {
                match s {
                    Signal::PowerOff => (),
                    Signal::Read => {
                        stdout().write_all(&[self.cpu.read_out_port()]).unwrap();
                    }
                    Signal::Restart => unimplemented!(),
                }
                break
            }

            match rx.try_recv() {
                Ok(int) => self.cpu.trigger_interrupt(&mut self.memory, int),
                Err(TryRecvError::Empty | TryRecvError::Disconnected) => (),
            }
        }

        running.store(false, Ordering::SeqCst);
        thr.join().unwrap();
    }
}

pub type TeldaEndian = LittleEndian;

pub mod standard16;
pub mod is;
pub mod lazy16;
pub mod wrappers;
pub mod tc;
pub mod teldasm;