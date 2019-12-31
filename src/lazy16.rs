use crate::{Memory, TeldaEndian};
use std::io::{ErrorKind, Read, Result};
use std::fmt::{self, Debug};

use byteorder::ByteOrder;
use arr_macro::arr;

const DEFAULT_SLOT: [u8; 256] = [0; 256];

pub struct Lazy16Memory {
    slots: [Option<Box<[u8; 256]>>; 256],
}

impl Debug for Lazy16Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.slots.iter().map(|o| o.as_ref().map(|b| &b[..]))).finish()
    }
}

impl Lazy16Memory {
    #[inline(always)]
    pub fn new() -> Self {
        Lazy16Memory {
            slots: arr![None; 256],
        }
    }
    pub fn from_reader<R: Read>(mut r: R) -> Result<Self> {
        let mut mem = Self::new();

        'outer: for slot in &mut mem.slots[..] {
            *slot = Some(Box::new(DEFAULT_SLOT));

            {
                let mut buf = &mut slot.as_mut().unwrap()[..];

                while !buf.is_empty() {
                    match r.read(buf) {
                        Ok(0) => break 'outer,
                        Ok(n) => { let tmp = buf; buf = &mut tmp[n..]; }
                        Err(ref e) if e.kind() == ErrorKind::Interrupted => (),
                        Err(e) => return Err(e),
                    }
                }
            }
        }

        Ok(mem)
    }
    #[inline]
    fn convert_indices(&self, i: u16) -> (u8, u8) {
        let mut indices = [0; 2];
        TeldaEndian::write_u16(&mut indices, i);
        let [a, b] = indices;
        (a, b)
    }
    #[inline(always)]
    fn get_slot(&self, j: u8)  -> &[u8; 256] {
        unsafe {
            self.slots.get_unchecked(j as usize).as_ref()
        }.map(|s| &**s).unwrap_or(&DEFAULT_SLOT)
    }
    #[inline]
    fn get_mut_slot(&mut self, j: u8) -> &mut [u8; 256] {
        match  unsafe { self.slots.get_unchecked_mut(j as usize) } {
            Some(slot) => return &mut *slot,
            a => {
                let new_section = Box::new(DEFAULT_SLOT);
                *a = Some(new_section);

                a.as_mut().map(|a| &mut *a).unwrap()
            }
        }
    }
}

impl Memory<u16> for Lazy16Memory {
    const INDEX_WIDTH: u16 = 2;
    #[inline]
    fn read(&self, i: u16) -> u8 {
        let (i, j) = self.convert_indices(i);

        *unsafe {self.get_slot(j).get_unchecked(i as usize)}
    }
    #[inline]
    fn read_index(&self, i: u16) -> u16 {
        match self.convert_indices(i) {
            (0xff, j) => {
                let arr = [
                    unsafe { *self.get_slot(j).get_unchecked(255) },
                    unsafe { *self.get_slot(j+1).get_unchecked(0) },
                ];

                TeldaEndian::read_u16(&arr)
            }
            (i, j) => {
                let reference: &[u8] = unsafe { self.get_slot(j).get_unchecked(i as usize ..) };
                TeldaEndian::read_u16(reference)
            }
        }
    }
    #[inline]
    fn write(&mut self, i: u16, c: u8) {
        let (i, j) = self.convert_indices(i);

        *unsafe { self.get_mut_slot(j).get_unchecked_mut(i as usize) } = c;
    }
    #[inline]
    fn write_index(&mut self, i: u16, c: u16) {
        match self.convert_indices(i) {
            (0xff, j) => {
                let mut buf = [0; 2];
                TeldaEndian::write_u16(&mut buf, c);
                let [a, b] = buf;

                * unsafe {self.get_mut_slot(j).get_unchecked_mut(255)} = a;
                * unsafe {self.get_mut_slot(j+1).get_unchecked_mut(0)} = b;
            }
            (i, j) => {
                let reference: &mut [u8] = unsafe { self.get_mut_slot(j).get_unchecked_mut(i as usize ..) };
                TeldaEndian::write_u16(reference, c);
            }
        }
    }
    #[inline(always)]
    fn size(&self) -> usize {
        0x10000
    }
}