use std::sync::mpsc::{Receiver, Sender};

use crate::mem::Io;

use super::card::Draw;

#[derive(Debug)]
pub struct Cables {
    pub(super) monitor_cable: Sender<Draw>,
    pub(super) keyboard_cable: Receiver<u8>,

    x: u16,
    y: u16,
}

impl Cables {
    pub const fn new(monitor_cable: Sender<Draw>, keyboard_cable: Receiver<u8>) -> Self {
        Self {
            monitor_cable,
            keyboard_cable,
            x: 0,
            y: 0,
        }
    }
}

pub const KEYBOARD_ADDR: u8 = 0x10;
pub const MONITOR_X_ADDR: u8 = 0x12;
pub const MONITOR_Y_ADDR: u8 = 0x13;
pub const MONITOR_DRAW_C_ADDR: u8 = 0x14;

impl Io for Cables {
    fn read(&mut self, addr: u8) -> u8 {
        if addr == KEYBOARD_ADDR {
            self.keyboard_cable.recv().unwrap()
        } else {
            0
        }
    }
    fn write(&mut self, addr: u8, val: u8) {
        match addr {
            MONITOR_X_ADDR => {
                self.x <<= 8;
                self.x |= val as u16;
            }
            MONITOR_Y_ADDR => {
                self.y <<= 8;
                self.y |= val as u16;
            }
            MONITOR_DRAW_C_ADDR => {
                let _ = self.monitor_cable.send(Draw {
                    x: self.x,
                    y: self.y,
                    c: val,
                });
            }
            _ => (),
        }
    }
}