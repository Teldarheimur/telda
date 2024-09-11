use std::sync::mpsc::channel;
use std::thread;

use cables::Cables;
use softbuffer::SoftBufferError;
use winit::error::EventLoopError;
use winit::event_loop::EventLoop;
use winit_app::WinitApp;

use crate::machine::OffButton;

use self::card::FrameBuf;

pub const WIDTH: u32 = 320;
pub const HEIGHT: u32 = 240;

#[cfg(debug_assertions)]
const FACTOR: u32 = 2;
#[cfg(not(debug_assertions))]
const FACTOR: u32 = 4;

pub mod card;
pub mod cables;
mod winit_app;

pub enum Error {
    EventLoop(EventLoopError),
    SoftBuffer(SoftBufferError),
}
impl From<EventLoopError> for Error {
    fn from(e: EventLoopError) -> Self {
        Error::EventLoop(e)
    }
}
impl From<SoftBufferError> for Error {
    fn from(e: SoftBufferError) -> Self {
        Error::SoftBuffer(e)
    }
}

/// Starts a thread for other logic and runs the monitor code in the current thread
pub fn start_monitor<T: 'static + Send, F: 'static + Send + FnOnce(Cables) -> T>(logic_loop: F, power_off: OffButton) -> T {
    let (framebuf, monitor_cable) = FrameBuf::new();

    let (input_cable, keyboard_cable) = channel();

    let logic_thrd = thread::Builder::new()
        .name("main machine".to_owned())
        .spawn(move || logic_loop(Cables::new(monitor_cable, keyboard_cable)))
        .unwrap();

    {
        let app = WinitApp::new(input_cable, framebuf);
        let event_loop = EventLoop::new().unwrap();
        winit_app::run_app(event_loop, app);
    }

    power_off.press();

    logic_thrd.join().unwrap()
}

struct FpsRingBuffer {
    buffer: [f32; 16],
    pointer: usize,
}

impl FpsRingBuffer {
    pub fn new() -> Self {
        Self {
            buffer: [0.; 16],
            pointer: 1,
        }
    }
    pub fn add(&mut self, val: f32) {
        self.buffer[self.pointer] = val;
        self.pointer = (self.pointer + 1) % self.buffer.len();
    }
    pub fn avg(&self) -> f32 {
        self.buffer.iter().sum::<f32>() / self.buffer.len() as f32
    }
}
