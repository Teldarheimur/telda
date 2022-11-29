#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TrapMode {
    Halt = 0,
    Invalid = 1,
    ZeroDiv = 2,
}

pub struct Registers {
    a: u16,
    b: u16,
    x: u16,
    y: u16,

    pub(crate) pc: u16,
    pub(crate) sp: u16,
    pub(crate) trap: bool,
    pub(crate) trap_mode: TrapMode,
    pub(crate) zero: bool,
    pub(crate) sign: bool,
    pub(crate) overflow: bool,
    pub(crate) carry: bool,
}

impl Registers {
    pub fn read_al(&self) -> u8 {
        self.a as u8
    }
    pub fn read_ar(&self) -> u8 {
        (self.a >> 8) as u8
    }
    pub fn read_bl(&self) -> u8 {
        self.b as u8
    }
    pub fn write_al(&mut self, val: u8) {
        self.a = val as u16 | (self.a & 0xff00);
    }
    pub fn write_ar(&mut self, val: u8) {
        self.a = ((val as u16) << 8) | (self.a & 0x00ff);
    }
    pub fn write_bl(&mut self, val: u8) {
        self.a = val as u16 | (self.a & 0xff00);
    }
    pub fn read(&self, r: u8) -> u16 {
        match r {
            0 => 0,
            1 => self.read_al() as u16,
            2 => self.read_ar() as u16,
            3 => self.read_bl() as u16,
            4 => self.a,
            5 => self.b,
            6 => self.x,
            7 => self.y,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn write(&mut self, r: u8, val: u16) {
        match r {
            0 => (),
            1 => self.write_al(val as u8),
            2 => self.write_ar(val as u8),
            3 => self.write_bl(val as u8),
            4 => self.a = val,
            5 => self.b = val,
            6 => self.x = val,
            7 => self.y = val,
            _ => unimplemented!("no such register"),
        }
    }
    pub fn trap(&mut self, trap_mode: TrapMode) {
        self.trap = true;
        self.trap_mode = trap_mode;
    }
}
