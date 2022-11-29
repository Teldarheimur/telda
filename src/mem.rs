pub trait Memory {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);
}

pub struct Lazy {
    pub mem: Vec<u8>,
}

impl Memory for Lazy {
    fn read(&self, addr: u16) -> u8 {
        self.mem.get(addr as usize).copied().unwrap_or(0)
    }
    fn write(&mut self, addr: u16, val: u8) {
        if self.mem.len() < addr as usize {
            self.mem.resize(addr as usize, 0);
        }
        self.mem[addr as usize] = val;
    }
}
