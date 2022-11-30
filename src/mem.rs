pub trait Memory {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);

    fn read_wide(&self, addr: u16) -> u16 {
        let lower = self.read(addr);
        let higher = self.read(addr+1);

        u16::from_le_bytes([lower, higher])
    }
    fn write_wide(&mut self, addr: u16, val: u16) {
        let [lower, higher] = val.to_le_bytes();

        self.write(addr, lower);
        self.write(addr+1, higher);
    }
}

pub struct Lazy {
    pub mem: Vec<u8>,
}

impl Memory for Lazy {
    fn read(&self, addr: u16) -> u8 {
        self.mem.get(addr as usize).copied().unwrap_or(0)
    }
    fn write(&mut self, addr: u16, val: u8) {
        if self.mem.len() <= addr as usize {
            self.mem.resize(addr as usize + 1, 0);
        }
        self.mem[addr as usize] = val;
    }
}

impl Memory for [u8] {
    fn read(&self, addr: u16) -> u8 {
        self[addr as usize]
    }
    fn write(&mut self, addr: u16, val: u8) {
        self[addr as usize] = val;
    }
}
