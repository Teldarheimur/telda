use crate::mem::MainMemory;

pub trait Cpu {
    type TrapMode;

    fn execute_instruction<M: MainMemory>(&mut self, main_memory: &mut M) -> Result<(), Self::TrapMode>;
}

pub struct Machine<M, C> {
    pub memory: M,
    pub cpu: C,
}

impl<M, C> Machine<M, C> {
    pub fn new(memory: M, cpu: C) -> Self {
        Machine { memory, cpu }
    }
}

impl<M: MainMemory, C: Cpu> Machine<M, C> {
    /// Until unhandled trap
    pub fn run_until_abort(&mut self) -> C::TrapMode {
        loop {
            match self.cpu.execute_instruction(&mut self.memory) {
                Ok(()) => (),
                Err(tm) => break tm,
            }
        }
    }
}
