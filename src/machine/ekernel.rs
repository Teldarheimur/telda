use crate::mem::MainMemory;

use super::Cpu;

pub trait EmulatedKernel<C: Cpu> {
    fn handle_trap(&mut self, tm: C::TrapMode, cpu: &mut C, mem: &mut dyn MainMemory) -> Result<(), C::TrapMode>;
}
