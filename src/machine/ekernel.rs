use crate::{blf4::clock_counter::Clocker, mem::MainMemory};

use super::Cpu;

pub trait EmulatedKernel<C: Cpu> {
    fn handle_trap(
        &mut self,
        tm: C::TrapMode,
        cpu: &mut C,
        mem: &mut dyn MainMemory,
        clocker: &mut dyn Clocker,
    ) -> Result<(), C::TrapMode>;
}
