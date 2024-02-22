use crate::mem::MainMemory;

mod ekernel;
pub use self::ekernel::*;

pub trait Cpu {
    type TrapMode;

    fn execute_instruction<M: MainMemory>(&mut self, main_memory: &mut M) -> Result<(), Self::TrapMode>;
}

pub struct Machine<M, C> {
    pub memory: M,
    pub cpu: C,

    ekernel: Option<Box<dyn EmulatedKernel<C>>>,
}

impl<M, C> Machine<M, C> {
    pub fn new(memory: M, cpu: C) -> Self {
        Machine { memory, cpu, ekernel: None }
    }
}

impl<M: MainMemory, C: Cpu> Machine<M, C> {
    /// Installs an emulated kernel that handles traps
    ///
    /// Returns true if one was already installed
    pub fn install_emulated_kernel<K: EmulatedKernel<C> + 'static>(&mut self, ek: K) -> bool {
        let installed_alreday = self.ekernel.is_some();
        self.ekernel = Some(Box::new(ek));
        installed_alreday
    }
    pub fn execute_once(&mut self) -> Result<(), C::TrapMode> {
        match self.cpu.execute_instruction(&mut self.memory) {
            Ok(()) => Ok(()),
            Err(tm) => {
                let Some(k) = self.ekernel.as_deref_mut() else {
                    return Err(tm);
                };

                // handle trap with emulated kernel if one was installed
                k.handle_trap(tm, &mut self.cpu, &mut self.memory)
            }
        }
    }
    /// Until unhandled trap
    pub fn run_until_abort(&mut self) -> C::TrapMode {
        loop {
            match self.execute_once() {
                Ok(()) => (),
                Err(tm) => break tm,
            }
        }
    }
}
