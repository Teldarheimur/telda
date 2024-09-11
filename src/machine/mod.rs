use std::sync::{atomic::{AtomicBool, Ordering}, Arc};

use crate::mem::MainMemory;

mod ekernel;
pub use self::ekernel::*;

pub trait Cpu {
    type TrapMode;
    const TRAP_MODE_OFF: Self::TrapMode;

    fn execute_instruction<M: MainMemory>(
        &mut self,
        main_memory: &mut M,
    ) -> Result<(), Self::TrapMode>;
}

pub struct Machine<M, C> {
    pub memory: M,
    pub cpu: C,

    ekernel: Option<Box<dyn EmulatedKernel<C>>>,
    off_button: Option<OffButton>,
}

impl<M, C> Machine<M, C> {
    pub fn new(memory: M, cpu: C) -> Self {
        Machine {
            memory,
            cpu,
            ekernel: None,
            off_button: None,
        }
    }
    pub fn new_with_off_button(memory: M, cpu: C, off_button: OffButton) -> Self {
        Machine {
            memory,
            cpu,
            ekernel: None,
            off_button: Some(off_button),
        }
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
        if let Some(off_button) = &self.off_button {
            if off_button.is_pressed() {
                off_button.unpress();
                return Err(C::TRAP_MODE_OFF);
            }
        }
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

#[derive(Debug, Clone)]
pub struct OffButton {
    pressed: Arc<AtomicBool>,
}

impl OffButton {
   pub fn new() -> Self {
        Self {
            pressed: Arc::new(AtomicBool::new(false))
        }
    }
    #[inline]
    pub fn press(&self) {
        self.pressed.store(true, Ordering::Relaxed);
    }
    #[inline]
    fn unpress(&self) {
        self.pressed.store(false, Ordering::Relaxed);
    }
    #[inline]
    pub fn is_pressed(&self) -> bool {
        self.pressed.load(Ordering::Relaxed)
    }
}