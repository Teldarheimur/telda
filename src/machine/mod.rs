use crate::{blf4::clock_counter::{ClockCounter, Clocker, Cycles}, mem::MainMemory};

mod ekernel;
pub use self::ekernel::*;

pub trait Cpu {
    type TrapMode;

    fn execute_instruction<M: MainMemory, C: Clocker>(
        &mut self,
        main_memory: &mut M,
        clocker: &mut C,
    ) -> Result<(), Self::TrapMode>;
}

pub struct Machine<M, C> {
    pub memory: M,
    pub cpu: C,

    clock_speed: Option<f32>,
    clocker: ClockCounter,
    ekernel: Option<Box<dyn EmulatedKernel<C>>>,
}

impl<M, C> Machine<M, C> {
    pub fn new_with_clockspeed(memory: M, cpu: C, mhz: f32) -> Self {
        assert!(mhz > 0., "clockspeed should be a meaningful value");
        Machine {
            memory,
            cpu,
            clocker: ClockCounter::new(),
            clock_speed: Some(mhz),
            ekernel: None,
        }
    }
    pub fn new_unlimited(memory: M, cpu: C) -> Self {
        Machine {
            memory,
            cpu,
            clocker: ClockCounter::new(),
            clock_speed: None,
            ekernel: None,
        }
    }
    pub fn cycle_count(self) -> Cycles {
        self.clocker.cycle_count()
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
        match self.cpu.execute_instruction(&mut self.memory, &mut self.clocker) {
            Ok(()) => {
                if self.memory.should_catchup() {
                    self.clocker.catch_up(self.clock_speed);
                }
                Ok(())
            }
            Err(tm) => {
                // Catch up every time we trap, because that's when something interesting happens
                let Some(k) = self.ekernel.as_deref_mut() else {
                    return Err(tm);
                };

                // The emulated kernel might do smth interesting
                self.clocker.catch_up(self.clock_speed);
                // handle trap with emulated kernel if one was installed
                k.handle_trap(tm, &mut self.cpu, &mut self.memory, &mut self.clocker)
            }
        }
    }
    /// Until unhandled trap
    pub fn run_until_abort(&mut self) -> C::TrapMode {
        self.clocker.catch_up(self.clock_speed);
        loop {
            match self.execute_once() {
                Ok(()) => (),
                Err(tm) => break tm,
            }
        }
    }
}
