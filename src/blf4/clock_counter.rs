use std::time::{Duration, Instant};

pub type Cycles = u32;
pub const MEM_READ: Cycles = 10;
pub const MEM_WRITE: Cycles = 10;

pub struct IdleMarker<'a> {
    clocker: &'a mut dyn Clocker,
}
impl<'a> IdleMarker<'a> {
    pub(crate) fn make(clocker: &'a mut dyn Clocker) -> Self {
        Self {
            clocker,
        }
    }
    pub(crate) fn start_idle(&mut self) -> Idle {
        self.clocker.start_idle()
    }
    pub(crate) fn stop_idle(&mut self, idle: Idle) {
        self.clocker.stop_idle(idle);
    }
}

#[must_use]
pub struct Idle {
    time: Instant,
}

pub trait Clocker {
    fn cycle(&mut self, times: Cycles);
    /// Indicate that the CPU is idling waiting for a signal to continue,
    /// eg. when waiting for input on a port. Must call `stop_idle` with the handler afterwards
    fn start_idle(&mut self) -> Idle;
    /// Called with an idle from `start_idle` to indicate that the CPU is no longer idling
    fn stop_idle(&mut self, idle: Idle);
}

#[derive(Debug, Clone, Copy)]
pub struct NothingClocker;

impl Clocker for NothingClocker {
    fn cycle(&mut self, _: Cycles) {}
    fn start_idle(&mut self) -> Idle {
        Idle { time: Instant::now() }
    }
    fn stop_idle(&mut self, _: Idle) {}
}

#[derive(Debug, Clone)]
pub struct ClockCounter {
    cycles: Cycles,
    last_catchup: (Instant, Cycles),
}
impl ClockCounter {
    pub fn new() -> Self {
        Self {
            cycles: 0,
            last_catchup: (Instant::now(), 0),
        }
    }
    pub fn catch_up(&mut self, clock_speed_mhz: Option<f32>) {
        let time_now = Instant::now();
        if let Some(clock_speed_mhz) = clock_speed_mhz {
            let (last_time, last_cycles) = self.last_catchup;
            let cycle_diff = self.cycles - last_cycles;
            const MEGA: f32 = 1_000_000.;

            let diff_time = Duration::from_secs_f32(cycle_diff as f32 / MEGA / clock_speed_mhz);
            let expected_cur_time = last_time + diff_time;
            if expected_cur_time > time_now {
                // if we're ahead of the expected time of the elapsed clocks, we need to sleep to simulate the time it takes for given
                // clock speed to do those cycles
                let sleep_time = expected_cur_time - time_now;
                #[cfg(feature = "debug_sleeps")]
                eprintln!("Need to sleep for {}µs to catch up with {cycle_diff} cycles", sleep_time.as_micros());
                std::thread::sleep(sleep_time);
                self.last_catchup = (expected_cur_time, self.cycles);
            }
        } else {
            self.last_catchup = (time_now, self.cycles);
        }
    }
    
    pub fn cycle_count(self) -> Cycles {
        self.cycles
    }
}

impl Clocker for ClockCounter {
    fn cycle(&mut self, times: Cycles) {
        self.cycles += times;
    }
    fn start_idle(&mut self) -> Idle {
        Idle { time: Instant::now() }
    }
    fn stop_idle(&mut self, Idle{time}: Idle) {
        let jump = Instant::now() - time;
        #[cfg(feature = "debug_sleeps")]
        eprintln!("Jumping {jump:#?} from idle");
        self.last_catchup.0 += jump;
    }
}

