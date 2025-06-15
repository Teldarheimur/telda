use std::time::{Duration, Instant};

pub type Cycles = u32;
pub const MEM_READ: Cycles = 10;
pub const MEM_WRITE: Cycles = 2;
pub const SYSCALL_COST: Cycles = 20;

pub trait Clocker {
    fn cycle(&mut self, times: Cycles);
}

#[derive(Debug, Clone, Copy)]
pub struct NothingClocker;

impl Clocker for NothingClocker {
    #[inline]
    fn cycle(&mut self, _: Cycles) {}
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
}

