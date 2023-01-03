use std::{path::PathBuf, process::ExitCode};

use clap::Parser;
use telda2::{cpu::{Cpu, TrapMode}, aalv::obj::{Object, SymbolDefinition}, mem::Lazy};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Binary file
    binary: PathBuf,

    /// Whether the termination point should be displayed
    #[arg(short, long)]
    termination_point: bool,
}

pub fn main() -> ExitCode {
    let Cli { binary, termination_point } = Cli::parse();

    let (mem, symbols, start_addr) = {
        let obj = Object::from_file(binary).unwrap();
        let mem = obj.get_flattened_memory();

        let iter = obj.symbols.into_iter();

        (mem, iter, obj.entry.unwrap().0)
    };

    let mut lazy = Lazy::new_stdio(mem);

    let mut cpu = Cpu::new(start_addr);
    let tm = cpu.run_until_abort(&mut lazy);

    if termination_point {
        let pc = cpu.registers.program_counter;
        let mut diff = pc;
        let mut closest = "".into();
        for SymbolDefinition{name, location, ..} in symbols {
            if name.is_empty() { continue; }
            if pc >= location {
                let new_diff = pc - location;
                if new_diff < diff {
                    diff = new_diff;
                    closest = name;
                }
            }
        }
        println!("Ended with {tm:?} at <{closest}+{diff:02X}>");
    } else {
        if tm != TrapMode::Halt {
            eprintln!("trapped with {tm:?}")
        }
    }

    ExitCode::SUCCESS
}
