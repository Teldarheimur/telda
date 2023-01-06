use std::{path::PathBuf, process::ExitCode, io};

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

enum Error {
    NoEntry,
    Trap(TrapMode),
    IoError(io::Error),
}

pub fn main() -> ExitCode {
    match t_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            match e {
                Error::NoEntry => eprintln!("no entry point in binary"),
                Error::Trap(tm) => eprintln!("trapped with {tm:?}"),
                Error::IoError(e) => eprintln!("unexpected io error occured: {e}"),
            }
            ExitCode::FAILURE
        }
    }
}

fn t_main() -> Result<(), Error> {
    let Cli { binary, termination_point } = Cli::parse();

    let (mem, symbols, start_addr) = {
        let obj = Object::from_file(binary).map_err(Error::IoError)?;
        let mem = obj.get_flattened_memory();

        let iter = obj.symbols.into_iter();

        (mem, iter, obj.entry.ok_or(Error::NoEntry)?.1)
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
    } else if tm != TrapMode::Halt {
        return Err(Error::Trap(tm));
    }

    Ok(())
}
