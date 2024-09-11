#![cfg(feature="monitor")]
use std::{
    fs::File,
    io::{self, Read},
    mem::replace,
    path::PathBuf,
    process::ExitCode,
};

use clap::Parser;
use telda2::{
    aalv::obj::{Object, SymbolDefinition, SymbolTable},
    blf4::{Blf4, TrapMode},
    machine::{Machine, OffButton},
    mem::LazyMain, monitor::{self, cables::Cables},
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Binary file
    ///
    /// By default this is an object file that will be loaded in as a user program with memory mapping
    binary: PathBuf,

    /// If set, the binary is interpreted as raw binary data rather than an object file and is loaded in at 0x00_0080 (ROM)
    ///
    /// No emulated kernel will be present and the cpu will go through normal startup,
    // the CPU will start execution at 0x0080 in direct where the binary is loaded
    #[arg(short, long)]
    raw_binary: bool,

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
    let off_button = OffButton::new();
    let off_button_clone = off_button.clone();
    match monitor::start_monitor(move |cables| t_main(cables, off_button), off_button_clone) {
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

fn t_main(cables: Cables, off_button: OffButton) -> Result<(), Error> {
    let Cli {
        binary,
        raw_binary,
        termination_point,
    } = Cli::parse();

    let mut machine = Machine::new_with_off_button(LazyMain::new(cables), Blf4::new(), off_button);

    let mut symbols = SymbolTable::default();
    if raw_binary {
        let mut file = File::open(binary).map_err(Error::IoError)?;
        let mut raw_binary_data = Vec::new();
        file.read_to_end(&mut raw_binary_data)
            .map_err(Error::IoError)?;

        machine.memory = machine.memory.with_rom(&raw_binary_data);
    } else {
        let mut obj = Object::from_file(binary).map_err(Error::IoError)?;
        // error if there is no entry
        obj.entry.is_some().then_some(()).ok_or(Error::NoEntry)?;
        symbols = replace(&mut obj.symbols, symbols);
        machine.load_user_binary(&obj);
    }
    let symbols = symbols.into_iter();

    let tm = machine.run_until_abort();

    if termination_point {
        let pc = machine.cpu.program_counter;
        let mut diff = pc;
        let mut closest = "".into();
        for SymbolDefinition { name, location, .. } in symbols {
            if name.is_empty() {
                continue;
            }
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
