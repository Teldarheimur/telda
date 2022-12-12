use std::{path::PathBuf, process::ExitCode};

use clap::Parser;
use telda2::{cpu::{Cpu, TrapMode}, aalv::obj::Object};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Binary file
    binary: PathBuf,

    /// Whether there is a shebang header to skip
    #[arg(short, long)]
    skip_shebang: bool,

    /// Whether there is a shebang header to skip
    #[arg(short, long)]
    termination_point: bool,
    
    /// Sets the entry-point for this execution
    #[arg(short, long)]
    entry: String,
}

pub fn main() -> ExitCode {
    let Cli { binary, skip_shebang, entry, termination_point } = Cli::parse();

    let start_addr;
    loop {
        if entry.starts_with("0x") {
            if let Ok(p) = u16::from_str_radix(&entry[2..], 16) {
                start_addr = p;
                break;
            }
        }
        eprintln!("Invalid entry-point; needs to be a hexadecimal 16-bit value prefixed by 0x");
        eprintln!("{entry:?}");

        return ExitCode::FAILURE;
    }

    let (mut lazy, symbols) = {
        let obj;
        if skip_shebang {
            let (_, o) = Object::from_file_ignoring_shebang(binary).unwrap();
            obj = o;
        } else {
            obj = Object::from_file(binary).unwrap();
        }

        let iter = obj.global_symbols
            .map(|is| is.0.into_iter())
            .into_iter()
            .flatten()
            .chain(obj.internal_symbols.map(|is| is.0.into_iter()).into_iter().flatten());

        (obj.mem.unwrap(), iter)
    };

    let mut cpu = Cpu::new(start_addr);
    let tm = cpu.run_until_trap(&mut lazy);

    if termination_point {
        let pc = cpu.registers.pc;
        let mut diff = pc;
        let mut closest = "".into();
        for (lbl, loc) in symbols {
            if pc >= loc {
                let new_diff = pc - loc;
                if new_diff < diff {
                    diff = new_diff;
                    closest = lbl;
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
