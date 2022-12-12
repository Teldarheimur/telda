use std::{path::PathBuf, process::ExitCode, fs::{self, File}, io::{BufReader, BufRead, Read}};

use clap::Parser;
use telda2::{cpu::{Cpu, TrapMode}, mem::Lazy};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Binary file
    binary: PathBuf,

    /// Whether there is a shebang header to skip
    #[arg(short, long)]
    skip_shebang: bool,
    
    /// Sets the entry-point for this execution
    #[arg(short, long)]
    entry: String,
}

pub fn main() -> ExitCode {
    let Cli { binary, skip_shebang, entry } = Cli::parse();

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

    let mut mem = Vec::new();
    {
        if skip_shebang {
            let f = File::open(binary).unwrap();
            let mut br = BufReader::new(f);
            let mut s = String::new();
            br.read_line(&mut s).unwrap();
            drop(s);

            br.read_to_end(&mut mem).unwrap();
        } else {
            mem = fs::read(binary).unwrap();
        }
    }

    let mut cpu = Cpu::new(start_addr);
    let tm = cpu.run_until_trap(&mut Lazy { mem });

    if tm != TrapMode::Halt {
        eprintln!("trapped with {tm:?}")
    }

    ExitCode::SUCCESS
}
