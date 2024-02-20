use std::{io, path::PathBuf, process::ExitCode};

use clap::Parser;
use telda2::{
    aalv::obj::{
        Flags,
        Object,
        SegmentType,
        SymbolDefinition
    },
    align_end,
    align_start,
    blf4::{Blf4, TrapMode},
    machine::Machine,
    mem::{LazyMain, StdIo},
    PAGE_SIZE,
};

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

const PERM_X: u8 = 0b1000;
const PERM_W: u8 = 0b0100;
const PERM_R: u8 = 0b0010;

// TODO: URGENT port this to other binaries, test all telda binaries with this new system, ...

fn t_main() -> Result<(), Error> {
    let Cli {
        binary,
        termination_point,
    } = Cli::parse();

    let mut lazy = LazyMain::new(StdIo);

    let (symbols, start_addr, user_mode, virtual_mode) = {
        let obj = Object::from_file(binary).map_err(Error::IoError)?;
        let Flags {
            user_mode,
            virtual_mode,
            readable_text,
        } = obj.flags.unwrap_or_default();

        if virtual_mode {
            let mut mmbuilder = lazy.with_memory_mapping();
            for (seg, (offset, bytes)) in obj.segs {
                let mut heap = false;
                use self::SegmentType::*;
                let permissions = match seg {
                    Data => PERM_W | PERM_R,
                    RoData => PERM_R,
                    Text => if readable_text {
                        PERM_X | PERM_R
                    } else {
                        PERM_X
                    },
                    Heap => {
                        heap = true;
                        eprintln!("mapping heap");
                        PERM_R | PERM_W
                    }
                    // zero segment and unknown segments should have all permissions just in case
                    Zero | Unknown => PERM_X | PERM_W | PERM_R
                };

                mmbuilder.add_segment(permissions, offset, &bytes);
                if heap {
                    let heap = align_end(offset, PAGE_SIZE);
                    let size = obj.heap_size.unwrap_or_default().0 - bytes.len() as u16;
                    mmbuilder.map_wr_pages(heap, size, 0xff_0000 | heap as u32);
                }
            }

            
            eprintln!("mapping stack");
            // map space for I/O and stack
            let stack_size = obj.stack_size.unwrap_or_default().0;
            let stack_start = align_start(0xffff-stack_size.saturating_sub(1), PAGE_SIZE);
            mmbuilder.map_wr_pages(stack_start, stack_size, 0xff_0000 | stack_start as u32);
        } else {
            lazy = lazy.with_ff_seg(obj.get_flattened_memory());
        }

        let iter = obj.symbols.into_iter();

        (iter, obj.entry.ok_or(Error::NoEntry)?.1, user_mode, virtual_mode)
    };

    let mut cpu = Blf4::new(start_addr);

    if user_mode {
        cpu.flags.user_mode = true;
    }
    if virtual_mode {
        cpu.page = 0;
        cpu.flags.virtual_mode = true;
        cpu.context(&mut lazy).print_mmap();
    }

    let mut machine = Machine::new(lazy, cpu);
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
        eprintln!("{:#04x?}", machine.cpu);
        return Err(Error::Trap(tm));
    }

    Ok(())
}
