use std::{path::PathBuf, collections::HashMap, process::ExitCode, fs::{self}, os::unix::prelude::PermissionsExt};

use clap::Parser;
use telda2::{source::Format, aalv::obj::{Object, GlobalSymbols, InternalSymbols}, mem::Lazy};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Input binary files (with symbol and relocation table files implied)
    input_files: Vec<PathBuf>,

    /// Sets the output path, otherwise a.out is used
    #[arg(short, long, value_name = "FILE")]
    out: Option<PathBuf>,
    
    /// Defines an entry-point and makes this output an executable binary
    #[arg(short, long)]
    entry: Option<String>,
    
    /// Includes internal symbols and potentially other debug info to the object
    #[arg(short = 'g', long)]
    debug: bool,
}


fn main() -> ExitCode {
    let Cli {
        input_files,
        out,
        entry,
        debug
    } = Cli::parse();

    let out = out.unwrap_or_else(|| PathBuf::from("a.to"));

    let mut mem_out = Vec::new();
    let mut global_refs = Vec::new();
    let mut global_symbols = HashMap::new();
    // Vec because overlap is allowed
    let mut local_symbols = Vec::new();
    
    for input_file in input_files {
        let offset = mem_out.len() as u16;

        let mut mem_file;
        let global_labels: HashMap<_, _>;
        let internal_labels: HashMap<_, _>;
        let refs;
        {
            let obj = Object::from_file(&input_file).unwrap();
            mem_file = obj.mem.unwrap().mem;

            internal_labels = obj.internal_symbols.map(|is| is.0.into_iter()).into_iter().flatten().collect();
            global_labels = obj.global_symbols.map(|is| is.0.into_iter()).into_iter().flatten().collect();
            refs = obj.symbol_reference_table.map(|srt| srt.0).unwrap_or(Vec::new());
        }

        for (f, lbl, pos_in_file) in refs {
            let Some(&pos) = internal_labels.get(&*lbl) else {
                global_refs.push((f, lbl, pos_in_file+offset));
                continue;
            };

            rewrite(&mut mem_file, pos_in_file, f, pos+offset);
        }

        for (lbl, pos_in_file) in global_labels {
            let previous_symbol = global_symbols.insert(lbl.clone(), pos_in_file+offset);
            if let Some(loc) = previous_symbol {
                eprintln!("global symbol {lbl} defined in {} but was already defined in a previous file at location 0x{loc:02x}", input_file.display());

                return ExitCode::FAILURE;
            }
        }

        for (lbl, pos_in_file) in internal_labels {
            local_symbols.push((lbl, pos_in_file));
        }

        mem_out.extend(mem_file);
    }

    let mut undefined_references = false;

    for (f, lbl, pos) in global_refs {
        let Some(&new_value) = global_symbols.get(&*lbl) else {
            eprintln!("undefined reference to {lbl} at 0x{pos:02x}");
            undefined_references = true;
            continue;
        };

        rewrite(&mut mem_out, pos, f, new_value);
    }

    if undefined_references {
        return ExitCode::FAILURE;
    }

    let start_addr = match entry {
        Some(entry) => Some({
            if entry.starts_with("0x") {
                u16::from_str_radix(&entry[2..], 16).unwrap()
            } else {
                if let Some(&pos) = global_symbols.get(&*entry) {
                    pos
                } else {
                    eprintln!("Start symbol {entry} was not found. Perhaps it is not global?");
                    eprintln!("Aborting linking");
                    return ExitCode::FAILURE;
                }
            }
        }),
        None => None,
    };

    let obj = {
        let mut obj = Object::default();

        obj.mem = Some(Lazy { mem: mem_out });
        obj.global_symbols = Some(GlobalSymbols(global_symbols.into_iter().collect()));
        if debug {
            obj.internal_symbols = Some(InternalSymbols(local_symbols));
        }
        obj
    };

    if let Some(addr) = start_addr {
        println!("Writing executable binary telda file");

        obj.write_to_file_with_shebang(&out, format!("#!/bin/env -S t -s -e 0x{addr:02x}")).unwrap();

        let mut perms = fs::metadata(&out).unwrap().permissions();
        perms.set_mode(perms.mode() | 0o111);
        fs::set_permissions(out, perms).unwrap();
    } else {
        println!("Writing non-executable binary telda file");
        obj.write_to_file(out).unwrap();
    }

    ExitCode::SUCCESS
}

fn rewrite(v: &mut Vec<u8>, pos_in_v: u16, f: Format, new_value: u16) {
    let arr = match f {
        Format::Absolute => new_value.to_le_bytes(),
        Format::Big => {
            if new_value == 0 {
                0
            } else {
                // Since this w is a number from 1 up to 65527, we can just add 7 to encode it between 0x08 and 0xffff
                new_value.checked_add(7).expect("new immediate was greater than 65527 in a big slot")
            }.to_le_bytes()
        }
    };
    v[pos_in_v as usize .. pos_in_v as usize + 2].copy_from_slice(&arr);
}
