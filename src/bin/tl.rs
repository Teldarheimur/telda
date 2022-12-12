use std::{path::PathBuf, collections::HashMap, io::Write, process::ExitCode, fs::{self, File}, os::unix::prelude::PermissionsExt};

use clap::Parser;
use telda2::{ext_files::{read_symbol_file, read_symbol_references, SYMBOL_FILE_EXT, NON_GLOBAL_SYMBOL_FILE_EXT, SYMBOL_REFERENCE_EXT}, source::Format};

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

    /// If defined, write the global symbols to this file
    #[arg(short, long, value_name = "FILE")]
    global_symbol_output: Option<PathBuf>,
    /// If defined, write the local symbols to this file
    ///
    /// Overlap can occur in this file, and they are not mangled
    #[arg(short, long, value_name = "FILE")]
    local_symbol_output: Option<PathBuf>,
}


fn main() -> ExitCode {
    let Cli {
        input_files,
        out,
        entry,
        global_symbol_output,
        local_symbol_output,
    } = Cli::parse();

    let out = out.unwrap_or_else(|| PathBuf::from("a.out"));

    let mut mem_out = Vec::new();
    let mut global_refs = Vec::new();
    let mut global_symbols = HashMap::new();
    // Vec because overlap is allowed
    let mut local_symbols = Vec::new();
    
    for input_file in input_files {
        let offset = mem_out.len() as u16;
        let mut mem_file = fs::read(&input_file).unwrap();
        
        let mut global_labels: HashMap<String, u16> = HashMap::new();
        let mut local_labels: HashMap<String, u16> = HashMap::new();
        read_symbol_file(input_file.with_extension(SYMBOL_FILE_EXT), &mut global_labels, &mut ()).unwrap();
        read_symbol_file(input_file.with_extension(NON_GLOBAL_SYMBOL_FILE_EXT), &mut local_labels, &mut ()).unwrap();

        let refs = read_symbol_references(input_file.with_extension(SYMBOL_REFERENCE_EXT)).unwrap();
        for (f, lbl, pos_in_file) in refs {
            let Some(&pos) = local_labels.get(&*lbl) else {
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

        for (lbl, pos_in_file) in local_labels {
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

    {
        let mut out_file = File::create(&out).unwrap();
        let executable = entry.is_some();
        if let Some(entry) = entry {
            let addr;
            if entry.starts_with("0x") {
                addr = u16::from_str_radix(&entry[2..], 16).unwrap();
            } else {
                if let Some(&pos) = global_symbols.get(&entry) {
                    addr = pos;
                } else {
                    eprintln!("Start symbol {entry} was not found. Perhaps it is not global?");
                    eprintln!("Aborting linking");
                    return ExitCode::FAILURE;
                }
            }

            println!("Writing executable binary telda file");

            writeln!(out_file, "#!/bin/env -S t -s -e 0x{addr:02x}").unwrap();
        } else {
            println!("Writing un-relocatable binary file");
        }
        out_file.write_all(&mut mem_out).unwrap();
        println!("Wrote to binary {}", out.display());
        drop(mem_out);

        if executable {
            let mut perms = out_file.metadata().unwrap().permissions();
            perms.set_mode(perms.mode() | 0o111);
            out_file.set_permissions(perms).unwrap();
        }
    }

    if let Some(path) = global_symbol_output {
        let mut f = File::create(&path).unwrap();
        for (lbl, loc) in global_symbols {
            writeln!(f, "{lbl}: 0x{loc:02X}").unwrap();
        }
        println!("Wrote global symbols to {}", path.display());
    }

    if let Some(path) = local_symbol_output {
        let mut f = File::create(&path).unwrap();
        for (lbl, loc) in local_symbols {
            writeln!(f, "{lbl}: 0x{loc:02X}").unwrap();
        }
        println!("Wrote local symbols to {}", path.display());
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
