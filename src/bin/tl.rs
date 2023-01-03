use std::{
    collections::{BTreeMap, HashMap},
    fs::{self, File},
    io::{Seek, Write},
    os::unix::prelude::PermissionsExt,
    path::PathBuf,
    process::ExitCode,
};

use clap::Parser;
use telda2::{
    aalv::obj::{
        Entry, Object, RelocationEntry, RelocationTable, SegmentType,
        SymbolDefinition, SymbolTable,
    },
    align, SEGMENT_ALIGNMENT,
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Input telda object files
    input_files: Vec<PathBuf>,

    /// Sets the output path, otherwise a.out is used
    #[arg(short, long, value_name = "FILE")]
    out: Option<PathBuf>,

    /// Defines an entry-point and makes this output an executable binary
    ///
    /// Can be either a hexadecimal address prefixed by 0x or a symbol
    #[arg(short, long)]
    entry: Option<String>,

    /// Erase internal symbols
    #[arg(short = 'S', long)]
    strip_internal: bool,

    /// Allow undefined references
    #[arg(short, long)]
    relocatable: bool,
}

fn main() -> ExitCode {
    let mut failure = false;

    let Cli {
        input_files,
        out,
        entry,
        strip_internal,
        relocatable,
    } = Cli::parse();

    let objects: Vec<_> = input_files
        .into_iter()
        .map(|p| {
            let obj = Object::from_file(&p).unwrap();
            (p, obj)
        })
        .collect();

    let mut segs = BTreeMap::new();

    {
        let mut lengths = BTreeMap::new();

        for (_, obj) in &objects {
            for (&stype, &(_start, ref v)) in &obj.segs {
                *lengths.entry(stype).or_insert(0) += v.len() as u16;
            }
        }
        let mut last_end = lengths.remove(&SegmentType::Zero).unwrap_or(0);
        last_end = last_end.max(SEGMENT_ALIGNMENT);

        for (st, size) in lengths {
            let start = align(last_end, SEGMENT_ALIGNMENT);
            segs.insert(st, (start, Vec::with_capacity(size as usize)));
            last_end = start + size;
        }
    }

    let out = out.unwrap_or_else(|| PathBuf::from("a.to"));

    let mut global_symbols = HashMap::new();
    let mut symbols_out = Vec::new();
    let mut reloc_out = Vec::new();
    let mut undefined_references = Vec::new();

    for (input_file, mut obj) in objects {
        let symbols_file_start = symbols_out.len();
        let reloc;

        {
            for mut symdef in obj.symbols.into_iter() {
                let next_id = symbols_out.len();

                symdef.location -= obj.segs.get(&symdef.segment_type).map(|s| s.0).unwrap_or(0);
                symdef.location += segs.get(&symdef.segment_type).map(|s| s.0).unwrap_or(0);

                if symdef.is_global {
                    match global_symbols.get(&symdef.name) {
                        None => {
                            global_symbols.insert(symdef.name.clone(), next_id);
                        }
                        Some(&id) => {
                            let cur_symdef: &mut SymbolDefinition = &mut symbols_out[id];

                            if let SegmentType::Unknown = symdef.segment_type {
                            } else {
                                if let SegmentType::Unknown = cur_symdef.segment_type {
                                    *cur_symdef = symdef;
                                } else {
                                    eprintln!("global symbol {} defined in {} but was already defined in a previous file at location 0x{:02x} in {}",
                                        symdef.name,
                                        input_file.display(),
                                        symdef.location,
                                        symdef.segment_type,
                                    );
                                    failure = true;
                                }
                            }

                            continue;
                        }
                    }
                } else {
                    if strip_internal {
                        symdef.name = "".into();
                    }
                }

                symbols_out.push(symdef);
            }
            reloc = obj
                .relocation_table
                .take()
                .map(|r| r.0)
                .unwrap_or(Vec::new());
        }

        for RelocationEntry {
            reference_location,
            reference_segment,
            symbol_index,
        } in reloc
        {
            // TODO: translate this correctly
            let symbol_index = symbol_index as usize - symbols_file_start;

            let location_in_file =
                reference_location - obj.segs.get(&reference_segment).map(|s| s.0).unwrap_or(0);
            let reference_location =
                location_in_file + segs.get(&reference_segment).map(|s| s.0).unwrap_or(0);

            let bytes = &mut obj.segs.get_mut(&reference_segment).unwrap().1;

            let symdef = &symbols_out[symbol_index];
            let undefined = matches!(symdef.segment_type, SegmentType::Unknown);

            bytes[location_in_file as usize..location_in_file as usize + 2]
                .copy_from_slice(&symdef.location.to_le_bytes());

            let entry = RelocationEntry {
                reference_location,
                reference_segment,
                symbol_index: symbol_index as u16,
            };

            reloc_out.push(entry);
            if undefined {
                undefined_references.push(entry);
            }
        }

        for (t, (_, bytes)) in obj.segs {
            segs.get_mut(&t).unwrap().1.extend(bytes);
        }
    }

    for RelocationEntry {
        reference_segment,
        reference_location,
        symbol_index,
    } in undefined_references
    {
        let symdef = &symbols_out[symbol_index as usize];
        if let SegmentType::Unknown = symdef.segment_type {
            if !relocatable {
                eprintln!(
                    "undefined reference to {} at 0x{:02x}",
                    symdef.name, symdef.location
                );
                failure = true;
            }
            continue;
        };

        let seg = segs.get_mut(&reference_segment).unwrap();
        let index = (reference_location - seg.0) as usize;
        seg.1[index..index + 2].copy_from_slice(&symdef.location.to_le_bytes());
    }

    let start_addr = match entry {
        Some(entry) => Some({
            if entry.starts_with("0x") {
                u16::from_str_radix(&entry[2..], 16).unwrap()
            } else {
                if let Some(&pos) = global_symbols.get(&*entry) {
                    symbols_out[pos].location
                } else {
                    eprintln!("Start symbol {entry} was not found. Perhaps it is not global?");
                    eprintln!("Aborting linking");
                    failure = true;
                    0xffff
                }
            }
        }),
        None => None,
    };

    if failure {
        return ExitCode::FAILURE;
    }

    let obj = {
        let mut obj = Object::default();

        obj.segs = segs;
        obj.entry = start_addr.map(Entry);
        obj.symbols = SymbolTable(symbols_out);
        obj.relocation_table = Some(RelocationTable(reloc_out));

        obj
    };

    if let Some(_addr) = start_addr {
        println!("Writing executable binary telda file");

        let mut obj = obj;
        {
            let mut file = File::create(&out).unwrap();
            writeln!(file, "#!/bin/env t").unwrap();

            obj.file_offset = file.stream_position().unwrap();
        }

        obj.write_to_file(&out).unwrap();

        let mut perms = fs::metadata(&out).unwrap().permissions();
        perms.set_mode(perms.mode() | 0o111);
        fs::set_permissions(&out, perms).unwrap();
    } else {
        println!("Writing non-executable binary telda file");
        obj.write_to_file(out).unwrap();
    }

    ExitCode::SUCCESS
}
