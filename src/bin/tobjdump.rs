use std::{path::PathBuf, collections::{HashMap, HashSet, VecDeque, BTreeMap}, process::ExitCode, borrow::Cow};

use clap::{Parser, ArgGroup};
use telda2::{aalv::{obj::{Object, SymbolTable, SegmentType}, Section}, disassemble::{DisassembledInstruction, disassemble_instruction}};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(group(
            ArgGroup::new("show")
                .required(true)
                .multiple(true)
                .args(["disassemble", "show_symbols"]),
        ))]
struct Cli {
    /// Input telda object file
    input_file: PathBuf,

    /// Disassemble symbols in .text. If -D is not set, disassembles from all global symbols
    #[arg(short, long, group = "show")]
    disassemble: bool,
    /// If disassembling, sets the symbols to start disassembling from seperated by commas
    #[arg(short = 'D', long, requires = "disassemble", value_name = "SYMBOLS")]
    disassemble_from: Option<String>,
    
    /// Whether to show the symbol table
    #[arg(short = 't', long = "syms", group = "show")]
    show_symbols: bool,

    /// Shows relocations in disassembly
    #[arg(short = 'R', long, requires = "disassemble")]
    show_relocations: bool,
}

fn main() -> ExitCode {
    let Cli {
        input_file,
        disassemble,
        disassemble_from: dissasemble_from,
        show_symbols,
        show_relocations,
    } = Cli::parse();

    let obj = match Object::from_file(&input_file) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("could not read object file: {e}");

            return ExitCode::FAILURE;
        }
    };

    if show_symbols {
        symbols(&obj);
    }
    if disassemble {
        disassembly(&obj, dissasemble_from, show_relocations);
    }

    ExitCode::SUCCESS
}

fn symbols(obj: &Object) {
    if !obj.symbols.0.is_empty() {
        println!("{}:", SymbolTable::NAME);
        for sym_def in &obj.symbols.0 {
            print!("    ");
            if sym_def.is_global {
                print!("GLOBAL ");
            }
            match sym_def.segment_type {
                SegmentType::Unknown => println!("{} = UNDEFINED ({:02x})", sym_def.name, sym_def.location),
                stype => println!("{} = {:02x} in {:?}", sym_def.name, sym_def.location, stype),
            }
        }
        println!();
    }
}

fn disassembly(obj: &Object, start_symbol: Option<String>, show_relocations: bool) {
    let syms = &obj.symbols.0;

    let symbols: VecDeque<usize>;
    if let Some(start_symbol) = start_symbol.as_ref() {
        symbols = start_symbol
            .split(',')
            .map(|s| s.trim())
            .map(|name| syms.iter().position(|s| &*s.name == name).expect("start symbol did not exist in symbol table"))
            .collect();
    } else {
        let entry_id = obj.symbols.0.len();
        symbols = obj.entry
            .map(|_| entry_id)
            .into_iter()
            .chain(
                obj.symbols.0.iter()
                    .enumerate()
                    .filter_map(|(i, s)| if s.is_global { Some(i) } else { None })
            )
            .collect();
    }

    println!("disassembly:");
    let mem;
    let mut pos_to_labels = HashMap::new();
    {
        mem = obj.get_flattened_memory();

        for (id, s) in obj.symbols.0.iter().enumerate() {
            pos_to_labels.insert(s.location, id);
        }
        if let Some(ep) = obj.entry {
            pos_to_labels.insert(ep.1, obj.symbols.0.len());
        }
    }

    let mut relocs = BTreeMap::new();
    if show_relocations {
        for &re in &obj.relocation_table.0 {
            relocs.insert(re.reference_location, re.symbol_index as usize);
        }
    }

    let mut printed_labels = HashSet::new();
    let mut labels_to_print = symbols;

    let get_name = |id: usize| {
        if id == syms.len() { return Cow::Borrowed(".entry") }

        let name = &*syms[id].name;
        if name.is_empty() {
            Cow::Owned(format!("@L{id}"))
        } else {
            Cow::Borrowed(name)
        }
    };

    while let Some(label_to_print) = labels_to_print.pop_front() {
        // Printed labels can end up in the queue
        if printed_labels.contains(&label_to_print) {
            continue;
        }

        println!("<{}>:", get_name(label_to_print));
        printed_labels.insert(label_to_print);

        let mut location = if label_to_print == syms.len() {
            obj.entry.expect("this value would not happen if it is None").1
        } else {
            syms[label_to_print].location
        };

        'labelled_block: loop {
            let mut label_name = Cow::Borrowed("");
            let DisassembledInstruction { annotated_source, ends_block, nesting_difference: _, next_instruction_location }
                = disassemble_instruction(location, &mem, |p| {
                    let l = pos_to_labels.get(&p).copied();
                    if let Some(l) = l {
                        if !printed_labels.contains(&l) {
                            labels_to_print.push_back(l);
                        }

                        label_name = get_name(l);
                        Some(&label_name)
                    } else {
                        None
                    }
                });

            if show_relocations {
                for (&loc, &sym) in relocs.range(location..next_instruction_location) {
                    println!("    RELOC: {} @ 0x{loc:02x}", get_name(sym));
                }
            }
            println!("{}", annotated_source);
            if ends_block {
                break 'labelled_block;
            }
            if let Some(&lbl) = pos_to_labels.get(&next_instruction_location) {
                let name = get_name(lbl);
                if printed_labels.insert(lbl) {
                    // Was not printed before
                    println!("<{name}>:");
                } else {
                    // Was printed before => end block
                    println!("<{name}> ...");
                    break 'labelled_block;
                }
            }
            location = next_instruction_location;
        }
        println!();
    }
}