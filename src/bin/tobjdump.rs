use std::{
    borrow::Cow, collections::{BTreeMap, HashMap, HashSet, VecDeque}, path::PathBuf, process::ExitCode
};

use clap::{ArgGroup, Parser};
use telda2::{
    aalv::{
        obj::{BinarySegment, HeapSize, Object, SegmentType, StackSize, SymbolDefinition, SymbolTable}, read_archive, Section
    },
    blf4::{Blf4, TrapMode},
    disassemble::{disassemble_instruction, DisassembledInstruction},
    machine::Machine,
    mem::{LazyMain, PanickingIO},
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(group(
    ArgGroup::new("show")
        .required(true)
        .multiple(true)
        .args(["disassemble", "show_symbols", "show_segments"]),
))]
struct Cli {
    /// Input telda object file
    #[arg(required = true)]
    input_files: Vec<PathBuf>,

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

    /// Show segment mappings
    #[arg(short = 'S', long, group = "show")]
    show_segments: bool,
}

fn read_objs(ret: &mut ExitCode, input_file: PathBuf) -> impl Iterator<Item=(String, Object)> {
    let e = 'miav: {
        if let Ok(mut ar) = read_archive(&input_file) {
            let mut objs = Vec::new();
            let mut i = 0;

            while let Some(obj) = ar.next(Object::from_aalv_reader).transpose() {
                i += 1;
                match obj {
                    Ok(o) => objs.push((format!("{}.{i}", input_file.display()), o)),
                    Err(e) => {
                        break 'miav e;
                    }
                }
            }
    
            return objs.into_iter();
        }
    
        match Object::from_file(&input_file) {
            Ok(o) => return vec![(format!("{}", input_file.display()), o)].into_iter(),
            Err(e) => break 'miav e,
        }
    };

    eprintln!("could not read object file: {e}");
    *ret = ExitCode::FAILURE;
    Vec::new().into_iter()
}

fn main() -> ExitCode {
    let Cli {
        input_files,
        disassemble,
        disassemble_from: dissasemble_from,
        show_symbols,
        show_relocations,
        show_segments,
    } = Cli::parse();

    let mut ret = ExitCode::SUCCESS;

    for (f, obj) in input_files.into_iter().flat_map(|p| read_objs(&mut ret, p)) {
        println!("{f}:");
        if show_segments {
            segments(&obj);
        }
        if show_symbols {
            symbols(&obj);
        }
        if disassemble {
            disassembly(&obj, &dissasemble_from, show_relocations);
        }
    } 

    ExitCode::SUCCESS
}

fn segments(obj: &Object) {
    for (seg_type, (addr, bytes)) in &obj.segs {
        println!("{}: {seg_type}: 0x{addr:03x} ({} bytes)", BinarySegment::NAME, bytes.len());
    }
    if let Some(stack_size) = obj.stack_size {
        println!("{}: {}", StackSize::NAME, stack_size.0);
    }
    if let Some(heap_size) = obj.heap_size {
        println!("{}: {}", HeapSize::NAME, heap_size.0);
    }
}

fn symbols(obj: &Object) {
    if !obj.symbols.0.is_empty() {
        println!("{}:", SymbolTable::NAME);

        let max_name_len = obj.symbols.0.iter().map(|s| s.name.len()).max().unwrap_or_default();

        for sym_def in &obj.symbols.0 {
            let &SymbolDefinition {
                ref name,
                is_global,
                segment_type,
                location,
            } = sym_def;

            if is_global {
                print!("  GLOBAL  ");
            } else {
                print!("  local   ");
            }
            match segment_type {
                SegmentType::Unknown => println!("{name:max_name_len$} = UNDEFINED (0x{location:04x})"),
                stype => println!("{name:max_name_len$} = 0x{location:04x} in {stype}"),
            }
        }
        println!();
    }
}

fn disassembly(obj: &Object, start_symbol: &Option<String>, show_relocations: bool) {
    let syms = &obj.symbols.0;

    let symbols: VecDeque<usize>;
    if let Some(start_symbol) = start_symbol.as_ref() {
        symbols = start_symbol
            .split(',')
            .map(|s| s.trim())
            .map(|name| {
                syms.iter()
                    .position(|s| &*s.name == name)
                    .expect("start symbol did not exist in symbol table")
            })
            .collect();
    } else {
        let entry_id = obj.symbols.0.len();
        symbols = obj
            .entry
            .map(|_| entry_id)
            .into_iter()
            .chain(obj.symbols.0.iter().enumerate().filter_map(|(i, s)| {
                if s.is_global {
                    Some(i)
                } else {
                    None
                }
            }))
            .collect();
    }

    println!("disassembly:");
    let mut machine;
    let mut pos_to_labels = HashMap::new();
    {
        machine = Machine::new(LazyMain::new(PanickingIO), Blf4::new());
        machine.load_user_binary(obj);

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
        if id == syms.len() {
            return Cow::Borrowed(".entry");
        }

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
            obj.entry
                .expect("this value would not happen if it is None")
                .1
        } else {
            syms[label_to_print].location
        };

        'labelled_block: loop {
            let mut label_name = Cow::Borrowed("");
            machine.cpu.program_counter = location;
            let res = disassemble_instruction(&mut machine, |p| {
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

            let DisassembledInstruction {
                annotated_source,
                ends_block,
                nesting_difference: _,
                next_instruction_location,
            } = match res {
                Ok(di) => di,
                Err(t) => {
                    match t {
                        TrapMode::IllegalExecute => {
                            println!("  data");
                        },
                        TrapMode::Level1PageFault | TrapMode::Level2PageFault => {
                            println!("  unmapped data");
                        },
                        _ => {
                            println!("  ERROR!");
                        }
                    }
                    break 'labelled_block;
                }
            };

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
