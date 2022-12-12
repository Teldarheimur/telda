use std::{fs::File, env::args, path::Path, io::Write, process::ExitCode};

use telda2::{source::{SourceLines, process, DataLine, write_data_operand, LabelRead, SymbolType}, ext_files::{BINARY_EXT, SYMBOL_FILE_EXT, NON_GLOBAL_SYMBOL_FILE_EXT, write_symbol_references, SYMBOL_REFERENCE_EXT}};

fn main() -> ExitCode {
    let mut ret = ExitCode::SUCCESS;
    for arg in args().skip(1) {
        let p = Path::new(&arg);
        let (labels, data_lines)
        = match SourceLines::new(p).and_then(|l| process(l)) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("{}", e);
                ret = ExitCode::FAILURE;
                continue;
            }
        };
        let mut label_reads: Vec<Vec<LabelRead>> = Vec::new();
        label_reads.resize_with(labels.len(), Vec::new);

        let mut mem = Vec::with_capacity(256);
        for data_line in data_lines {
            match data_line {
                DataLine::Raw(mut bytes) => {
                    mem.append(&mut bytes);
                }
                DataLine::Ins(opcode, dat_op) => {
                    mem.push(opcode);

                    let read_label = |id: usize, lr| {
                        label_reads[id].push(lr);
                        labels[id].2
                    };

                    write_data_operand(&mut mem, read_label, dat_op).unwrap();
                }
            }
        }

        {
            let bin_path = p.with_extension(BINARY_EXT);
            let mut f = File::create(&bin_path).unwrap();
            f.write_all(&mem).unwrap();
            println!("Wrote binary to {}", bin_path.display());
        }

        {
            let gsym_path = p.with_extension(SYMBOL_FILE_EXT);
            let lsym_path = p.with_extension(NON_GLOBAL_SYMBOL_FILE_EXT);
            let mut global_symbol_f = File::create(&gsym_path).unwrap();
            let mut local_symbol_f = File::create(&lsym_path).unwrap();
            for (lbl, st, loc) in labels.iter() {
                match st {
                    SymbolType::Global => writeln!(global_symbol_f, "{lbl}: 0x{loc:02X}").unwrap(),
                    SymbolType::Internal => writeln!(local_symbol_f, "{lbl}: 0x{loc:02X}").unwrap(),
                    SymbolType::Reference => eprintln!("references {lbl}"),
                }
            }
            println!("Wrote symbols to {} and {}", gsym_path.display(), lsym_path.display());
    
            if labels.iter().all(|(s, _, _)| &**s != "_start") {
                eprintln!("Warning: no _start symbol");
            }
        }

        {
            let p = p.with_extension(SYMBOL_REFERENCE_EXT);
            let iter = label_reads
                .iter()
                .zip(&labels)
                .map(|(a, (l, _, _))| (&**l, &**a));

            write_symbol_references(p.with_extension(SYMBOL_REFERENCE_EXT), iter).unwrap();
            println!("Wrote symbol references to {}", p.display());
        }
    }
    ret
}
