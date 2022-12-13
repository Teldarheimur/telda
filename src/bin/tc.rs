use std::{env::args, path::Path, process::ExitCode};

use telda2::{
    source::{SourceLines, process, DataLine, write_data_operand, LabelRead, SymbolType, Wide},
    aalv::obj::{AALV_OBJECT_EXT, Object, GlobalSymbols, InternalSymbols, SymbolReferenceTable},
    mem::Lazy,
};

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
                DataLine::Wide(Wide::Number(w)) => mem.extend_from_slice(&w.to_le_bytes()),
                DataLine::Wide(Wide::Label(id)) => {
                    let lr = LabelRead { position: mem.len() as u16, format: telda2::source::Format::Absolute };
                    label_reads[id].push(lr);
                    let w = labels[id].2;
                    mem.extend_from_slice(&w.to_le_bytes());
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

        let mut aalvur = Object::default();
        aalvur.mem = Some(Lazy { mem });

        let (global_symbols, internal_symbols);

        {
            let mut gs = Vec::new();
            let mut is = Vec::new();

            for (lbl, st, loc) in labels.iter() {
                match st {
                    SymbolType::Global => gs.push((lbl.clone(), *loc)),
                    SymbolType::Internal => is.push((lbl.clone(), *loc)),
                    SymbolType::Reference => eprintln!("references {lbl}"),
                }
            }

            global_symbols = GlobalSymbols(gs);
            internal_symbols = InternalSymbols(is);
        }

        aalvur.global_symbols = Some(global_symbols);
        aalvur.internal_symbols = Some(internal_symbols);

        let symbol_reference_table;
        {
            let mut srt = Vec::new();
            for (i, label_reads) in label_reads.into_iter().enumerate() {
                let lbl = &labels[i].0;
                for LabelRead{position, format} in label_reads {
                    srt.push((format, lbl.clone(), position));
                }
            }
            symbol_reference_table = SymbolReferenceTable(srt);
        }
        aalvur.symbol_reference_table = Some(symbol_reference_table);

        aalvur.write_to_file(p.with_extension(AALV_OBJECT_EXT)).unwrap();
    }
    ret
}
