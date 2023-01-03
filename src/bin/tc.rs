use std::{env::args, path::Path, process::ExitCode, collections::BTreeMap};

use telda2::{
    source::{SourceLines, process, DataLine, write_data_operand, LabelRead, SymbolType, Wide, ProcessedSource},
    aalv::obj::{AALV_OBJECT_EXT, Object, SymbolTable, SymbolDefinition, RelocationTable, RelocationEntry, SegmentType, Entry},
};

fn main() -> ExitCode {
    let mut ret = ExitCode::SUCCESS;
    for arg in args().skip(1) {
        let p = Path::new(&arg);
        let ProcessedSource{labels, dls, entry}
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

        let mut segs = BTreeMap::new();
        let mut lines = Vec::with_capacity(dls.len());

        for (stype, dls) in dls {
            segs.insert(stype, (dls.start, Vec::with_capacity(dls.size as usize)));
            lines.push(dls.lines);
        }

        for ((&st, &mut (segment_start, ref mut mem)), lines) in segs.iter_mut().zip(lines) {
            for data_line in lines {
                match data_line {
                    DataLine::Raw(mut bytes) => {
                        mem.append(&mut bytes);
                    }
                    DataLine::Wide(Wide::Number(w)) => mem.extend_from_slice(&w.to_le_bytes()),
                    DataLine::Wide(Wide::Label(id)) => {
                        let lr = LabelRead { segment: st, position: mem.len() as u16 + segment_start };
                        label_reads[id].push(lr);
                        let w = labels[id].3;
                        mem.extend_from_slice(&w.to_le_bytes());
                    }
                    DataLine::Ins(opcode, dat_op) => {
                        mem.push(opcode);

                        let read_label = |id: usize, lr| {
                            label_reads[id].push(lr);
                            labels[id].3
                        };

                        write_data_operand(st, mem, read_label, dat_op).unwrap();
                    }
                }
            }
        }

        let mut aalvur = Object {segs, .. Object::default()};

        aalvur.entry = entry.map(Entry);

        let mut symbol_table = Vec::new();
        {
            for &(ref lbl, st, segment_type, location) in labels.iter() {
                let is_global;
                match st {
                    SymbolType::Global => is_global = true,
                    SymbolType::Internal => is_global = false,
                    SymbolType::Reference => {
                        assert_eq!(segment_type, SegmentType::Unknown, "reference symbols should have unknown segment type");
                        is_global = true;
                    }
                }

                symbol_table.push(SymbolDefinition {
                    name: lbl.clone(),
                    is_global,
                    segment_type,
                    location,
                })
            }
        }
        aalvur.symbols = SymbolTable::from_iter(symbol_table.into_iter());

        let reloc_table;
        {
            let mut reloc_t = Vec::new();

            for (i, label_reads) in label_reads.into_iter().enumerate() {
                let symbol_index = i as u16;

                for LabelRead{ segment, position } in label_reads {
                    let entry = RelocationEntry {
                        reference_location: aalvur.segs[&segment].0 + position,
                        reference_segment: segment,
                        symbol_index,
                    };

                    reloc_t.push(entry);
                }
            }
            reloc_table = RelocationTable(reloc_t);
        }
        aalvur.relocation_table = Some(reloc_table);

        aalvur.write_to_file(p.with_extension(AALV_OBJECT_EXT)).unwrap();
    }
    ret
}
