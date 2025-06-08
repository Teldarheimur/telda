use std::{collections::BTreeMap, env::args_os, io::{stdin, stdout, BufRead, Cursor, Write}, path::Path, process::ExitCode};

use telda2::{
    aalv::{obj::{
        Object, RelocationEntry, RelocationTable, SegmentType, SymbolDefinition, SymbolTable,
        AALV_OBJECT_EXT,
    }, AalvWriter},
    source::{
        process, write_data_operand, DataLine, LabelRead, ProcessedSource, Result, SourceLines, SymbolType, Wide
    },
};

fn main() -> ExitCode {
    let args = args_os().skip(1);
    if args.len() == 0 {
        let stdin = stdin().lock();
        let res = assemble(Ok(SourceLines::from_reader(stdin)))
            .and_then(|obj| {
                let mut out = Vec::new();
                let writer = AalvWriter::new(Cursor::new(&mut out), 0)?;
                obj.write_to(writer)?;
                stdout().write_all(&out)?;
                Ok(())
            });
        let exit_code;
        match res {
            Ok(()) => exit_code = ExitCode::SUCCESS,
            Err(e) => {
                exit_code = ExitCode::FAILURE;
                eprintln!("{e}");
            }
        }
        return exit_code;
    }

    let mut exit_code = ExitCode::SUCCESS;
    for arg in args {
        let p = Path::new(&arg);
        let res = assemble(SourceLines::new(p))
            .and_then(|obj| obj.write_to_file(p.with_extension(AALV_OBJECT_EXT)).map_err(From::from));
        if let Err(e) = res {
            eprintln!("{e}");
            exit_code = ExitCode::FAILURE;
        }
    }
    exit_code
}

fn assemble<B: BufRead>(source_lines: Result<SourceLines<B>>) -> Result<Object> {
    let ProcessedSource { labels, dls, entry } = source_lines.and_then(process)?;
    let mut label_reads: Vec<Vec<LabelRead>> = Vec::new();
    label_reads.resize_with(labels.len(), Vec::new);

    let mut segs = BTreeMap::new();

    for (st, dl_seg) in dls {
        let segment_start = dl_seg.start;
        let mut mem = Vec::with_capacity(dl_seg.size as usize);
        for data_line in dl_seg.lines {
            match data_line {
                DataLine::Raw(mut bytes) => {
                    mem.append(&mut bytes);
                }
                DataLine::Wide(Wide::Number(w)) => mem.extend_from_slice(&w.to_le_bytes()),
                DataLine::Wide(Wide::Label(id)) => {
                    let lr = LabelRead {
                        segment: st,
                        position: mem.len() as u16 + segment_start,
                        relative: false,
                    };
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

                    write_data_operand(st, segment_start, &mut mem, read_label, dat_op);
                }
            }
        }
        segs.insert(st, (segment_start, mem));
    }

    let mut aalvur = Object {
        segs,
        entry,
        ..Object::default()
    };

    let mut symbol_table = Vec::new();
    for &(ref lbl, st, segment_type, location) in labels.iter() {
        let is_global = match st {
            SymbolType::Global => true,
            SymbolType::Internal => false,
            SymbolType::Reference => {
                assert_eq!(
                    segment_type,
                    SegmentType::Unknown,
                    "reference symbols should have unknown segment type"
                );
                true
            }
        };

        symbol_table.push(SymbolDefinition {
            name: lbl.clone(),
            is_global,
            segment_type,
            location,
        })
    }
    aalvur.symbols = SymbolTable(symbol_table);

    let mut reloc_table = Vec::new();

    for (i, label_reads) in label_reads.into_iter().enumerate() {
        let symbol_segment = labels[i].2;
        let symbol_index = i as u16;

        for LabelRead { segment, position, relative } in label_reads {
            if relative && symbol_segment == segment {
                // we don't need to make relocation entries for relative references in the same segment
                // as the relative addresses inside the same segment will stay the same. the linker cannot move parts of data inside segments around, only concatenate like segments.
                continue;
            }

            let entry = RelocationEntry {
                reference_location: aalvur.segs[&segment].0 + position,
                reference_segment: segment,
                symbol_index,
                relative,
            };

            reloc_table.push(entry);
        }
    }
    aalvur.relocation_table = RelocationTable(reloc_table);

    Ok(aalvur)
}
