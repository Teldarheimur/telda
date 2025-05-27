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
                    let lr = LabelRead {
                        segment: st,
                        position: mem.len() as u16 + segment_start,
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

                    write_data_operand(st, mem, read_label, dat_op);
                }
            }
        }
    }

    let mut aalvur = Object {
        segs,
        entry,
        ..Object::default()
    };

    let mut symbol_table = Vec::new();
    {
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
    }
    aalvur.symbols = SymbolTable(symbol_table);

    let reloc_table;
    {
        let mut reloc_t = Vec::new();

        for (i, label_reads) in label_reads.into_iter().enumerate() {
            let symbol_index = i as u16;

            for LabelRead { segment, position } in label_reads {
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
    aalvur.relocation_table = reloc_table;

    Ok(aalvur)
}