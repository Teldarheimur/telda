use std::{fs::File, env::args, io::{BufReader, Write}, path::Path};

use telda2::source::{SourceLines, process, DataLine, write_data_operand};

fn main() {
    for arg in args().skip(1) {
        let p = Path::new(&arg);
        let f = File::open(p).unwrap();
        let (id_to_pos, labels, data_lines) = process(SourceLines::new(BufReader::new(f)));
        let mut mem = Vec::with_capacity(256);
        for data_line in data_lines {
            match data_line {
                DataLine::Raw(mut bytes) => {
                    mem.append(&mut bytes);
                }
                DataLine::Ins(opcode, dat_op) => {
                    mem.push(opcode);

                    write_data_operand(&mut mem, &id_to_pos, dat_op);
                }
            }
        }

        let bin_path = p.with_extension("tbin");
        let mut f = File::create(&bin_path).unwrap();
        f.write_all(&mem).unwrap();
        println!("Wrote binary to {}", bin_path.display());

        let sym_path = p.with_extension("tsym");
        let mut f = File::create(p.with_extension("tsym")).unwrap();
        for (&id, &loc) in id_to_pos.iter() {
            let lbl = &labels[id];
            writeln!(f, "{lbl}: 0x{loc:02X}").unwrap();
        }
        println!("Wrote symbols to {}", sym_path.display());

        if labels.iter().all(|s| &**s != "_start") {
            eprintln!("Warning: no _start symbol");
        }
    }
}
