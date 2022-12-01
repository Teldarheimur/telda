use std::{fs::File, env::args, io::{BufReader, Read, BufRead}, collections::HashMap, path::Path};

use telda2::disassemble::{disassemble_instruction, DisassembledInstruction};

fn main() {
    for arg in args().skip(1) {
        let p = Path::new(&arg);
        let mut binary_code = Vec::new();
        let mut f = File::open(p).unwrap();
        f.read_to_end(&mut binary_code).unwrap();
        let mut labels = HashMap::new();
        let mut pos_to_labels = HashMap::new();
        let f = File::open(p.with_extension("tsym")).unwrap();
        for line in BufReader::new(f).lines() {
            let line = line.unwrap();
            let colon = line.find(':').unwrap();
            let lbl = line[..colon].to_owned();
            let pos = u16::from_str_radix(&line[colon+4..], 16).unwrap();
            labels.insert(
                lbl.clone(),
                pos
            );
            pos_to_labels.insert(
                pos,
                lbl
            );
        }


        let mut printed_labels = vec!["_start"];
        let mut found_labels = vec!["_start"];

        loop {
            if found_labels.is_empty() {
                break;
            }

            let mut new_labels = Vec::new();
            for label in found_labels {
                printed_labels.push(label);
                let mut location = labels[label];

                println!("<{label}>:");
                loop {
                    let DisassembledInstruction { annotated_source, does_not_end_function, next_instruction_location }
                        = disassemble_instruction(location, &binary_code, &mut new_labels, &pos_to_labels);
                    println!("{}", annotated_source);
                    if !does_not_end_function {
                        break;
                    }
                    location = next_instruction_location;
                }
                println!();
            }
            new_labels.sort();
            new_labels.dedup();
            new_labels.retain(|s| !printed_labels.contains(s));

            found_labels = new_labels;
        }
    }
}
