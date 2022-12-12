use std::{fs::File, env::args, io::Read, collections::HashMap, path::Path};

use telda2::{disassemble::{disassemble_instruction, DisassembledInstruction}, ext_files::{read_symbol_file, NON_GLOBAL_SYMBOL_FILE_EXT, SYMBOL_FILE_EXT}};

fn main() {
    for arg in args().skip(1) {
        let p = Path::new(&arg);
        let mut binary_code = Vec::new();
        let mut f = File::open(p).unwrap();
        f.read_to_end(&mut binary_code).unwrap();

        let mut labels = HashMap::new();
        let mut pos_to_labels = HashMap::new();
        read_symbol_file(p.with_extension(NON_GLOBAL_SYMBOL_FILE_EXT), &mut labels, &mut pos_to_labels).unwrap();
        read_symbol_file(p.with_extension(SYMBOL_FILE_EXT), &mut labels, &mut pos_to_labels).unwrap();

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
