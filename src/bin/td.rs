use std::{env::args, collections::HashMap, path::Path};

use telda2::{disassemble::{disassemble_instruction, DisassembledInstruction}, aalv::{obj::ShebangAgnosticObject}};

fn main() {
    for arg in args().skip(1) {
        let p = Path::new(&arg);
        
        let binary_code;
        let mut labels = HashMap::new();
        let mut pos_to_labels = HashMap::new();
        {
            let obj = ShebangAgnosticObject::from_file(p).unwrap().into_object();
            binary_code = obj.mem.unwrap().mem;

            let iter = obj.internal_symbols
                .map(|is| is.0.into_iter())
                .into_iter()
                .flatten()
                .chain(obj.global_symbols.map(|is| is.0.into_iter()).into_iter().flatten());

            for (label, position) in iter {
                labels.insert(label.clone(), position);
                pos_to_labels.insert(position, label);
            }
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
