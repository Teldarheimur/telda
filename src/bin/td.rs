use std::{env::args, collections::{HashMap, VecDeque, HashSet}, path::Path};

use telda2::{disassemble::{disassemble_instruction, DisassembledInstruction}, aalv::{obj::ShebangAgnosticObject}};

fn main() {
    let mut args = args();
    let arg = args.nth(1).unwrap();
    let symbols = args.collect::<Vec<_>>();

    if symbols.is_empty() {
        eprintln!("No starting symbols were given. Perhaps try `_start`");
        return;
    }

    {
        let p = Path::new(&arg);
        
        let binary_code;
        let mut labels = HashMap::new();
        let mut pos_to_labels = HashMap::new();
        {
            // TODO: also show relocation rules
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

        let mut printed_labels = HashSet::new();
        let mut labels_to_print = VecDeque::from_iter(symbols.iter().map(|s| &**s));

        while let Some(label_to_print) = labels_to_print.pop_front() {
            // Printed labels can end up in the queue
            if printed_labels.contains(&label_to_print) {
                continue;
            }

            println!("<{label_to_print}>:");
            printed_labels.insert(label_to_print);

            let mut location = labels[label_to_print];

            'labelled_block: loop {
                let DisassembledInstruction { annotated_source, does_not_end_function, next_instruction_location }
                    = disassemble_instruction(location, &binary_code, |p| {
                        let l = pos_to_labels.get(&p).map(|s| &**s);
                        if let Some(l) = l {
                            if !printed_labels.contains(&l) {
                                labels_to_print.push_back(l);
                            }
                        }
                        l
                    });
                println!("{}", annotated_source);
                if !does_not_end_function {
                    break 'labelled_block;
                }
                if let Some(lbl) = pos_to_labels.get(&next_instruction_location) {
                    if printed_labels.insert(lbl) {
                        // Was not printed before
                        println!("<{lbl}>:");
                    } else {
                        // Was printed before => end block
                        println!("<{lbl}> ...");
                        break 'labelled_block;
                    }
                }
                location = next_instruction_location;
            }
            println!();
        }
    }
}
