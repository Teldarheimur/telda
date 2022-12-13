use std::{env::args, collections::{HashSet, HashMap}};

use telda2::{self, aalv::{obj::{ShebangAgnosticObject, GlobalSymbols, InternalSymbols, SymbolReferenceTable}, Segment}, source::Format};

fn main() {
    let arg = args().nth(1).unwrap();

    let obj = ShebangAgnosticObject::from_file(&arg).unwrap().into_object();
    let mut defined_symbols = HashSet::new();
    let mut undefined_symbols = HashMap::new();
    
    if let Some(v) = obj.global_symbols {
        println!("{}:", GlobalSymbols::NAME);
        for (l, a) in v.0 {
            println!("    {l} = 0x{a:02x}");
            defined_symbols.insert(l);
        }
        println!();
    }
    if let Some(v) = obj.internal_symbols {
        println!("{}:", InternalSymbols::NAME);
        for (l, a) in v.0 {
            println!("    {l} = 0x{a:02x}");
            defined_symbols.insert(l);
        }
        println!();
    }
    if let Some(v) = obj.symbol_reference_table {
        println!("{}:", SymbolReferenceTable::NAME);
        for (f, l, a) in v.0 {
            print!("    ");
            match f {
                Format::Absolute => print!("A"),
                Format::Big => print!("B"),
            }
            println!(" {l} @ 0x{a:02x}");
            if !defined_symbols.contains(&l) {
                *undefined_symbols.entry(l).or_insert(0u64) += 1;
            }
        }
        println!();
    }

    if !undefined_symbols.is_empty() {
        println!("undefined symbols:");
        for (s, num) in undefined_symbols {
            let pl = if num == 1 { "" } else { "s" };
            println!("    {s} with {num} reference{pl}");
        }
        println!();
    }
}
