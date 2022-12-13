use std::env::args;

use telda2::{self, aalv::{obj::{ShebangAgnosticObject, GlobalSymbols, InternalSymbols, SymbolReferenceTable}, Segment}, source::Format};

fn main() {
    let arg = args().nth(1).unwrap();

    let obj = ShebangAgnosticObject::from_file(&arg).unwrap().into_object();
    
    if let Some(v) = obj.global_symbols {
        println!("{}:", GlobalSymbols::NAME);
        for (l, a) in v.0 {
            println!("    {l} = 0x{a:02x}");
        }
        println!();
    }
    if let Some(v) = obj.internal_symbols {
        println!("{}:", InternalSymbols::NAME);
        for (l, a) in v.0 {
            println!("    {l} = 0x{a:02x}");
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
        }
        println!();
    }
}
