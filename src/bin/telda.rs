use std::{env::args, path::Path, fs::File, io::Read, collections::HashMap};

use telda2::{cpu::Cpu, mem::Lazy, ext_files::{read_symbol_file, NON_GLOBAL_SYMBOL_FILE_EXT, SYMBOL_FILE_EXT}};

fn main() {
    let arg = args().nth(1).unwrap();
    let p = Path::new(&arg);
    let mut mem = Vec::new();
    {
        let mut f = File::open(p).unwrap();
        f.read_to_end(&mut mem).unwrap();
    }

    let mut labels = HashMap::new();
    if let Err(_) =
        read_symbol_file(p.with_extension(NON_GLOBAL_SYMBOL_FILE_EXT), &mut labels, &mut ())
            .and_then(|_| read_symbol_file(p.with_extension(SYMBOL_FILE_EXT), &mut labels, &mut ()))
    {
        eprintln!("could not read symbol file(s)");
    }

    let start_id = labels.get("_start").copied()
        .unwrap_or_else(|| {eprintln!("no _start symbol found, using as 0 startpoint"); 0});
    let mut cpu = Cpu::new(start_id);

    let tm = cpu.run_until_trap(&mut Lazy { mem });
    let pc = cpu.registers.pc;
    let mut diff = pc;
    let mut closest = "";
    for (lbl, &loc) in labels.iter() {
        if pc >= loc {
            let new_diff = pc - loc;
            if new_diff < diff {
                diff = new_diff;
                closest = lbl;
            }
        }
    }
    println!("Ended with {tm:?} at <{closest}+{diff:02X}>");
}