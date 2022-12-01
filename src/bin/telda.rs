use std::{env::args, path::Path, fs::File, io::{Read, BufReader, BufRead}, collections::HashMap};

use telda2::{cpu::Cpu, mem::Lazy};

fn main() {
    let arg = args().nth(1).unwrap();
    let p = Path::new(&arg);
    let mut mem = Vec::new();
    let mut f = File::open(p).unwrap();
    f.read_to_end(&mut mem).unwrap();
    let mut labels = HashMap::new();
    if let Ok(f) = File::open(p.with_extension("tsym")) {
        for line in BufReader::new(f).lines() {
            let line = line.unwrap();
            let colon = line.find(':').unwrap();
            let lbl = line[..colon].to_owned();
            let pos = u16::from_str_radix(&line[colon+4..], 16).unwrap();
            labels.insert(
                lbl.clone(),
                pos
            );
        }
    } else {
        eprintln!("no symbol file found");
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