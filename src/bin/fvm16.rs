use fvm::{
    Machine,
    standard16::StandardCpu
};
use std::{
    io::Read,
    fs::File,
    env::args,
};

fn main() {
    let mut memory = [0; 0x10000];

    if let Some(file) = args().skip(1).next() {
        let mut file = File::open(file).unwrap();

        file.read(&mut memory).unwrap();
    } else {
        eprintln!("Oh dear, you didn't give me a file to run");
        std::process::exit(1);
    }

    let mut machine = Machine::new(memory, StandardCpu::new());

    machine.run();
}
