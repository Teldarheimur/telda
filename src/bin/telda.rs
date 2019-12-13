use std::{
    env::args,
    fs::File,
    io::{Read},
};
use telda::{
    Machine,
    standard8::StandardCpu as Cpu8,
    standard16::StandardCpu as Cpu16,
};
use byteorder::{NativeEndian, ByteOrder};

fn main() {
    let mut file = if let Some(file) = args().skip(1).next() {
        File::open(file).unwrap()
    } else {
        eprintln!("Oh dear, you didn't give me a file to run");
        std::process::exit(1);
    };

    let mut magic = [0; 5];
    file.read_exact(&mut magic).unwrap();
    if &magic[0..2] == b"#!" {
        let mut b = [0];
        while &b != b"\n" {
            file.read_exact(&mut b).unwrap();
        }
        file.read_exact(&mut magic).unwrap();
    }
    let mut version = [0; 1];
    file.read_exact(&mut version).unwrap();
    if &magic != b"telda" {
        eprintln!("Invalid format");
        std::process::exit(2);
    }
    match version[0] {
        8 => {
            let mut start = [0; 1];
            file.read_exact(&mut start).unwrap();
            let start = start[0];

            let mut memory = [0; 0x100];
            file.read(&mut memory).unwrap();
            let mut machine = Machine::new(memory, Cpu8::new(start));
            machine.run();
        },
        16 => {
            let mut start = [0; 2];
            file.read_exact(&mut start).unwrap();
            let start = NativeEndian::read_u16(&start);

            let mut memory = [0; 0x10000];
            file.read(&mut memory).unwrap();
            let mut machine = Machine::new(memory, Cpu16::new(start));
            machine.run();
        }
        _ => {
            eprintln!("Invalid format");
            std::process::exit(2);
        }
    }
}

