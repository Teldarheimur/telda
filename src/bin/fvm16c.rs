use telda::{
    SmartMemory,
    ArrayMemory,
    Memory,
    standard16::Opcode,
};
use std::{
    io::{Write, BufReader, BufRead, Result as IOResult},
    collections::HashMap,
    fs::File,
    env::args,
};

fn main() -> IOResult<()> {
    let mut args = args();
    let mut memory = SmartMemory::new([0; 0x10000]);
    let file;

    if let Some(file_name) = (&mut args).skip(1).next() {
        file = File::open(&file_name)?;
    } else {
        eprintln!("Oh dear, you didn't give me a file to compile");
        std::process::exit(1);
    }

    let file = BufReader::new(file);

    let mut labels = HashMap::new();

    let mut i = 2;
    let mut line_num = 0;

    for line in file.lines() {
        let line = line?;
        let line = line.trim();
        line_num += 1;

        if !line.is_empty() && !line.starts_with("#") {
            let first_space_index = line.find(' ').unwrap_or(line.len());
            let ins = &line[..first_space_index];
            let args = line[(first_space_index + 1).min(line.len())..].split(',').skip_while(|s| s.chars().all(char::is_whitespace));

            if ins == "DATA" {
                for arg in args {
                    if let Some(data) = Operand::from_str(arg) {
                        match &data {
                            Operand::Byte(b) => {
                                memory.write(i, *b);
                                i += 1;
                            }
                            Operand::Index(p) => {
                                memory.write_index(i, *p);
                                i += 2;
                            }
                            Operand::Label(l) => {
                                if let Some(&p) = labels.get(l) {
                                    memory.write_index(i, p);
                                } else {
                                    eprintln!("Unknown label {} at line {}", l, line_num);
                                    std::process::exit(3);
                                }
                                i += 2;
                            }
                            Operand::String(s) => {
                                for &b in s {
                                    memory.write(i, b);
                                    i += 1;
                                }
                            }
                        }
                    } else {
                        eprintln!("Invalid data at line {}", line_num);
                        std::process::exit(2);
                    }
                }
            } else if let Some(ins) = Opcode::from_str(ins) {
                print!("{:?} ", ins);
                let ins = ins as u8;
                print!("{:02X}", ins);
                
                let ins_p = i;
                i += 1;
                for arg in args {
                    if let Some(operand) = Operand::from_str(arg) {
                        let mut width = 1;
                        match &operand {
                            Operand::Byte(b) => memory.write(i, *b),
                            Operand::Index(p) => {
                                width = 2;
                                memory.write_index(i, *p)
                            }
                            Operand::Label(l) => {
                                width = 2;
                                if let Some(&p) = labels.get(l) {
                                    memory.write_index(i, p);
                                } else {
                                    eprintln!("Unknown label {} at line {}", l, line_num);
                                    std::process::exit(3);
                                }
                            }
                            Operand::String(_) => {
                                eprintln!("Didn't expect string for this instruction at line {}", line_num);
                                std::process::exit(2);
                            }
                        }
                        i += width;
                    } else {
                        eprintln!("Invalid operand `{}' at line {}", arg, line_num);
                        std::process::exit(2);
                    }
                }
                memory.write(ins_p, ins | ((i - ins_p - 1) << 6) as u8);
            } else if ins.ends_with(":") {
                labels.insert(ins[..ins.len()-1].to_owned(), i);
            } else {
                eprintln!("Invalid instruction {} at line {}", ins, line_num);
                std::process::exit(2);
            }
        }
    }
    if let Some(start) = labels.get("start") {
        memory.write_index(0, *start);
    } else {
        eprintln!("No start label found");
        std::process::exit(3);
    }

    let mut output_file = File::create(args.next().unwrap_or_else(|| "out.fvm16".to_owned()))?;
    output_file.write_all(memory.slice())?;

    Ok(())
}

pub enum Operand {
    Byte(u8),
    Index(u16),
    Label(String),
    String(Vec<u8>),
}

fn escape_char(b: u8) -> Option<u8> {
    Some(match b {
        b @ b'\\' | b @ b'\"'..=b'\'' => b,
        b'n' => b'\n',
        b'r' => b'\r',
        b @ b'0' ..= b'9' => b - 0x30,
        _ => return None
    })
}

fn decode_number_or_string(text: &str) -> Result<Result<u16, u8>, &str> {
    if text.starts_with("0x") {
        let s = &text[2..];
        match s.len() {
            1..=2 => u8::from_str_radix(s, 16).map(|s| Err(s)).ok(),
            3..=4 => u16::from_str_radix(s, 16).map(|s| Ok(s)).ok(),
            _ => None,
        }.ok_or(text)
    } else if let Ok(b) = text.parse() {
        Ok(Err(b))
    } else {
        Err(text)
    }
}

impl Operand {
    fn from_str(s: &str) -> Option<Self> {
        if s.starts_with("'") {
            if s.ends_with("'") && (s.len() == 3 || (s[1..].starts_with('\\') && s.len() == 4)){
                if s[1..].starts_with('\\') {
                    Some(Operand::Byte(escape_char(s.as_bytes()[2])?))
                } else {
                    Some(Operand::Byte(s.as_bytes()[1]))
                }
            } else {
                None
            }
        } else if s.starts_with("\"") && s.ends_with("\"") {
            let mut buf = Vec::with_capacity(s.len()-2);
            let mut escaping = false;

            for ch in s[1..s.len()-1].bytes() {
                if escaping {
                    buf.push(escape_char(ch)?);
                    escaping = false;
                } else if ch == b'\\' {
                    escaping = true;
                } else {
                    buf.push(ch);
                }
            }
            if escaping {
                None
            } else {
                Some(Operand::String(buf))
            }
        } else {
            match decode_number_or_string(s) {
                Ok(Ok(p)) => Some(Operand::Index(p)),
                Ok(Err(b)) => Some(Operand::Byte(b)), 
                Err(s) => Some(Operand::Label(s.to_owned())),
            }
        }
    }
}
