use fvm::{
    Memory,
    standard16::Opcode,
};
use std::{
    io::{Write, BufReader, BufRead, Result as IOResult},
    collections::HashMap,
    fs::File,
    env::args,
};

struct SmartMemory {
    length: usize,
    buffer: [u8; 0x10000],
}

impl SmartMemory {
    #[inline]
    fn new() -> Self {
        SmartMemory {
            length: 0,
            buffer: [0; 0x10000],
        }
    }
    #[inline]
    fn slice(&self) -> &[u8] {
        &self.buffer[..self.length]
    }
}

impl Memory<u16> for SmartMemory {
    type Cell = u8;

    #[inline]
    fn read(&self, r: u16) -> Self::Cell {
        self.buffer.read(r)
    }
    #[inline]
    fn read_index(&self, r: u16) -> u16 {
        self.buffer.read_index(r)
    }
    #[inline]
    fn write(&mut self, r: u16, c: Self::Cell) {
        let new_length = r as usize;
        if new_length > self.length {
            self.length = new_length;
        }
        self.buffer.write(r, c)
    }
    #[inline]
    fn write_index(&mut self, r: u16, c: u16) {
        let new_length = r as usize + 1;
        if new_length > self.length {
            self.length = new_length;
        }
        self.buffer.write_index(r, c)
    }
    #[inline]
    fn size(&self) -> usize {
        self.buffer.size()
    }
}

fn main() -> IOResult<()> {
    let mut args = args();
    let mut memory = SmartMemory::new();
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
                            Operand::PointerLabel(_) | Operand::Pointer(_) => {
                                panic!("Can't make data using a pointer");
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
                let mut ins = ins as u8;
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
                            Operand::Pointer(p) => {
                                width = 2;
                                ins |= 0b1000_0000;
                                memory.write_index(i, *p)
                            }
                            Operand::PointerLabel(l) => {
                                width = 2;
                                ins |= 0b1000_0000;
                                if let Some(&p) = labels.get(l) {
                                    memory.write_index(i, p);
                                } else {
                                    eprintln!("Unknown label {} at line {}", l, line_num);
                                    std::process::exit(3);
                                }
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
                memory.write(ins_p, ins);
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

    Pointer(u16),
    PointerLabel(String),
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
        } else if s.starts_with("[") && s.ends_with("]") {
            match decode_number_or_string(&s[1 .. s.len() - 1]) {
                Ok(Ok(p)) => Some(Operand::Pointer(p)),
                Ok(Err(p)) => Some(Operand::Pointer(p as u16)), 
                Err(s) => Some(Operand::PointerLabel(s.to_owned())),
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
