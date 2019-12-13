use telda::{
    Memory,
    is::Opcode,
};
use std::{
    io::{Write, BufReader, BufRead, Result as IOResult},
    collections::HashMap,
    path::PathBuf,
    fs::File,
    env::args,
};
use byteorder::{NativeEndian, ByteOrder};

struct DynMemory {
    mem: Vec<u8>,
    mode: Mode,
}

impl DynMemory {
    #[inline]
    fn new(mode: Mode) -> Self {
        DynMemory {
            mode,
            mem: Vec::new(),
        }
    }
}

impl Memory<u16> for DynMemory {
    #[inline]
    fn read(&self, r: u16) -> u8 {
        self.mem.get(r as usize).copied().unwrap_or(0)
    }
    #[inline]
    fn read_index(&self, r: u16) -> u16 {
        match self.mode {
            Mode::Bit8 => self.read(r) as u16,
            Mode::Bit16 => if (r as usize) < self.mem.len() {
                NativeEndian::read_u16(&self.mem[r as usize..])
            } else { 0 },
        }
    }
    fn read_to_slice(&self, r: u16, length: u16) -> &[u8] {
        &self.mem[r as usize..(r+length) as usize]
    }
    #[inline]
    fn write(&mut self, r: u16, c: u8) {
        let new_length = r as usize + 1;
        if new_length > self.mem.len() {
            self.mem.resize(new_length, 0);
        }
        self.mem[r as usize] = c;
    }
    #[inline]
    fn write_index(&mut self, r: u16, c: u16) {
        let new_length = usize::from(r) + self.mode.size();
        if new_length > self.mem.len() {
            self.mem.resize(new_length, 0);
        }
        match self.mode {
            Mode::Bit8 => self.mem[r as usize] = c as u8,
            Mode::Bit16 => NativeEndian::write_u16(&mut self.mem[r as usize..], c),
        }
    }
    #[inline]
    fn size(&self) -> usize {
        self.mem.len()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
enum Mode {
    Bit8,
    Bit16,
}

impl Mode {
    fn size(self) -> usize {
        match self {
            Mode::Bit8 => 1,
            Mode::Bit16 => 2,
        }
    }
}

fn main() -> IOResult<()> {
    let mut args = args();
    let path: PathBuf;

    if let Some(file_name) = (&mut args).skip(1).next() {
        path = file_name.into();
    } else {
        eprintln!("Oh dear, you didn't give me a file to compile");
        std::process::exit(1);
    }
    let mode = match path.extension().and_then(|a| a.to_str()).unwrap_or("") {
        "t8" | "fvm8" | "fvm8c" => Mode::Bit8,
        "t16" | "fvm16" | "fvm16c" => Mode::Bit16,
        _ => Mode::Bit16,
    };
    let mut memory = DynMemory::new(mode);

    let file = BufReader::new(File::open(&path)?);

    let mut labels = HashMap::new();
    let mut label_references: Vec<(u16, u32, String)> = Vec::new();

    let mut i = 0;
    let mut line_num = 0;

    for line in file.lines() {
        let line = line?;
        let line = line.trim();
        line_num += 1;

        if !line.is_empty() && !line.starts_with("#") {
            parse_line(line, &mut memory, &mut i, mode, line_num, &mut labels, &mut label_references)?;
        }
    }

    for (i, line_num, label_name) in label_references {
        if let Some(&p) = labels.get(&label_name) {
            memory.write_index(i, p);
        } else {
            eprintln!("Unknown label {} at line {}", label_name, line_num);
            std::process::exit(3);
        }
    }

    let start = if let Some(start) = labels.get("start") {
        *start
    } else {
        eprintln!("No start label found");
        std::process::exit(3);
    };

    let mut output_file = File::create(&args.next().map(Into::into).unwrap_or_else(|| path.with_extension("t")))?;
    output_file.write_all(b"telda")?;
    output_file.write_all(&[match mode {
        Mode::Bit8 => 8,
        Mode::Bit16 => 16,
    }])?;
    match mode {
        Mode::Bit8 => output_file.write_all(&[start as u8])?,
        Mode::Bit16 => {
            let mut start_buf = [0; 2];
            NativeEndian::write_u16(&mut start_buf, start);
            output_file.write_all(&start_buf)?;
        }
    }
    output_file.write_all(&memory.mem)?;

    Ok(())
}

fn parse_line(line: &str, memory: &mut DynMemory, i: &mut u16, mode: Mode, line_num: u32, labels: &mut HashMap<String, u16>, label_references: &mut Vec<(u16, u32, String)>) -> IOResult<()> {
    // eprintln!("{:3}: {:02X}: {}", line_num, i, line);

    let first_space_index = line.find(' ').unwrap_or(line.len());
    let ins = &line[..first_space_index];
    let args = line[(first_space_index + 1).min(line.len())..].split(',').skip_while(|s| s.chars().all(char::is_whitespace));

    if ins == "data" || ins == "dat" {
        for arg in args {
            if let Some(data) = Operand::from_str(arg) {
                match &data {
                    Operand::Byte(b) => {
                        memory.write(*i, *b);
                        *i += 1;
                    }
                    Operand::Index(p) => {
                        memory.write_index(*i, *p);
                        *i += mode.size() as u16;
                    }
                    Operand::Label(l) => {
                        label_references.push((*i, line_num, l.to_owned()));
                        *i += mode.size() as u16;
                    }
                    Operand::String(s) => {
                        for &b in s {
                            memory.write(*i, b);
                            *i += 1;
                        }
                    }
                }
            } else {
                eprintln!("Invalid data at line {}", line_num);
                std::process::exit(2);
            }
        }
    } else if ins == "include" {
        for arg in args {
            let file = BufReader::new(File::open(arg)?);
            let mut inc_labels: HashMap<String, u16> = labels.clone();
            let mut label_references = Vec::new();

            for line in file.lines() {
                let line = line?;
                let line = line.trim();

                if !line.is_empty() && !line.starts_with("#") {
                    parse_line(line, memory, i, mode, line_num, &mut inc_labels, &mut label_references)?;
                }
            }

            for (i, line_num, label_name) in label_references {
                if let Some(&p) = inc_labels.get(&label_name) {
                    memory.write_index(i, p);
                } else {
                    eprintln!("Unknown label {} at line {}", label_name, line_num);
                    std::process::exit(3);
                }
            }
            // Remove all labels that don't begin in capital letters
            inc_labels.retain(|k, _| k.chars().next().map(|c| c.is_uppercase()).unwrap_or(false));
            labels.extend(inc_labels.into_iter());
        }
    } else if let Some(ins) = Opcode::from_str(ins) {
        let ins = ins as u8;
        
        let ins_p = *i;
        *i += 1;
        for arg in args {
            if let Some(operand) = Operand::from_str(arg) {
                let mut width = 1;
                match &operand {
                    Operand::Byte(b) => memory.write(*i, *b),
                    Operand::Index(p) => {
                        width = mode.size() as u16;
                        memory.write_index(*i, *p)
                    }
                    Operand::Label(l) => {
                        width = mode.size() as u16;
                        label_references.push((*i, line_num, l.to_owned()));
                    }
                    Operand::String(_) => {
                        eprintln!("Didn't expect string for this instruction at line {}", line_num);
                        std::process::exit(2);
                    }
                }
                *i += width;
            } else {
                eprintln!("Invalid operand `{}' at line {}", arg, line_num);
                std::process::exit(2);
            }
        }
        memory.write(ins_p, ins | ((*i - ins_p - 1) << 6) as u8);
    } else if ins.ends_with(":") {
        let o = labels.insert(ins[..ins.len()-1].to_owned(), *i);
        if o.is_some() {
            eprintln!("Label {} is already defined once before, cannot be defined again at line {}", &ins[..ins.len()-1], line_num);
            std::process::exit(3);
        }
    } else {
        eprintln!("Invalid instruction {} at line {}", ins, line_num);
        std::process::exit(2);
    }
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
