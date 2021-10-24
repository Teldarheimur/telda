use telda::{
    Memory, TeldaEndian,
    is::{Reg, FullArg, OpAndArg, write_snd, write_both},
    standard16::VERSION,
};
use std::{
    io::{Write, BufReader, BufRead, Result as IOResult},
    collections::HashMap,
    path::PathBuf,
    fs::File,
    env::args,
};
use byteorder::ByteOrder;

struct DynMemory {
    mem: Vec<u8>,
}

impl DynMemory {
    #[inline]
    fn new() -> Self {
        DynMemory {
            mem: Vec::new(),
        }
    }
}

const BIN_EXTENSION: &'static str = "tld";
const INDEX_WIDTH: usize = 2;
const BYTE_WIDTH: usize = 1;

const UNDEFINED_REFERENCE: u16 = u16::MAX;

impl Memory<u16> for DynMemory {
    const INDEX_WIDTH: u16 = INDEX_WIDTH as u16;
    #[inline]
    fn read(&self, r: u16) -> u8 {
        self.mem.get(r as usize).copied().unwrap_or(0)
    }
    #[inline]
    fn read_index(&self, r: u16) -> u16 {
        if (r as usize) < self.mem.len() { 
            TeldaEndian::read_u16(&self.mem[r as usize..])
        } else { 0 }
    }
    #[inline]
    fn write(&mut self, r: u16, c: u8) {
        let new_length = r as usize + BYTE_WIDTH;
        if new_length > self.mem.len() {
            self.mem.resize(new_length, 0);
        }
        self.mem[r as usize] = c;
    }
    #[inline]
    fn write_index(&mut self, r: u16, c: u16) {
        let new_length = usize::from(r) + INDEX_WIDTH;
        if new_length > self.mem.len() {
            self.mem.resize(new_length, 0);
        }
        TeldaEndian::write_u16(&mut self.mem[r as usize..], c)
    }
    #[inline]
    fn size(&self) -> usize {
        self.mem.len()
    }
}

struct LabelReference {
    memory_index: u16,
    line_num: u32,
    label_name: String,
    offset: i16,
    args: Option<(Option<Reg>, FullArg)>,
}

impl LabelReference {
    #[inline]
    fn new(memory_index: u16, line_num: u32, label_name: String) -> Self {
        LabelReference {
            memory_index,
            line_num,
            label_name,
            offset: 0,
            args: None,
        }
    }
    #[inline]
    fn with_args(memory_index: u16, line_num: u32, label_name: String, offset: i16, args: (Option<Reg>, FullArg)) -> Self {
        LabelReference {
            memory_index,
            line_num,
            label_name,
            offset,
            args: Some(args),
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
    let mut memory = DynMemory::new();

    let file = BufReader::new(File::open(&path)?);

    let mut labels = HashMap::new();
    let mut label_references: Vec<LabelReference> = Vec::new();

    let mut i = 0;
    let mut line_num = 0;

    for line in file.lines() {
        let line = line?;
        let line = line.trim();
        line_num += 1;

        if !line.is_empty() && !line.starts_with("#") {
            parse_line(line, &mut memory, &mut i, line_num, &mut labels, &mut label_references)?;
        }
    }

    process_labels(label_references, &mut memory, &labels);

    let start = if let Some(start) = labels.get("start") {
        *start
    } else {
        eprintln!("No start label found");
        std::process::exit(3);
    };

    let mut output_file = File::create(&args.next().map(Into::into).unwrap_or_else(|| path.with_extension(BIN_EXTENSION)))?;
    output_file.write_all(b"tld")?;
    output_file.write_all(&[VERSION])?;
    {
        let mut start_buf = [0; 2];
        TeldaEndian::write_u16(&mut start_buf, start);
        output_file.write_all(&start_buf)?;
    }
    output_file.write_all(&memory.mem)?;

    Ok(())
}

fn process_labels(label_references: Vec<LabelReference>, memory: &mut DynMemory, labels: &HashMap<String, u16>) {
    for LabelReference { memory_index, line_num, args, label_name, offset } in label_references {
        let replacement = {
            if let Some(&p) = labels.get(&label_name) {
                (p as i16 + offset as i16) as u16
            } else {
                eprintln!("Unknown label `{}' at line {}", label_name, line_num);
                std::process::exit(3)
            }
        };

        if let Some(mut args) = args {
            if let FullArg::Wide(ref mut p) | FullArg::ImmRef(_, ref mut p) = args.1 {
                assert_eq!(*p, UNDEFINED_REFERENCE, "ICE: reference to replace wasn't undefined");
                *p = replacement;
            } else {
                panic!("ICE: invalid label reference");
            }

            let mut buf = [0; 3];

            let len = match args {
                (None, full_arg) => write_snd(&mut buf, full_arg),
                (Some(r), full_arg) => write_both(&mut buf, r, full_arg),
            };

            for (b, i) in buf[..len].iter().zip(0..) {
                memory.write(memory_index+i, *b);
            }
        } else {
            memory.write_index(memory_index, replacement);
        }
    }
} 

fn parse_line(line: &str, memory: &mut DynMemory, i: &mut u16, line_num: u32, labels: &mut HashMap<String, u16>, label_references: &mut Vec<LabelReference>) -> IOResult<()> {
    // eprintln!("{:3}: {:02X}: {}", line_num, i, line);

    let first_space_index = line.find(' ').unwrap_or(line.len());
    let ins = &line[..first_space_index];
    let args = line[(first_space_index + 1).min(line.len())..].split(',').map(|s| s.trim()).skip_while(|s| s.is_empty());

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
                        *i += INDEX_WIDTH as u16;
                    }
                    Operand::Label(l) => {
                        label_references.push(LabelReference::new(*i, line_num, l.to_owned()));
                        *i += INDEX_WIDTH as u16;
                    }
                    Operand::String(s) => {
                        for &b in s {
                            memory.write(*i, b);
                            *i += 1;
                        }
                    }
                    Operand::Reg(_) | Operand::RefReg(_, _, _) | Operand::Reference(_, _) => panic!("Invalid data"),
                }
            } else {
                eprintln!("Invalid data at line {}", line_num);
                std::process::exit(2);
            }
        }
    } else if ins == "include" {
        for arg in args {
            // TODO resolve include paths better
            // perhaps use enviroment variable for search path
            // and allow searching relative to the current source file
            let file = BufReader::new(File::open(arg)?);
            let mut inc_labels: HashMap<String, u16> = labels.clone();
            let mut label_references = Vec::new();

            for line in file.lines() {
                let line = line?;
                let line = line.trim();

                if !line.is_empty() && !line.starts_with("#") {
                    parse_line(line, memory, i, line_num, &mut inc_labels, &mut label_references)?;
                }
            }

            process_labels(label_references, memory, &inc_labels);

            // Remove all labels that don't begin in capital letters
            labels.extend(
                inc_labels.into_iter()
                .filter(|(k, _)| k.chars().next().map(|c| c.is_uppercase()).unwrap_or(false))
            );
        }
    } else if ins.ends_with(":") {
        let o = labels.insert(ins[..ins.len()-1].to_owned(), *i);
        if o.is_some() {
            eprintln!("Label {} is already defined once before, cannot be defined again at line {}", &ins[..ins.len()-1], line_num);
            std::process::exit(3);
        }
    } else {
        let mut args: Vec<_> = args.map(|arg| {
            if let Some(operand) = Operand::from_str(arg) {
                operand
            } else {
                eprintln!("Invalid operand `{}' at line {}", arg, line_num);
                std::process::exit(2);
            }
        }).collect();

        if args.len() > 2 {
            eprintln!("Too many arguments");
            std::process::exit(2);
        }

        fn parse_full_arg(fst: Option<Reg>, operand: Operand, i: u16, line_num: u32, label_references: &mut Vec<LabelReference>) -> (Option<Reg>, Option<FullArg>) {
            (fst, Some(match operand {
                Operand::String(_) => panic!(),
                Operand::Byte(b) => FullArg::Byte(b),
                Operand::Index(w) => FullArg::Wide(w),
                Operand::Reg(r) => FullArg::Reg(r),
                Operand::Label(label) => {
                    let arg = FullArg::Wide(UNDEFINED_REFERENCE);

                    label_references.push(LabelReference::with_args(i, line_num, label, 0, (fst, arg)));

                    arg
                },
                Operand::Reference(mut w, label) => {
                    // If the first argument is a wide register, then the reference implicitly is to a wide
                    if fst.map(|r| r.is_wide()).unwrap_or(false) {
                        w = true;
                    }
                    let arg = FullArg::ImmRef(w, UNDEFINED_REFERENCE);

                    label_references.push(LabelReference::with_args(i, line_num, label, 0, (fst, arg)));

                    arg
                }
                Operand::RefReg(mut w, reg, offset) => {
                    // If the first argument is a wide register, then the reference implicitly is to a wide
                    if fst.map(|r| r.is_wide()).unwrap_or(false) {
                        w = true;
                    }

                    FullArg::RegRef(w, reg, offset)
                }
            }))
        }

        *i += 1;

        let args = match args.pop() {
            None => (None, None),
            Some(snd) => match (args.pop(), snd) {
                (_, Operand::String(_)) | (Some(Operand::String(_)), _) => {
                    eprintln!("Invalid instruction {} at line {}", ins, line_num);
                    std::process::exit(2)
                },
                (None, Operand::Reg(r)) => (Some(r), None),

                (Some(Operand::Byte(0)), op) | (None, op) => parse_full_arg(None, op, *i, line_num, label_references),

                (Some(Operand::Reg(r)), op) => parse_full_arg(Some(r), op, *i, line_num, label_references),

                (Some(_), _) => {
                    eprintln!("Error: First argument can only be a register (or null) at line {}", line_num);
                    std::process::exit(2)
                },
            }
        };

        let size_match = args.0
            .and_then(|r| args.1.and_then(|fa| fa.is_wide().map(|w| w == r.is_wide()).ok()))
            .unwrap_or(true);

        if !size_match {
            eprintln!("Mismatching sizes in instruction {} at line {}", ins, line_num);
            std::process::exit(2);
        }

        if let Some(op_and_arg) = OpAndArg::from_str(ins, args) {
            let ins = op_and_arg.opcode as u8;

            let mut buf = [0; 3];

            memory.write(*i-1, ins);
            let len = op_and_arg.write(&mut buf);
            
            for b in &buf[..len] {
                memory.write(*i, *b);
                *i += 1;
            }
        } else {
            eprintln!("Invalid instruction {} at line {}", ins, line_num);
            std::process::exit(2);
        }
    }
    Ok(())
}

#[derive(Debug)]
pub enum Operand {
    Byte(u8),
    Index(u16),
    Label(String),
    Reg(Reg),
    Reference(bool, String),
    RefReg(bool, Reg, i8),
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
            1..=2 => u8::from_str_radix(s, 16).map(Err).ok(),
            3..=4 => u16::from_str_radix(s, 16).map(Ok).ok(),
            _ => None,
        }.ok_or(text)
    } else if text.starts_with("~") {
        text[1..].parse().map(Ok).map_err(|_| text)
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
        } else if s.starts_with("$") {
            Reg::from_str(&s[1..]).map(|r| Operand::Reg(r))
        } else if (s.starts_with("[") || s.starts_with("~[")) && s.ends_with("]") {
            let is_ref_to_wide = s.starts_with("~");
            let s = &s[1 + is_ref_to_wide as usize..s.len()-1];

            if s.starts_with("$") {
                let offset_index = s.find(|c| c == '+' || c == '-').unwrap_or(s.len());
                let offset = if offset_index == s.len() {
                    0
                } else {
                    if s.as_bytes()[0] == b'+' {
                        &s[offset_index+1..]
                    } else {
                        &s[offset_index..]
                    }.parse::<i8>().ok()?
                };

                let reg = Reg::from_str(&s[1..offset_index])?;

                Some(Operand::RefReg(is_ref_to_wide, reg, offset))
            } else {
                Some(Operand::Reference(is_ref_to_wide, s.to_owned()))
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
