use telda::{
    Memory, TeldaEndian,
    is::{Opcode, Reg, Reference, FullArg, Args},
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
                TeldaEndian::read_u16(&self.mem[r as usize..])
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
            Mode::Bit16 => TeldaEndian::write_u16(&mut self.mem[r as usize..], c),
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
    fn eight(self) -> bool {
        match self {
            Mode::Bit8 => true,
            Mode::Bit16 => false,
        }
    }
    fn bin_extension(self) -> &'static str {
        match self {
            Mode::Bit8 => "t8",
            Mode::Bit16 => "tld",
        }
    }
}

struct LabelReference {
    memory_index: u16,
    line_num: u32,
    label_name: String,
    offset: i8,
    args: Option<Args>,
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
    fn with_args(memory_index: u16, line_num: u32, label_name: String, offset: i8, args: Args) -> Self {
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
    let mode = match path.extension().and_then(|a| a.to_str()).unwrap_or("") {
        "telda8" | "fvm8" | "fvm8c" => Mode::Bit8,
        "telda" | "telda16" | "fvm16" | "fvm16c" => Mode::Bit16,
        _ => Mode::Bit16,
    };
    let mut memory = DynMemory::new(mode);

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
            parse_line(line, &mut memory, &mut i, mode, line_num, &mut labels, &mut label_references)?;
        }
    }

    process_labels(label_references, &mut memory, mode, &labels);

    let start = if let Some(start) = labels.get("start") {
        *start
    } else {
        eprintln!("No start label found");
        std::process::exit(3);
    };

    let mut output_file = File::create(&args.next().map(Into::into).unwrap_or_else(|| path.with_extension(mode.bin_extension())))?;
    output_file.write_all(b"tld")?;
    output_file.write_all(&[match mode {
        Mode::Bit8 => 8,
        Mode::Bit16 => 16,
    }])?;
    match mode {
        Mode::Bit8 => output_file.write_all(&[start as u8])?,
        Mode::Bit16 => {
            let mut start_buf = [0; 2];
            TeldaEndian::write_u16(&mut start_buf, start);
            output_file.write_all(&start_buf)?;
        }
    }
    output_file.write_all(&memory.mem)?;

    Ok(())
}

fn process_labels(label_references: Vec<LabelReference>, memory: &mut DynMemory, mode: Mode, labels: &HashMap<String, u16>) {
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
            if let Some(FullArg::Wide(ref mut p)) | Some(FullArg::Ref(Reference::Val(ref mut p))) = args.snd {
                assert_eq!(*p, 0xff, "ICE: invalid label reference");
                *p = replacement;
            } else {
                panic!("ICE: invalid label reference");
            }

            let mut buf = [0; 3];
    
            let len = args.write(&mut buf, mode.eight());

            for (b, i) in buf[..len].iter().zip(0..) {
                memory.write(memory_index+i, *b);
            }
        } else {
            memory.write_index(memory_index, replacement);
        }
    }
} 

fn parse_line(line: &str, memory: &mut DynMemory, i: &mut u16, mode: Mode, line_num: u32, labels: &mut HashMap<String, u16>, label_references: &mut Vec<LabelReference>) -> IOResult<()> {
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
                        *i += mode.size() as u16;
                    }
                    Operand::Label(l) => {
                        label_references.push(LabelReference::new(*i, line_num, l.to_owned()));
                        *i += mode.size() as u16;
                    }
                    Operand::String(s) => {
                        for &b in s {
                            memory.write(*i, b);
                            *i += 1;
                        }
                    }
                    Operand::Reg(_) | Operand::RefReg(_, _) | Operand::Reference(_, _) => panic!("Invalid data"),
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
                    parse_line(line, memory, i, mode, line_num, &mut inc_labels, &mut label_references)?;
                }
            }

            process_labels(label_references, memory, mode, &inc_labels);

            // Remove all labels that don't begin in capital letters
            labels.extend(
                inc_labels.into_iter()
                .filter(|(k, _)| k.chars().next().map(|c| c.is_uppercase()).unwrap_or(false))
            );
        }
    } else if let Some(ins) = Opcode::from_str(ins) {
        let ins = ins as u8;
        
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

        fn parse_full_arg_from_operand(operand: Operand, i: u16, line_num: u32, label_references: &mut Vec<LabelReference>, mode: Mode) -> FullArg {
            match operand {
                Operand::String(_) => panic!(),
                Operand::Byte(b) => FullArg::Byte(b),
                Operand::Index(w) => match mode {
                    Mode::Bit16 => FullArg::Wide(w),
                    Mode::Bit8 => if w > 0xff {
                        eprintln!("Pointer too big for 8-bit");
                        std::process::exit(3);
                    } else {
                        FullArg::Byte(w as u8)
                    }
                },
                Operand::Reg(r) => FullArg::Reg(r),
                Operand::Label(label) => {
                    let arg = FullArg::Wide(0xff);
                    let args = Args { fst : None, snd: Some(arg)};

                    label_references.push(LabelReference::with_args(i, line_num, label, 0, args));

                    arg
                },
                Operand::Reference(label, offset) => {
                    let arg = FullArg::Ref(Reference::Val(0xff));
                    let args = Args { fst : None, snd: Some(arg) };

                    label_references.push(LabelReference::with_args(i, line_num, label, offset, args));

                    arg
                }
                Operand::RefReg(reg, offset) => FullArg::Ref(Reference::Reg{reg, offset}),
            }
        }

        *i += 1;

        let args = match args.pop() {
            None => Args { fst: None, snd: None},
            Some(snd) => match (args.pop(), snd) {
                (_, Operand::String(_)) | (Some(Operand::String(_)), _) => {
                    eprintln!("Invalid instruction {} at line {}", ins, line_num);
                    std::process::exit(2)
                },
                (None, Operand::Reg(r)) => Args { fst: Some(r), snd: None },

                (None, op) => Args { fst: None, snd: Some(parse_full_arg_from_operand(op, *i, line_num, label_references, mode))},

                (Some(Operand::Byte(0)), op) => Args { fst: None, snd: Some(parse_full_arg_from_operand(op, *i, line_num, label_references, mode)) },
                (Some(Operand::Reg(r)), op) => Args { fst: Some(r), snd: Some(parse_full_arg_from_operand(op, *i, line_num, label_references, mode))},
                (Some(_), _) => {
                    eprintln!("Error: First argument can only be a register (or null) at line {}", line_num);
                    std::process::exit(2)
                },
            }
        };

        let mut buf = [0; 3];

        memory.write(*i-1, ins | args.mask());
        let len = args.write(&mut buf, mode.eight());
        
        for b in &buf[..len] {
            memory.write(*i, *b);
            *i += 1;
        }


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

#[derive(Debug)]
pub enum Operand {
    Byte(u8),
    Index(u16),
    Label(String),
    Reg(Reg),
    Reference(String, i8),
    RefReg(Reg, i8),
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
            reg(&s[1..]).map(|r| Operand::Reg(r))
        } else if s.starts_with("[") && s.ends_with("]") {
            let s = &s[1..s.len()-1];

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

            if s.starts_with("$") {
                let reg = reg(&s[1..offset_index])?;

                Some(Operand::RefReg(reg, offset))
            } else {
                let label = s[..offset_index].to_owned();

                Some(Operand::Reference(label, offset))
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

#[inline]
fn reg(s: &str) -> Option<Reg> {
    match s {
        "s" | "sp" => Some(Reg::Sp),
        "b" | "bc" => Some(Reg::Bc),
        "a" => Some(Reg::Acc),
        "ac" => Some(Reg::AccW),
        _ => None,
    }
}
