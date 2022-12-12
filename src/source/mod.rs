use std::{io::{Lines, BufRead, BufReader}, fs::File, path::Path};

use crate::isa;

mod err;
pub use self::err::*;
mod symbols;
use self::symbols::*;
pub use self::symbols::{Format, LabelRead, SymbolType};

type Opcode = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BReg {
    Zero = 0,
    Al = 1,
    Ah = 2,
    Bl = 3,
    Bh = 4,
    Cl = 5,
    Ch = 6,
    Io = 7,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum WReg {
    Zero = 0,
    A = 1,
    B = 2,
    C = 3,
    X = 4,
    Y = 5,
    Z = 6,
    S = 7,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceOperand {
    Byte(u8),
    Wide(u16),
    Number(i32),
    ByteReg(BReg),
    WideReg(WReg),
    Label(String),
}

#[derive(Debug, Clone)]
pub enum SourceLine {
    Label(String),
    Ins(String, Vec<SourceOperand>),
    Comment,
    DirInclude(String),
    DirString(Vec<u8>),
    DirByte(u8),
    DirWide(u16),
    DirGlobal(String),
    DirReference(String),
}

pub struct SourceLines<B> {
    lines: Lines<B>,
    ln: LineNumber,
    source: Box<str>,
}

impl SourceLines<BufReader<File>> {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let source = format!("{}", path.as_ref().display()).into_boxed_str();
        let f = File::open(path).map_err(|e| Error::new(source.clone(), 0, ErrorType::IoError(e)))?;
        let br = BufReader::new(f);
        Ok(SourceLines {
            lines: br.lines(),
            ln: 0,
            source
        })
    }
}

impl<B: BufRead> SourceLines<B> {
    pub fn from_reader(r: B) -> Self {
        SourceLines {
            lines: r.lines(),
            ln: 0,
            source: "<input>".into(),
        }
    }
    fn parse_line(&mut self, line: StdResult<String, IoError>) -> Result<SourceLine> {
        Ok({
            self.ln += 1;
            let line = line?;
            let line = line.trim();

            if line.is_empty() {
                SourceLine::Comment
            } else
            if line.starts_with(";") || line.starts_with("//") || line.starts_with("#") {
                SourceLine::Comment
            } else
            if line.starts_with(".") {
                let line = &line[1..];
                let i = line.find(' ').unwrap_or(line.len());
                let arg = &line[i+1..];
                match &line[..i] {
                    "string" => SourceLine::DirString({
                        let mut string = Vec::with_capacity(arg.len());
                        let mut arg = arg.as_bytes();
                        while !arg.is_empty() {
                            let (c, rest) = parse_bytechar(arg);
                            arg = rest;
                            string.push(c);
                        }
                        string
                    }),
                    "byte" => SourceLine::DirByte(arg.parse().map_err(|_| Error::new(self.source.clone(), self.ln, ErrorType::Other(format!("invalid byte literal \'{arg}\'").into_boxed_str())))?),
                    "wide" | "word" => SourceLine::DirWide(arg.parse().map_err(|_| Error::new(self.source.clone(), self.ln, ErrorType::Other(format!("invalid wide literal \'{arg}\'").into_boxed_str())))?),
                    "include" => SourceLine::DirInclude(arg.to_string()),
                    "global" | "globl" => SourceLine::DirGlobal(arg.to_string()),
                    "ref" | "reference" => SourceLine::DirReference(arg.to_string()),
                    s => return Err(Error::new(self.source.clone(), self.ln, ErrorType::UnknownDirective(s.into()))),
                }
            } else 
            if line.ends_with(":") {
                SourceLine::Label((line[..line.len()-1]).to_owned())
            } else 
            if let Some(i) = line.find(' ') {
                let (ins, args) = line.split_at(i);
                let mut sos = Vec::new();

                for arg in args.split(',') {
                    let arg = arg.trim();

                    sos.push(match arg {
                        "al" => SourceOperand::ByteReg(BReg::Al),
                        "ah" => SourceOperand::ByteReg(BReg::Ah),
                        "bl" => SourceOperand::ByteReg(BReg::Bl),
                        "bh" => SourceOperand::ByteReg(BReg::Bh),
                        "cl" => SourceOperand::ByteReg(BReg::Cl),
                        "ch" => SourceOperand::ByteReg(BReg::Ch),
                        "io" => SourceOperand::ByteReg(BReg::Io),
                        "a" => SourceOperand::WideReg(WReg::A),
                        "b" => SourceOperand::WideReg(WReg::B),
                        "c" => SourceOperand::WideReg(WReg::C),
                        "x" => SourceOperand::WideReg(WReg::X),
                        "y" => SourceOperand::WideReg(WReg::Y),
                        "z" => SourceOperand::WideReg(WReg::Z),
                        "s" => SourceOperand::WideReg(WReg::S),
                        arg => {
                            let so;
                            if arg.ends_with("b") {
                                so = arg[..arg.len()-1]
                                    .parse()
                                    .ok()
                                    .or_else(|| arg[..arg.len()-1].parse::<i8>().ok().map(|b| b as u8))
                                    .map(SourceOperand::Byte);
                            } else if arg.ends_with("w") {
                                so = arg[..arg.len()-1]
                                    .parse()
                                    .ok()
                                    .or_else(|| arg[..arg.len()-1].parse::<i16>().ok().map(|w| w as u16))
                                    .map(SourceOperand::Wide);
                            } else if arg.starts_with('\'') && arg.ends_with('\'') {
                                so = Some(SourceOperand::Byte(parse_bytechar(arg[1..arg.len()-1].as_bytes()).0));
                            } else {
                                so = arg.parse().ok().map(SourceOperand::Number);
                            }

                            if let Some(so) = so {
                                so
                            } else {
                                SourceOperand::Label(arg.to_owned())
                            }
                        }
                    });
                }

                SourceLine::Ins(ins.to_owned(), sos)
            } else {
                SourceLine::Ins(line.to_owned(), Vec::new())
            }
        })
    }
}

fn parse_bytechar(s: &[u8]) -> (u8, &[u8]) {
    let mut bs = s.iter();
    match bs.next().unwrap() {
        b'\\' => match bs.next().unwrap() {
            b'r' => (b'\r', &s[2..]),
            b't' => (b'\t', &s[2..]),
            b'n' => (b'\n', &s[2..]),
            b'0' => (b'\0', &s[2..]),
            b'\\' => (b'\\', &s[2..]),
            b'\'' => (b'\'', &s[2..]),
            b'\"' => (b'\"', &s[2..]),
            b'x' => (u8::from_str_radix(String::from_utf8_lossy(&s[2..4]).as_ref(), 16).expect("invalid escape argument"), &s[4..]),
            c => panic!("invalid escape character \\{c}"),
        }
        &c => (c, &s[1..]),
    }
}

impl<B: BufRead> Iterator for SourceLines<B> {
    type Item = Result<(LineNumber, SourceLine)>;
    fn next(&mut self) -> Option<Self::Item> {
        Some({
            let line = self.lines.next()?;
            self.parse_line(line).map(|l| (self.ln, l))
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    source: Box<str>,
    line_number: LineNumber,
}
impl SourceLocation {
    fn new(src: &str, ln: u32) -> SourceLocation {
        SourceLocation { source: src.into(), line_number: ln }
    }
}

#[derive(Debug, Clone)]
pub enum DataLine {
    Ins(Opcode, DataOperand),
    Raw(Vec<u8>),
}

pub fn process<B: BufRead>(lines: SourceLines<B>) -> Result<(Vec<(Box<str>, SymbolType, u16)>, Vec<DataLine>)> {
    let mut symbols = Symbols::new();

    let data_lines = inner_process(lines, &mut 0, &mut symbols)?;

    let mut labels = Vec::with_capacity(symbols.size());

    for (l, st, r) in symbols.into_iter() {
        let element;
        use self::SymbolType::*;

        match r {
            Ok(pos) => {
                let st = match st {
                    Internal | Global => st,
                    Reference => return Err(Error::new(<Box<str>>::from("<src>"), 0, ErrorType::Other(format!("Symbol {l} is declared as reference but defined at 0x{pos:02x}").into_boxed_str()))),
                };

                element = (l, st, pos)
            }
            Err(e) => {
                match st {
                    Internal => {
                        let e = e
                            .into_iter()
                            .map(|SourceLocation { source, line_number }| Error::new(source, line_number, ErrorType::Other(
                                    format!("non-global label {l} was never defined, but used here").into_boxed_str()
                                )))
                            // Reversed order to make it faster (since it's a linked list)
                            .reduce(|accum, item| item.chain(accum))
                            .expect("ghost label, expected at least one use location")
                            ;
                        return Err(e);
                    },
                    Reference | Global => {
                        element = (l, Reference, 0xffaf);
                    }
                    
                }
            }
        }

        labels.push(element);
    }

    Ok((labels, data_lines))
}
fn inner_process<B: BufRead>(lines: SourceLines<B>, cur_offset: &mut u16, symbols: &mut Symbols) -> Result<Vec<DataLine>> {
    let mut data_lines = Vec::new();

    let src = lines.source.clone();

    for line in lines {
        let (ln, line) = line?;
        match line {
            SourceLine::Label(s) => {
                symbols.set_label(&s, *cur_offset, SourceLocation::new(&src, ln))?;
            }
            SourceLine::Ins(s, ops) => {
                let (opcode, dat_op) = parse_ins(s, ops, symbols, SourceLocation::new(&src, ln)).map_err(|e| Error::new(src.clone(), ln, ErrorType::Other(e.into())))?;
                *cur_offset += 1 + dat_op.size();
                data_lines.push(DataLine::Ins(opcode, dat_op));
            }
            SourceLine::DirByte(b) => {
                *cur_offset += 1;
                data_lines.push(DataLine::Raw(vec![b]));
            }
            SourceLine::DirWide(w) => {
                let [l, h] = w.to_le_bytes();
                *cur_offset += 2;
                data_lines.push(DataLine::Raw(vec![l, h]));
            }
            SourceLine::DirString(s) => {
                *cur_offset += s.len() as u16;
                data_lines.push(DataLine::Raw(s));
            }
            SourceLine::DirInclude(path) => {
                eprintln!("warning: {src}:{ln}: use of deprecated include directive");
                let pth_buf;

                let path = 
                    if path.starts_with("/") {
                        Path::new(&path[1..])
                    } else {
                        pth_buf = Path::new(&*src).with_file_name("").join(&path);
                        &pth_buf
                    };

                let lines = SourceLines::new(path)?;
                let old_symbols = symbols.marker();
                let included_data_lines = inner_process(lines, cur_offset, symbols)?;

                data_lines.extend(included_data_lines);
                symbols.mangle_interal(path.display(), old_symbols);
            }
            SourceLine::DirGlobal(l) => {
                let id = symbols.get_label(&l, SourceLocation::new(&src, ln));
                symbols.set_global(id);
            }
            SourceLine::DirReference(l) => {
                let id = symbols.get_label(&l, SourceLocation::new(&src, ln));
                symbols.set_reference(id);
            }
            SourceLine::Comment => (),
        }
    }

    Ok(data_lines)
}

fn parse_ins(s: String, ops: Vec<SourceOperand>, sym: &mut Symbols, sl: SourceLocation) -> StdResult<(u8, DataOperand), &'static str> {
    use self::isa::*;
    use self::DataOperand as O;
    let ops = ops.iter();
    Ok(match &*s {
        "null" => (NULL, O::parse_nothing(ops).ok_or("nothing")?),
        "halt" => (HALT, O::parse_nothing(ops).ok_or("nothing")?),
        "nop" => (NOP, O::parse_nothing(ops).ok_or("nothing")?),
        "push" => {
            if let Some(dat_op) = O::parse_b_big_r(ops.clone()) {
                (PUSH_B, dat_op)
            } else if let Some(dat_op) = O::parse_w_big_r(ops, sym, sl) {
                (PUSH_W, dat_op)
            } else {
                return Err("takes one big");
            }
        }
        "pop" => {
            if let Some(dat_op) = O::parse_breg(ops.clone()) {
                (POP_B, dat_op)
            } else if let Some(dat_op) = O::parse_wreg(ops) {
                (POP_W, dat_op)
            } else {
                return Err("takes one big");
            }
        }
        "call" => (CALL, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "ret" => (RET, O::parse_nothing(ops.clone()).map(|_| DataOperand::ImmediateByte(0)).or_else(|| O::parse_immediate_u8(ops)).ok_or("either nothing or a byte")?),
        "store" => {
            if let Some(dat_op) = O::parse_wide_big_byte(ops.clone(), sym, sl.clone()) {
                (STORE_B, dat_op)
            } else if let Some(dat_op) = O::parse_wide_big_wide(ops, sym, sl) {
                (STORE_W, dat_op)
            } else {
                return Err("a wide and a big for destination and a source register (any size)");
            }
        }
        "load" => {
             if let Some(dat_op) = O::parse_byte_wide_big(ops.clone(), sym, sl.clone()) {
                (LOAD_B, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_one_big(ops.clone(), sym, sl) {
                (LOAD_W, dat_op)
            } else {
                return Err("a destination register (any size) and then a wide and a big");
            }
        }
        "jmp" | "jump" => {
             if let Some(dat_op) = O::parse_immediate_u16(ops.clone(), sym, sl) {
                (JUMP, dat_op)
            } else if let Some(dat_op) = O::parse_wreg(ops) {
                (JUMP_REG, dat_op)
            } else {
                return Err("address or wide register");
            }
        }

        "jez" => (JEZ, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jlt" => (JLT, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jle" => (JLE, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jgt" => (JGT, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jge" => (JGE, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jnz" | "jne" => (JNZ, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jo" => (JO, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jno" => (JNO, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jb" | "jc" => (JB, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jae" | "jnc" => (JAE, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "ja" => (JA, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),
        "jbe" => (JBE, O::parse_immediate_u16(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?),

        "add" => {
            if let Some(dat_op) = O::parse_two_byte_one_big(ops.clone()) {
                (ADD_B, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_one_big(ops, sym, sl) {
                (ADD_W, dat_op)
            } else {
                return Err("two regs and one big");
            }
        }
        "sub" => {
            if let Some(dat_op) = O::parse_two_byte_one_big(ops.clone()) {
                (SUB_B, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_one_big(ops, sym, sl) {
                (SUB_W, dat_op)
            } else {
                return Err("two regs and one big");
            }
        }
        "and" => {
            if let Some(dat_op) = O::parse_two_byte_one_big(ops.clone()) {
                (AND_B, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_one_big(ops, sym, sl) {
                (AND_W, dat_op)
            } else {
                return Err("two regs and one big");
            }
        }
        "or" => {
            if let Some(dat_op) = O::parse_two_byte_one_big(ops.clone()) {
                (OR_B, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_one_big(ops, sym, sl) {
                (OR_W, dat_op)
            } else {
                return Err("two regs and one big");
            }
        }
        "xor" => {
            if let Some(dat_op) = O::parse_two_byte_one_big(ops.clone()) {
                (XOR_B, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_one_big(ops, sym, sl) {
                (XOR_W, dat_op)
            } else {
                return Err("two regs and one big");
            }
        }
        "mul" => {
            if let Some(dat_op) = O::parse_four_byte(ops.clone()) {
                (MUL_B, dat_op)
            } else if let Some(dat_op) = O::parse_four_wide(ops) {
                (MUL_W, dat_op)
            } else {
                return Err("four registers")
            }
        }
        "div" => {
            if let Some(dat_op) = O::parse_four_byte(ops.clone()) {
                (DIV_B, dat_op)
            } else if let Some(dat_op) = O::parse_four_wide(ops) {
                (DIV_W, dat_op)
            } else {
                return Err("four registers");
            }
        }
        // TODO: BAD
        _ => return Err(Box::leak(format!("unknown instruction {s}").into_boxed_str()))
    })
}

fn big_r_to_byte(br: BBigR) -> StdResult<u8, &'static str> {
    Ok(match br {
        BBigR::Register(r) => r as u8,
        BBigR::Byte(0) => BReg::Zero as u8,
        // Since this b is a number from 1 up to 247, we can just add 7 to encode it between 0x08 and 0xff
        BBigR::Byte(b) => b.checked_add(7).ok_or("immediate between 1-247")?,
    })
}
fn big_r_to_wide<F: FnOnce(usize, LabelRead) -> u16>(wr: WBigR, read_label: F, position: u16) -> StdResult<[u8; 2], &'static str> {
    Ok(match wr {
        WBigR::Register(r) => r as u16,
        WBigR::Wide(w) => {
            let w = parse_wide(w, read_label, position, Format::Big);
            if w == 0 {
                WReg::Zero as u16
            } else {
                // Since this w is a number from 1 up to 65527, we can just add 7 to encode it between 0x08 and 0xffff
                w.checked_add(7).ok_or("immediate between 1-247")?
            }
        }
    }.to_le_bytes())
}

fn parse_wide<F: FnOnce(usize, LabelRead) -> u16>(w: Wide, read_label: F, position: u16, format: Format) -> u16 {
    match w {
        Wide::Label(l) => read_label(l, LabelRead { position, format }),
        Wide::Number(n) => n,
    }
}

fn parse_wide_abs<F: FnOnce(usize, LabelRead) -> u16>(w: Wide, read_label: F, position: u16) -> u16 {
    parse_wide(w, read_label, position, Format::Absolute)
}

pub fn write_data_operand<F: FnOnce(usize, LabelRead) -> u16>(mem: &mut Vec<u8>, read_label: F, dat_op: DataOperand) -> StdResult<(), &'static str> {
    use self::DataOperand::*;

    let position = mem.len() as u16;

    match dat_op {
        Nothing => (),
        ByteBigR(br) => mem.push(big_r_to_byte(br)?),
        WideBigR(wr) => mem.extend_from_slice(&big_r_to_wide(wr, read_label, position)?),
        ByteRegister(r) => mem.push((r as u8) << 4),
        WideRegister(r) => mem.push((r as u8) << 4),
        ImmediateByte(b) => {
            mem.push(b);
        }
        ImmediateWide(w) => {
            mem.extend_from_slice(&parse_wide_abs(w, read_label, position).to_le_bytes());
        }
        TwoByteOneBig(r1, r2, br) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(big_r_to_byte(br)?);
        }
        WideBigByte(r1, wr, r2) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, read_label, position+1)?);
        }
        ByteWideBig(r1, r2, wr) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, read_label, position+1)?);
        }
        WideBigWide(r1, wr, r2) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, read_label, position+1)?);
        }
        TwoWideOneBig(r1, r2, wr) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, read_label, position+1)?);
        }
        FourByte(r1, r2, r3, r4) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(((r3 as u8) << 4) | r4 as u8);
        }
        FourWide(r1, r2, r3, r4) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(((r3 as u8) << 4) | r4 as u8);
        }
    }

    Ok(())
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Wide {
    Number(u16),
    Label(usize),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BBigR {
    Register(BReg),
    Byte(u8),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WBigR {
    Register(WReg),
    Wide(Wide),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DataOperand {
    Nothing,
    ByteBigR(BBigR),
    WideBigR(WBigR),
    ByteRegister(BReg),
    WideRegister(WReg),
    ImmediateByte(u8),
    ImmediateWide(Wide),
    TwoByteOneBig(BReg, BReg, BBigR),
    TwoWideOneBig(WReg, WReg, WBigR),
    WideBigWide(WReg, WBigR, WReg),
    ByteWideBig(BReg, WReg, WBigR),
    WideBigByte(WReg, WBigR, BReg),
    FourByte(BReg, BReg, BReg, BReg),
    FourWide(WReg, WReg, WReg, WReg),
}

impl DataOperand {
    fn size(&self) -> u16 {
        use self::DataOperand::*;
        match self {
            Nothing => 0,
            ByteBigR(_) => 1,
            WideBigR(_) => 2,
            ByteRegister(_) => 1,
            WideRegister(_) => 1,
            ImmediateByte(_) => 1,
            ImmediateWide(_) => 2,
            TwoByteOneBig(_, _, _) => 2,
            TwoWideOneBig(_, _, _) => 3,
            WideBigWide(_, _, _) => 3,
            ByteWideBig(_, _, _) => 3,
            WideBigByte(_, _, _) => 3,
            FourByte(_, _, _, _) => 2,
            FourWide(_, _, _, _) => 2,
        }
    }
    fn parse_nothing<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        if ops.next().is_none() {
            Some(DataOperand::Nothing)
        } else { None }
    }
    fn parse_breg<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        let breg = Self::byte(ops.next()?)?;
        Self::parse_nothing(ops)?;
        Some(DataOperand::ByteRegister(breg))
    }
    fn parse_wreg<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        let wreg = Self::wide(ops.next()?)?;
        Self::parse_nothing(ops)?;
        Some(DataOperand::WideRegister(wreg))
    }
    fn parse_immediate_u8<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        let ret = Some(DataOperand::ImmediateByte(Self::imm_byte(ops.next()?)?));
        Self::parse_nothing(ops)?;
        ret
    }
    fn parse_immediate_u16<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>, sym: &mut Symbols, sl: SourceLocation) -> Option<DataOperand> {
        let ret = Some(DataOperand::ImmediateWide(Self::imm_wide(ops.next()?, sym, sl)?));
        Self::parse_nothing(ops)?;
        ret
    }
    fn parse_b_big_r<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        let ret = Some(DataOperand::ByteBigR(Self::byte_or_imm(ops.next()?)?));
        Self::parse_nothing(ops)?;
        ret
    }
    fn parse_w_big_r<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>, sym: &mut Symbols, sl: SourceLocation) -> Option<DataOperand> {
        let ret = Some(DataOperand::WideBigR(Self::wide_or_imm(ops.next()?, sym, sl)?));
        Self::parse_nothing(ops)?;
        ret
    }
    fn parse_two_byte_one_big<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        Some(DataOperand::TwoByteOneBig(Self::byte(reg1)?, Self::byte(reg2)?, Self::byte_or_imm(ops.next()?)?))
    }
    fn parse_two_wide_one_big<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>, sym: &mut Symbols, sl: SourceLocation) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        Some(DataOperand::TwoWideOneBig(Self::wide(reg1)?, Self::wide(reg2)?, Self::wide_or_imm(ops.next()?, sym, sl)?))
    }
    fn parse_wide_big_byte<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>, sym: &mut Symbols, sl: SourceLocation) -> Option<DataOperand> {
        Some(DataOperand::WideBigByte(
            Self::wide(ops.next()?)?,
            Self::wide_or_imm(ops.next()?, sym, sl)?,
            Self::byte(ops.next()?)?,
        ))
    }
    fn parse_wide_big_wide<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>, sym: &mut Symbols, sl: SourceLocation) -> Option<DataOperand> {
        Some(DataOperand::WideBigWide(
            Self::wide(ops.next()?)?,
            Self::wide_or_imm(ops.next()?, sym, sl)?,
            Self::wide(ops.next()?)?,
        ))
    }
    fn parse_byte_wide_big<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>, sym: &mut Symbols, sl: SourceLocation) -> Option<DataOperand> {
        Some(DataOperand::ByteWideBig(
            Self::byte(ops.next()?)?,
            Self::wide(ops.next()?)?,
            Self::wide_or_imm(ops.next()?, sym, sl)?,
        ))
    }
    fn parse_four_byte<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        let reg3 = ops.next()?;
        let reg4 = ops.next()?;
        Self::parse_nothing(ops);
        Some(DataOperand::FourByte(Self::byte(reg1)?, Self::byte(reg2)?, Self::byte(reg3)?, Self::byte(reg4)?))
    }
    fn parse_four_wide<'a>(mut ops: impl Iterator<Item=&'a SourceOperand>) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        let reg3 = ops.next()?;
        let reg4 = ops.next()?;
        Self::parse_nothing(ops);
        Some(DataOperand::FourWide(Self::wide(reg1)?, Self::wide(reg2)?, Self::wide(reg3)?, Self::wide(reg4)?))
    }

    fn byte(op: &SourceOperand) -> Option<BReg> {
        match op {
            SourceOperand::Number(0) => Some(BReg::Zero),
            &SourceOperand::ByteReg(r) => Some(r),
            _ => None,
        }
    }
    fn wide(op: &SourceOperand) -> Option<WReg> {
        match op {
            SourceOperand::Number(0) => Some(WReg::Zero),
            &SourceOperand::WideReg(r) => Some(r),
            _ => None,
        }
    }
    fn imm_byte(op: &SourceOperand) -> Option<u8> {
        match op {
            &SourceOperand::Number(n) => Some(n as u8),
            &SourceOperand::Byte(n) => Some(n),
            _ => None,
        }
    }
    fn imm_wide(op: &SourceOperand, sym: &mut Symbols, sl: SourceLocation) -> Option<Wide> {
        match op {
            &SourceOperand::Number(n) => Some(Wide::Number(n as u16)),
            &SourceOperand::Wide(n) => Some(Wide::Number(n)),
            SourceOperand::Label(lbl) => Some(Wide::Label(sym.get_label(lbl, sl))),
            _ => None,
        }
    }
    fn byte_or_imm(op: &SourceOperand) -> Option<BBigR> {
        Self::byte(op)
            .map(BBigR::Register)
            .or_else(|| Self::imm_byte(op).map(BBigR::Byte))
    }
    fn wide_or_imm(op: &SourceOperand, sym: &mut Symbols, sl: SourceLocation) -> Option<WBigR> {
        Self::wide(op)
            .map(WBigR::Register)
            .or_else(|| Self::imm_wide(op, sym, sl).map(WBigR::Wide))
    }
}
