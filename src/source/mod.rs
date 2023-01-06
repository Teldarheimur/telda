use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader, Lines},
    path::Path,
    slice::Iter,
};

use crate::{cpu::{ByteRegister as BReg, WideRegister as WReg}, aalv::obj::Entry};
use crate::{aalv::obj::SegmentType, align, cpu::*, isa, SEGMENT_ALIGNMENT, U4};

mod err;
pub use self::err::*;
mod symbols;
use self::symbols::*;
pub use self::symbols::{LabelRead, SymbolType};

type Opcode = u8;

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
    DirWide(StdResult<u16, String>),
    DirGlobal(String),
    DirReference(String),
    DirSeg(String),
    DirEntry,
}

pub struct SourceLines<B> {
    lines: Lines<B>,
    ln: LineNumber,
    source: Box<str>,
    errors: Option<Error>
}

fn add_error_opt(errors: &mut Option<Error>, error: Error) {
    if let Some(cur_error) = errors.take() {
            *errors = Some(cur_error.chain(error));
        } else {
            *errors = Some(error);
        }
}

impl<B> SourceLines<B> {
    #[inline]
    fn add_error(&mut self, error: Error) {
        add_error_opt(&mut self.errors, error)
    }
}

impl SourceLines<BufReader<File>> {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let source = format!("{}", path.as_ref().display()).into_boxed_str();
        let f =
            File::open(path).map_err(|e| Error::new(source.clone(), 0, ErrorType::IoError(e)))?;
        let br = BufReader::new(f);
        Ok(SourceLines {
            lines: br.lines(),
            ln: 0,
            source,
            errors: None,
        })
    }
}

fn parse_number(arg: &str) -> StdResult<SourceOperand, ErrorType> {
    let so;
    let mut radix = 10;
    let mut num = arg;
    if let Some(new_num) = arg.strip_prefix("0x") {
        radix = 16;
        num = new_num;
    } else if let Some(new_num) = arg.strip_prefix("0b") {
        radix = 2;
        num = new_num;
    } else if let Some(new_num) = arg.strip_prefix("0o") {
        radix = 8;
        num = new_num;
    }

    if let Some(num) = num.strip_suffix('b') {
        so = u8::from_str_radix(num, radix)
            .ok()
            .or_else(|| i8::from_str_radix(num, radix).ok().map(|b| b as u8))
            .map(SourceOperand::Byte);
    } else if let Some(num) = num.strip_suffix('w') {
        so = u16::from_str_radix(num, radix)
            .ok()
            .or_else(|| i16::from_str_radix(num, radix).ok().map(|w| w as u16))
            .map(SourceOperand::Wide);
    } else if let Some(arg) = arg.strip_prefix('\'').and_then(|a| a.strip_suffix('\'')) {
        let (byte, rest) = parse_bytechar(arg.as_bytes())?;
        if !rest.is_empty() {
            return Err(ErrorType::CharacterLiteralTooLong);
        }

        so = Some(SourceOperand::Byte(byte));
    } else {
        so = i32::from_str_radix(num, radix).ok().map(SourceOperand::Number);
    }

    Ok(if let Some(so) = so {
        so
    } else {
        SourceOperand::Label(arg.to_owned())
    })
}

impl<B: BufRead> SourceLines<B> {
    pub fn from_reader(r: B) -> Self {
        SourceLines {
            lines: r.lines(),
            ln: 0,
            source: "<input>".into(),
            errors: None,
        }
    }
    pub fn parse_next_line(&mut self) -> Option<(u32, SourceLine)> {
        loop {
            let line = self.lines.next()?;
            self.ln += 1;
            match self.inner_parse_line(line) {
                Ok(sl) => break Some((self.ln, sl)),
                Err(e) => {
                    self.add_error(e);
                }
            }
        }
    }
    fn inner_parse_line(&mut self, line: StdResult<String, IoError>) -> Result<SourceLine> {
        Ok({
            let line = line?;
            let line = line.trim();

            if line.is_empty() || line.starts_with(';') || line.starts_with("//") || line.starts_with('#') {
                SourceLine::Comment
            } else if let Some(line) = line.strip_prefix('.') {
                let (line_i, arg_i) = line
                    .find(' ')
                    .map(|i| (i, i + 1))
                    .unwrap_or((line.len(), line.len()));
                let arg = &line[arg_i..];
                match &line[..line_i] {
                    "string" => SourceLine::DirString({
                        let mut string = Vec::with_capacity(arg.len());
                        let mut arg = arg.as_bytes();
                        while !arg.is_empty() {
                            let (c, rest) = parse_bytechar(arg)
                                .map_err(|et| Error::new(self.source.clone(), self.ln, et))?;
                            arg = rest;
                            string.push(c);
                        }
                        string
                    }),
                    "byte" => {
                        let b;
                        match parse_number(arg).map_err(|et| Error::new(self.source.clone(), self.ln, et))? {
                            SourceOperand::Byte(n) => b = n,
                            SourceOperand::Number(n) => {
                                if n > u8::MAX as i32 {
                                    eprintln!("warning: byte literal overflow");
                                } else if n < i8::MIN as i32 {
                                    eprintln!("warning: byte literal underflow");
                                }

                                b = n as u8
                            }
                            _ => {
                                return Err(Error::new(
                                    self.source.clone(),
                                    self.ln,
                                    ErrorType::Other(
                                        format!("invalid byte literal \'{arg}\'").into_boxed_str(),
                                    ),
                                ))
                            }
                        }
                        SourceLine::DirByte(b)
                    }
                    "wide" | "word" => {
                        let w;
                        match parse_number(arg).map_err(|et| Error::new(self.source.clone(), self.ln, et))? {
                            SourceOperand::Wide(n) => w = Ok(n),
                            SourceOperand::Number(n) => {
                                if n > u16::MAX as i32 {
                                    eprintln!("warning: wide literal overflow");
                                } else if n < i16::MIN as i32 {
                                    eprintln!("warning: wide literal underflow");
                                }

                                w = Ok(n as u16)
                            }
                            SourceOperand::Label(l) => w = Err(l),
                            _ => {
                                return Err(Error::new(
                                    self.source.clone(),
                                    self.ln,
                                    ErrorType::Other(
                                        format!("invalid wide literal \'{arg}\'").into_boxed_str(),
                                    ),
                                ))
                            }
                        }
                        SourceLine::DirWide(w)
                    }
                    "include" => SourceLine::DirInclude(arg.to_string()),
                    "global" | "globl" => SourceLine::DirGlobal(arg.to_string()),
                    "ref" | "reference" => SourceLine::DirReference(arg.to_string()),
                    "seg" => SourceLine::DirSeg(arg.to_string()),
                    "entry" => SourceLine::DirEntry,
                    s => {
                        return Err(Error::new(
                            self.source.clone(),
                            self.ln,
                            ErrorType::UnknownDirective(s.into()),
                        ))
                    }
                }
            } else if let Some(line) = line.strip_suffix(':') {
                SourceLine::Label(line.to_owned())
            } else if let Some(i) = line.find(' ') {
                let (ins, args) = line.split_at(i);
                let mut sos = Vec::new();

                for arg in args.split(',') {
                    let arg = arg.trim();

                    sos.push(match arg {
                        "r0b" => SourceOperand::ByteReg(R0B),
                        "r1l" => SourceOperand::ByteReg(R1L),
                        "r1h" => SourceOperand::ByteReg(R1H),
                        "r2l" => SourceOperand::ByteReg(R2L),
                        "r2h" => SourceOperand::ByteReg(R2H),
                        "r3l" => SourceOperand::ByteReg(R3L),
                        "r3h" => SourceOperand::ByteReg(R3H),
                        "r4l" => SourceOperand::ByteReg(R4L),
                        "r4h" => SourceOperand::ByteReg(R4H),
                        "r5l" => SourceOperand::ByteReg(R5L),
                        "r5h" => SourceOperand::ByteReg(R5H),
                        "r6b" => SourceOperand::ByteReg(R6B),
                        "r7b" => SourceOperand::ByteReg(R7B),
                        "r8b" => SourceOperand::ByteReg(R8B),
                        "r9b" => SourceOperand::ByteReg(R9B),
                        "r10b" => SourceOperand::ByteReg(R10B),
                        "r0" => SourceOperand::WideReg(R0),
                        "r1" => SourceOperand::WideReg(R1),
                        "r2" => SourceOperand::WideReg(R2),
                        "r3" => SourceOperand::WideReg(R3),
                        "r4" => SourceOperand::WideReg(R4),
                        "r5" => SourceOperand::WideReg(R5),
                        "r6" => SourceOperand::WideReg(R6),
                        "r7" => SourceOperand::WideReg(R7),
                        "r8" => SourceOperand::WideReg(R8),
                        "r9" => SourceOperand::WideReg(R9),
                        "r10" => SourceOperand::WideReg(R10),
                        "rs" => SourceOperand::WideReg(RS),
                        "rl" => SourceOperand::WideReg(RL),
                        "rf" => SourceOperand::WideReg(RF),
                        "rp" => SourceOperand::WideReg(RP),
                        "rh" => SourceOperand::WideReg(RH),
                        arg => parse_number(arg).map_err(|et| Error::new(self.source.clone(), self.ln, et))?,
                    });
                }

                SourceLine::Ins(ins.to_owned(), sos)
            } else {
                SourceLine::Ins(line.to_owned(), Vec::new())
            }
        })
    }
}

fn parse_bytechar(s: &[u8]) -> StdResult<(u8, &[u8]), ErrorType> {
    use self::ErrorType::*;

    let mut bs = s.iter();
    Ok(match bs.next().ok_or(UnexpectedEndOfString)? {
        b'\\' => match bs.next().ok_or(EscapeCharacterAtEnd)? {
            b'r' => (b'\r', &s[2..]),
            b't' => (b'\t', &s[2..]),
            b'n' => (b'\n', &s[2..]),
            b'0' => (b'\0', &s[2..]),
            b'\\' => (b'\\', &s[2..]),
            b'\'' => (b'\'', &s[2..]),
            b'\"' => (b'\"', &s[2..]),
            b'x' => (
                u8::from_str_radix(String::from_utf8_lossy(&s[2..4]).as_ref(), 16)
                    .map_err(|_| InvalidEscapeSequence)?,
                &s[4..],
            ),
            c => return Err(InvalidEscapeCharacter(*c)),
        },
        &c => (c, &s[1..]),
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    source: Box<str>,
    line_number: LineNumber,
}
impl SourceLocation {
    fn new(src: &str, ln: u32) -> SourceLocation {
        SourceLocation {
            source: src.into(),
            line_number: ln,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DataLine {
    Ins(Opcode, DataOperand),
    Wide(Wide),
    Raw(Vec<u8>),
}

#[derive(Debug, Clone)]
pub struct ProcessedSource {
    pub labels: Vec<(Box<str>, SymbolType, SegmentType, u16)>,
    pub dls: BTreeMap<SegmentType, DataLineSegment>,
    pub entry: Option<Entry>,
}

#[derive(Debug, Clone, Default)]
pub struct DataLineSegment {
    pub lines: Vec<DataLine>,
    pub size: u16,
    pub start: u16,
}

struct ProcessState {
    dls: BTreeMap<SegmentType, DataLineSegment>,
    pub entry: Option<Address>,
}

impl ProcessState {
    fn new() -> Self {
        Self {
            dls: BTreeMap::new(),
            entry: None,
        }
    }
    fn get_size(&self, st: SegmentType) -> u16 {
        self.dls.get(&st).map(|dls| dls.size).unwrap_or(0)
    }
    fn add_line(&mut self, st: SegmentType, line: DataLine, size: u16) {
        let dls = self.dls.entry(st).or_default();
        dls.lines.push(line);
        dls.size += size;
    }
    fn unknown_defined(&self) -> bool {
        self.dls.contains_key(&SegmentType::Unknown)
    }
}

pub fn process<B: BufRead>(lines: SourceLines<B>) -> Result<ProcessedSource> {
    let mut symbols = Symbols::new();
    let mut state = ProcessState::new();

    let src = lines.source.clone();

    let mut errors = inner_process(lines, &mut state, &mut symbols);

    let ProcessState { mut dls, entry } = state;

    let mut last_end = SEGMENT_ALIGNMENT;
    for s in dls.values_mut() {
        s.start = align(last_end, SEGMENT_ALIGNMENT);
        last_end = s.start + s.size;
    }

    let mut labels = Vec::with_capacity(symbols.size());

    for (l, st, r) in symbols.into_iter() {
        let element;
        use self::SymbolType::*;

        match r {
            Ok(addr) => {
                let st = match st {
                    Internal | Global => st,
                    Reference => {
                        add_error_opt(&mut errors, Error::new(
                            src.clone(),
                            0,
                            ErrorType::Other(
                                format!(
                                    "Symbol `{l}' is declared as reference but defined at {addr}"
                                )
                                .into_boxed_str(),
                            ),
                        ));
                        continue;
                    }
                };

                let stype = addr.0;
                let offset = dls.get(&stype).map(|dl| dl.start).unwrap_or(0);
                let pos = addr.1 + offset;

                element = (l, st, stype, pos)
            }
            Err(e) => {
                match st {
                    Internal => {
                        let e = e
                            .into_iter()
                            .map(|SourceLocation { source, line_number }| Error::new(source, line_number, ErrorType::Other(
                                    format!("non-global label `{l}' was never defined, but used here").into_boxed_str()
                                )))
                            // Reversed order to make it faster (since it's a linked list)
                            .reduce(|accum, item| item.chain(accum))
                            .expect("ghost label, expected at least one use location")
                            ;
                        add_error_opt(&mut errors, e);
                        continue;
                    }
                    Reference | Global => {
                        element = (l, Reference, SegmentType::Unknown, 0xfaff);
                    }
                }
            }
        }

        labels.push(element);
    }

    if let Some(error) = errors {
        Err(error)
    } else {
        let entry = entry.map(|addr| {
            let offset = dls.get(&addr.0).map(|dl| dl.start).unwrap_or(0);
            Entry(addr.0, addr.1 + offset)
        });
    
        Ok(ProcessedSource {
            labels,
            dls,
            entry,
        })
    }

}
fn inner_process<B: BufRead>(
    mut lines: SourceLines<B>,
    state: &mut ProcessState,
    symbols: &mut Symbols,
) -> Option<Error> {
    fn inner_process_line(
        src: &Box<str>,
        ln: u32,
        line: SourceLine,
        current_segment: &mut SegmentType,
        state: &mut ProcessState,
        symbols: &mut Symbols,
    ) -> Result<()> {
        match line {
            SourceLine::DirSeg(seg) => {
                let new_seg = match &*seg {
                    "data" => SegmentType::Data,
                    "rodata" => SegmentType::RoData,
                    "text" => SegmentType::Text,
                    "heap" => SegmentType::Heap,
                    seg => {
                        return Err(Error::new(
                            src.clone(),
                            ln,
                            ErrorType::InvalidOperand(
                                format!(".seg {seg} not supported, unknown segment")
                                    .into_boxed_str(),
                            ),
                        ))
                    }
                };

                *current_segment = new_seg;
            }
            SourceLine::DirEntry => {
                if state.entry.is_some() {
                    return Err(Error::new(src.clone(), ln, ErrorType::DoubleEntry));
                }
                state.entry = Some(Address(*current_segment, state.get_size(*current_segment)));
            }
            SourceLine::Label(s) => {
                let addr = Address(*current_segment, state.get_size(*current_segment));
                symbols.set_label(&s, addr, SourceLocation::new(&src, ln))?;
            }
            SourceLine::Ins(s, ops) => {
                let (opcode, dat_op) = parse_ins(s, ops, symbols, SourceLocation::new(&src, ln))
                    .map_err(|e| Error::new(src.clone(), ln, ErrorType::Other(e.into())))?;
                state.add_line(*current_segment, DataLine::Ins(opcode, dat_op), 1 + dat_op.size());
            }
            SourceLine::DirByte(b) => {
                state.add_line(*current_segment, DataLine::Raw(vec![b]), 1);
            }
            SourceLine::DirWide(w) => {
                let wide = match w {
                    Ok(w) => Wide::Number(w),
                    Err(l) => {
                        Wide::Label(symbols.get_label(&l, SourceLocation::new(&src, ln)))
                    }
                };
                state.add_line(*current_segment, DataLine::Wide(wide), 2);
            }
            SourceLine::DirString(s) => {
                let size = s.len() as u16;
                state.add_line(*current_segment, DataLine::Raw(s), size);
            }
            SourceLine::DirInclude(path) => {
                let pth_buf;

                let path = if let Some(path) = path.strip_prefix('/') {
                    Path::new(path)
                } else {
                    pth_buf = Path::new(&**src).with_file_name("").join(&path);
                    &pth_buf
                };

                let lines = SourceLines::new(path)?;
                if let Some(e) = inner_process(lines, state, symbols) {
                    return Err(e);
                }
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

        if state.unknown_defined() {
            return Err(Error::new(
                src.clone(),
                ln,
                ErrorType::Other("no segment was started".to_string().into_boxed_str()),
            ));
        }

        Ok(())
    }


    let mut current_segment = SegmentType::Unknown;

    while let Some((ln, line)) = lines.parse_next_line() {
        match inner_process_line(&lines.source, ln, line, &mut current_segment, state, symbols) {
            Ok(()) => (),
            Err(e) => lines.add_error(e),
        }
    }

    lines.errors
}

fn parse_ins(
    s: String,
    ops: Vec<SourceOperand>,
    sym: &mut Symbols,
    sl: SourceLocation,
) -> StdResult<(u8, DataOperand), &'static str> {
    use self::isa::*;
    use self::DataOperand as O;
    let ops = ops.iter();
    Ok(match &*s {
        "null" => (NULL, O::parse_nothing(ops).ok_or("nothing")?),
        "halt" => (HALT, O::parse_nothing(ops).ok_or("nothing")?),
        "ctf" => (CTF, O::parse_nothing(ops).ok_or("nothing")?),
        "reth" => (RETH, O::parse_nothing(ops).ok_or("nothing")?),
        "nop" => (NOP, O::parse_nothing(ops).ok_or("nothing")?),
        "push" => {
            if let Some(dat_op) = O::parse_breg(ops.clone()) {
                (PUSH_B, dat_op)
            } else if let Some(dat_op) = O::parse_wreg(ops.clone()) {
                (PUSH_W, dat_op)
            } else {
                return Err("takes one register");
            }
        }
        "pop" => {
            if let Some(dat_op) = O::parse_breg(ops.clone()) {
                (POP_B, dat_op)
            } else if let Some(dat_op) = O::parse_wreg(ops) {
                (POP_W, dat_op)
            } else {
                return Err("takes one register");
            }
        }
        "call" => (
            CALL,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "ret" => (
            RET,
            O::parse_nothing(ops.clone())
                .map(|_| DataOperand::ImmediateByte(0))
                .or_else(|| O::parse_imm_byte(ops))
                .ok_or("either nothing or a byte")?,
        ),
        "store" | "str" => {
            if let Some(dat_op) = O::parse_wide_imm_byte(ops.clone(), sym, sl.clone()) {
                (STORE_BI, dat_op)
            } else if let Some(dat_op) = O::parse_wide_imm_wide(ops.clone(), sym, sl) {
                (STORE_WI, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_one_byte(ops.clone()) {
                (STORE_BR, dat_op)
            } else if let Some(dat_op) = O::parse_three_wide(ops.clone()) {
                (STORE_WR, dat_op)
            } else {
                return Err("a wide and another wide or immediate for destination and a source register (any size)");
            }
        }
        "load" => {
            if let Some(dat_op) = O::parse_byte_wide_imm(ops.clone(), sym, sl.clone()) {
                (LOAD_BI, dat_op)
            } else if let Some(dat_op) = O::parse_two_wide_imm(ops.clone(), sym, sl) {
                (LOAD_WI, dat_op)
            } else if let Some(dat_op) = O::parse_byte_two_wide(ops.clone()) {
                (LOAD_BR, dat_op)
            } else if let Some(dat_op) = O::parse_three_wide(ops.clone()) {
                (LOAD_WR, dat_op)
            } else {
                return Err("a destination register (any size) and then a wide and a wide or immediate for source");
            }
        }
        "jez" => (
            JEZ,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jlt" => (
            JLT,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jle" => (
            JLE,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jgt" => (
            JGT,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jge" => (
            JGE,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jnz" | "jne" => (
            JNZ,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jo" => (
            JO,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jno" => (
            JNO,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jb" | "jc" => (
            JB,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jae" | "jnc" => (
            JAE,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "ja" => (
            JA,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),
        "jbe" => (
            JBE,
            O::parse_imm_wide(ops, sym, sl).ok_or("a wide (addr like a label or just a number)")?,
        ),

        "ldi" => {
            if let Some(dat_op) = O::parse_byte_imm(ops.clone()) {
                (LDI_B, dat_op)
            } else if let Some(dat_op) = O::parse_wide_imm(ops.clone(), sym, sl) {
                let DataOperand::WideImm(r, w) = dat_op else { unreachable!() };

                (LDI_W, DataOperand::TwoWideImm(r, R0, w))
            } else {
                return Err("one register and one immediate");
            }
        }
        "jmp" | "jump" => {
            if let Some(dat_op) = O::parse_imm_wide(ops.clone(), sym, sl) {
                let DataOperand::ImmediateWide(w) = dat_op else { unreachable!() };

                (LDI_W, DataOperand::TwoWideImm(R0, R1, w))
            } else if let Some(dat_op) = O::parse_wreg(ops) {
                let DataOperand::WideRegister(wr) = dat_op else { unreachable!() };
                if wr == R0 {
                    return Err("r0 is not a valid jmp destination");
                }
                (LDI_W, DataOperand::TwoWideImm(wr, R1, Wide::Number(0)))
            } else {
                return Err("address or wide register");
            }
        }

        "add" => parse_binop(ADD_B, ADD_W, ops)?,
        "sub" => parse_binop(SUB_B, SUB_W, ops)?,
        "and" => parse_binop(AND_B, AND_W, ops)?,
        "or" => parse_binop(OR_B, OR_W, ops)?,
        "xor" => parse_binop(XOR_B, XOR_W, ops)?,
        "shl" => parse_binop(SHL_B, SHL_W, ops)?,
        "asr" => parse_binop(ASR_B, ASR_W, ops)?,
        "lsr" => parse_binop(LSR_B, LSR_W, ops)?,
        "mul" => {
            if let Some(dat_op) = O::parse_four_byte(ops.clone()) {
                (MUL_B, dat_op)
            } else if let Some(dat_op) = O::parse_four_wide(ops) {
                (MUL_W, dat_op)
            } else {
                return Err("four registers");
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
        _ => {
            return Err(Box::leak(
                format!("unknown instruction {s}").into_boxed_str(),
            ))
        }
    })
}

fn parse_binop(
    bop: u8,
    wop: u8,
    ops: Iter<SourceOperand>,
) -> StdResult<(u8, DataOperand), &'static str> {
    if let Some(dat_op) = DataOperand::parse_three_byte(ops.clone()) {
        Ok((bop, dat_op))
    } else if let Some(dat_op) = DataOperand::parse_three_wide(ops) {
        Ok((wop, dat_op))
    } else {
        Err("three regs of same size")
    }
}

fn parse_wide<F: FnOnce(usize, LabelRead) -> u16>(
    w: Wide,
    read_label: F,
    segment: SegmentType,
    position: u16,
) -> u16 {
    match w {
        Wide::Label(l) => read_label(l, LabelRead { segment, position }),
        Wide::Number(n) => n,
    }
}

pub fn write_data_operand<F: FnOnce(usize, LabelRead) -> u16>(
    st: SegmentType,
    mem: &mut Vec<u8>,
    read_label: F,
    dat_op: DataOperand,
) {
    use self::DataOperand::*;

    match dat_op {
        Nothing => (),
        ByteRegister(r) => mem.push(r.0.pair(U4::ZERO)),
        WideRegister(r) => mem.push(r.0.pair(U4::ZERO)),
        ImmediateByte(b) => {
            mem.push(b);
        }
        ImmediateWide(w) => {
            let position = mem.len() as u16;
            mem.extend_from_slice(&parse_wide(w, read_label, st, position).to_le_bytes());
        }
        ByteImm(r, b) => {
            mem.push(r.0.pair(U4::ZERO));
            mem.push(b);
        }
        WideImm(r, w) => {
            mem.push(r.0.pair(U4::ZERO));
            let position = mem.len() as u16;
            mem.extend_from_slice(&parse_wide(w, read_label, st, position).to_le_bytes());
        }
        WideImmByte(r1, w, r2) => {
            mem.push(r1.0.pair(r2.0));
            let position = mem.len() as u16;
            mem.extend_from_slice(&parse_wide(w, read_label, st, position).to_le_bytes());
        }
        WideImmWide(r1, w, r2) => {
            mem.push(r1.0.pair(r2.0));
            let position = mem.len() as u16;
            mem.extend_from_slice(&parse_wide(w, read_label, st, position).to_le_bytes());
        }
        TwoWideOneByte(r1, r2, r3) => {
            mem.push(r1.0.pair(r2.0));
            mem.push(r3.0.pair(U4::ZERO))
        }
        ByteWideImm(r1, r2, w) => {
            mem.push(r1.0.pair(r2.0));
            let position = mem.len() as u16;
            mem.extend_from_slice(&parse_wide(w, read_label, st, position).to_le_bytes());
        }
        TwoWideImm(r1, r2, w) => {
            mem.push(r1.0.pair(r2.0));
            let position = mem.len() as u16;
            mem.extend_from_slice(&parse_wide(w, read_label, st, position).to_le_bytes());
        }
        ByteTwoWide(r1, r2, r3) => {
            mem.push(r1.0.pair(r2.0));
            mem.push(r3.0.pair(U4::ZERO));
        }
        ThreeByte(r1, r2, r3) => {
            mem.push(r1.0.pair(r2.0));
            mem.push(r3.0.pair(U4::ZERO));
        }
        ThreeWide(r1, r2, r3) => {
            mem.push(r1.0.pair(r2.0));
            mem.push(r3.0.pair(U4::ZERO));
        }
        FourByte(r1, r2, r3, r4) => {
            mem.push(r1.0.pair(r2.0));
            mem.push(r3.0.pair(r4.0));
        }
        FourWide(r1, r2, r3, r4) => {
            mem.push(r1.0.pair(r2.0));
            mem.push(r3.0.pair(r4.0));
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Wide {
    Number(u16),
    Label(usize),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DataOperand {
    Nothing,
    ByteRegister(BReg),
    WideRegister(WReg),
    ImmediateByte(u8),
    ImmediateWide(Wide),
    ByteImm(BReg, u8),
    WideImm(WReg, Wide),
    WideImmByte(WReg, Wide, BReg),
    WideImmWide(WReg, Wide, WReg),
    TwoWideOneByte(WReg, WReg, BReg),
    ByteWideImm(BReg, WReg, Wide),
    TwoWideImm(WReg, WReg, Wide),
    ByteTwoWide(BReg, WReg, WReg),
    ThreeByte(BReg, BReg, BReg),
    ThreeWide(WReg, WReg, WReg),
    FourByte(BReg, BReg, BReg, BReg),
    FourWide(WReg, WReg, WReg, WReg),
}

impl DataOperand {
    fn size(&self) -> u16 {
        use self::DataOperand::*;
        match self {
            Nothing => 0,
            ByteRegister(_) => 1,
            WideRegister(_) => 1,
            ImmediateByte(_) => 1,
            ImmediateWide(_) => 2,
            ByteImm(_, _) => 2,
            WideImm(_, _) => 3,
            WideImmByte(_, _, _) => 3,
            WideImmWide(_, _, _) => 3,
            TwoWideOneByte(_, _, _) => 2,
            ByteWideImm(_, _, _) => 3,
            TwoWideImm(_, _, _) => 3,
            ByteTwoWide(_, _, _) => 2,
            ThreeByte(_, _, _) => 2,
            ThreeWide(_, _, _) => 2,
            FourByte(_, _, _, _) => 2,
            FourWide(_, _, _, _) => 2,
        }
    }
    fn parse_nothing<'a>(mut ops: impl Iterator<Item = &'a SourceOperand>) -> Option<DataOperand> {
        if ops.next().is_none() {
            Some(DataOperand::Nothing)
        } else {
            None
        }
    }
    fn parse_breg<'a>(mut ops: impl Iterator<Item = &'a SourceOperand>) -> Option<DataOperand> {
        let breg = Self::byte(ops.next()?)?;
        Self::parse_nothing(ops)?;
        Some(DataOperand::ByteRegister(breg))
    }
    fn parse_wreg<'a>(mut ops: impl Iterator<Item = &'a SourceOperand>) -> Option<DataOperand> {
        let wreg = Self::wide(ops.next()?)?;
        Self::parse_nothing(ops)?;
        Some(DataOperand::WideRegister(wreg))
    }
    fn parse_imm_byte<'a>(mut ops: impl Iterator<Item = &'a SourceOperand>) -> Option<DataOperand> {
        let ret = Some(DataOperand::ImmediateByte(Self::imm_byte(ops.next()?)?));
        Self::parse_nothing(ops)?;
        ret
    }
    fn parse_imm_wide<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
        sym: &mut Symbols,
        sl: SourceLocation,
    ) -> Option<DataOperand> {
        let ret = Some(DataOperand::ImmediateWide(Self::imm_wide(
            ops.next()?,
            sym,
            sl,
        )?));
        Self::parse_nothing(ops)?;
        ret
    }
    fn parse_byte_imm<'a>(mut ops: impl Iterator<Item = &'a SourceOperand>) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let imm = ops.next()?;
        Some(DataOperand::ByteImm(
            Self::byte(reg1)?,
            Self::imm_byte(imm)?,
        ))
    }
    fn parse_wide_imm<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
        sym: &mut Symbols,
        sl: SourceLocation,
    ) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let imm = ops.next()?;
        Some(DataOperand::WideImm(
            Self::wide(reg1)?,
            Self::imm_wide(imm, sym, sl)?,
        ))
    }
    fn parse_three_byte<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
    ) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        let reg3 = ops.next()?;
        Some(DataOperand::ThreeByte(
            Self::byte(reg1)?,
            Self::byte(reg2)?,
            Self::byte(reg3)?,
        ))
    }
    fn parse_three_wide<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
    ) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        let reg3 = ops.next()?;
        Some(DataOperand::ThreeWide(
            Self::wide(reg1)?,
            Self::wide(reg2)?,
            Self::wide(reg3)?,
        ))
    }
    fn parse_wide_imm_byte<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
        sym: &mut Symbols,
        sl: SourceLocation,
    ) -> Option<DataOperand> {
        let reg1 = Self::wide(ops.next()?)?;
        let imm = Self::imm_wide(ops.next()?, sym, sl)?;
        let reg2 = Self::byte(ops.next()?)?;
        Some(DataOperand::WideImmByte(reg1, imm, reg2))
    }
    fn parse_wide_imm_wide<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
        sym: &mut Symbols,
        sl: SourceLocation,
    ) -> Option<DataOperand> {
        let reg1 = Self::wide(ops.next()?)?;
        let imm = Self::imm_wide(ops.next()?, sym, sl)?;
        let reg2 = Self::wide(ops.next()?)?;
        Some(DataOperand::WideImmWide(reg1, imm, reg2))
    }
    fn parse_two_wide_one_byte<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
    ) -> Option<DataOperand> {
        Some(DataOperand::TwoWideOneByte(
            Self::wide(ops.next()?)?,
            Self::wide(ops.next()?)?,
            Self::byte(ops.next()?)?,
        ))
    }
    fn parse_byte_wide_imm<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
        sym: &mut Symbols,
        sl: SourceLocation,
    ) -> Option<DataOperand> {
        Some(DataOperand::ByteWideImm(
            Self::byte(ops.next()?)?,
            Self::wide(ops.next()?)?,
            Self::imm_wide(ops.next()?, sym, sl)?,
        ))
    }
    fn parse_two_wide_imm<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
        sym: &mut Symbols,
        sl: SourceLocation,
    ) -> Option<DataOperand> {
        Some(DataOperand::TwoWideImm(
            Self::wide(ops.next()?)?,
            Self::wide(ops.next()?)?,
            Self::imm_wide(ops.next()?, sym, sl)?,
        ))
    }
    fn parse_byte_two_wide<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
    ) -> Option<DataOperand> {
        Some(DataOperand::ByteTwoWide(
            Self::byte(ops.next()?)?,
            Self::wide(ops.next()?)?,
            Self::wide(ops.next()?)?,
        ))
    }
    fn parse_four_byte<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
    ) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        let reg3 = ops.next()?;
        let reg4 = ops.next()?;
        Self::parse_nothing(ops);
        Some(DataOperand::FourByte(
            Self::byte(reg1)?,
            Self::byte(reg2)?,
            Self::byte(reg3)?,
            Self::byte(reg4)?,
        ))
    }
    fn parse_four_wide<'a>(
        mut ops: impl Iterator<Item = &'a SourceOperand>,
    ) -> Option<DataOperand> {
        let reg1 = ops.next()?;
        let reg2 = ops.next()?;
        let reg3 = ops.next()?;
        let reg4 = ops.next()?;
        Self::parse_nothing(ops);
        Some(DataOperand::FourWide(
            Self::wide(reg1)?,
            Self::wide(reg2)?,
            Self::wide(reg3)?,
            Self::wide(reg4)?,
        ))
    }

    fn byte(op: &SourceOperand) -> Option<BReg> {
        match op {
            &SourceOperand::ByteReg(r) => Some(r),
            _ => None,
        }
    }
    fn wide(op: &SourceOperand) -> Option<WReg> {
        match op {
            &SourceOperand::WideReg(r) => Some(r),
            _ => None,
        }
    }
    fn imm_byte(op: &SourceOperand) -> Option<u8> {
        match *op {
            SourceOperand::Number(n) => Some(n as u8),
            SourceOperand::Byte(n) => Some(n),
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
}
