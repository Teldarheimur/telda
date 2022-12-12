use std::{path::{Path}, io::{self, BufRead, Write, Read, BufReader}, fs::File, fmt::Display};

use crate::{mem::Lazy, source::Format};

use super::{Segment, read_aalv_file, Aalv, write_aalv_file};

pub const AALV_OBJECT_EXT: &str = "to";

#[derive(Debug)]
pub enum ShebangAgnosticObject {
    WithShebang(String, Object),
    Without(Object),
}

impl ShebangAgnosticObject {
    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();

        match Object::from_file(path) {
            Ok(obj) => Ok(Self::Without(obj)),
            // TODO: check that it's the right error
            Err(_) => {
                let (mut shebang, obj) = Object::from_file_ignoring_shebang(path)?;
                shebang.pop();

                Ok(Self::WithShebang(shebang, obj))
            }
        }
    }
    pub fn obj_mut(&mut self) -> &mut Object {
        match self {
            ShebangAgnosticObject::WithShebang(_, o) => o,
            ShebangAgnosticObject::Without(o) => o,
        }
    }
    pub fn into_object(self) -> Object {
        match self {
            ShebangAgnosticObject::WithShebang(_, o) => o,
            ShebangAgnosticObject::Without(o) => o,
        }
    }
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        match self {
            ShebangAgnosticObject::WithShebang(shebang, o) => o.write_to_file_with_shebang(path, shebang),
            ShebangAgnosticObject::Without(o) => o.write_to_file(path),
        }
    }
}

#[derive(Debug, Default)]
pub struct Object {
    pub mem: Option<Lazy>,
    pub global_symbols: Option<GlobalSymbols>,
    pub internal_symbols: Option<InternalSymbols>,
    pub symbol_reference_table: Option<SymbolReferenceTable>,
}

impl Object {
    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let aalvur = read_aalv_file(path)?;

        Ok(Object {
            mem: aalvur.get_segment().transpose().cannot_fail(),
            global_symbols: aalvur.get_segment().transpose()?,
            internal_symbols: aalvur.get_segment().transpose()?,
            symbol_reference_table: aalvur.get_segment().transpose()?,
        })
    }
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let Object {
            mem,
            global_symbols,
            internal_symbols,
            symbol_reference_table
        } = self;

        let mut aalvur = Aalv::new();

        if let Some(mem) = mem {
            aalvur.add_segment(mem).cannot_fail();
        }
        if let Some(global_symbols) = global_symbols {
            aalvur.add_segment(global_symbols)?;
        }
        if let Some(internal_symbols) = internal_symbols {
            aalvur.add_segment(internal_symbols)?;
        }
        if let Some(symbol_reference_table) = symbol_reference_table {
            aalvur.add_segment(symbol_reference_table)?;
        }

        write_aalv_file(path, &aalvur)
    }
}

impl Object {
    pub fn from_file_ignoring_shebang<P: AsRef<Path>>(path: P) -> io::Result<(String, Self)> {
        let mut f = BufReader::new(File::open(path)?);
        let mut shebang = String::new();
        f.read_line(&mut shebang)?;

        let aalvur = Aalv::read(f)?;

        Ok((shebang, Object {
            mem: aalvur.get_segment().transpose().cannot_fail(),
            global_symbols: aalvur.get_segment().transpose()?,
            internal_symbols: aalvur.get_segment().transpose()?,
            symbol_reference_table: aalvur.get_segment().transpose()?,
        }))
    }
    pub fn write_to_file_with_shebang<P: AsRef<Path>>(&self, path: P, shebang: impl Display) -> io::Result<()> {
        let Object {
            mem,
            global_symbols,
            internal_symbols,
            symbol_reference_table
        } = self;

        let mut aalvur = Aalv::new();

        if let Some(mem) = mem {
            aalvur.add_segment(mem).cannot_fail();
        }
        if let Some(global_symbols) = global_symbols {
            aalvur.add_segment(global_symbols)?;
        }
        if let Some(internal_symbols) = internal_symbols {
            aalvur.add_segment(internal_symbols)?;
        }
        if let Some(symbol_reference_table) = symbol_reference_table {
            aalvur.add_segment(symbol_reference_table)?;
        }

        let mut f = File::create(path)?;
        writeln!(f, "{shebang}")?;

        aalvur.write(f)
    }
}

impl Segment for Lazy {
    type Error = Infallible;

    const NAME: &'static str = ".mem";

    fn read(buf: &[u8]) -> Result<Self, Self::Error> {
        Ok(Lazy { mem: buf.to_vec() })
    }

    fn write(&self, buf: &mut Vec<u8>) -> Result<(), Self::Error> {
        buf.extend_from_slice(&self.mem);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct GlobalSymbols(pub Vec<(Box<str>, u16)>);
#[derive(Debug, Clone)]
pub struct InternalSymbols(pub Vec<(Box<str>, u16)>);
#[derive(Debug, Clone)]
pub struct SymbolReferenceTable(pub Vec<(Format, Box<str>, u16)>);

impl Segment for GlobalSymbols {
    type Error = io::Error;

    const NAME: &'static str = "symtab";

    fn read(mut buf: &[u8]) -> Result<Self, Self::Error> {
        let mut symbols = Vec::new();

        loop {
            let mut s = String::new();
            match buf.read_line(&mut s)? {
                0 => break,
                _ => ()
            }
            s.pop();

            let mut pos_buf = [0; 2];
            buf.read_exact(&mut pos_buf)?;
            symbols.push((s.into_boxed_str(), u16::from_le_bytes(pos_buf)));
        }

        Ok(Self(symbols))
    }

    fn write(&self, buf: &mut Vec<u8>) -> Result<(), Self::Error> {
        for (name, pos) in &self.0 {
            buf.extend_from_slice(name.as_bytes());
            buf.push(b'\n');
            buf.extend_from_slice(&pos.to_le_bytes());
        }

        Ok(())
    }
}

impl Segment for InternalSymbols {
    type Error = io::Error;

    const NAME: &'static str = "symtab.internal";

    fn read(mut buf: &[u8]) -> Result<Self, Self::Error> {
        let mut symbols = Vec::new();

        loop {
            let mut s = String::new();
            match buf.read_line(&mut s)? {
                0 => break,
                _ => ()
            }
            s.pop();

            let mut pos_buf = [0; 2];
            buf.read_exact(&mut pos_buf)?;
            symbols.push((s.into_boxed_str(), u16::from_le_bytes(pos_buf)));
        }

        Ok(Self(symbols))
    }

    fn write(&self, buf: &mut Vec<u8>) -> Result<(), Self::Error> {
        for (name, pos) in &self.0 {
            buf.extend_from_slice(name.as_bytes());
            buf.push(b'\n');
            buf.extend_from_slice(&pos.to_le_bytes());
        }

        Ok(())
    }
}

impl Segment for SymbolReferenceTable {
    type Error = io::Error;

    const NAME: &'static str = "symref";

    fn read(mut buf: &[u8]) -> Result<Self, Self::Error> {
        let mut symbols = Vec::new();

        loop {
            let mut s = String::new();
            match buf.read_line(&mut s)? {
                0 => break,
                _ => ()
            }
            s.pop();

            let mut pos_buf = [0; 2];
            buf.read_exact(&mut pos_buf)?;

            let mut format = [0; 1];
            buf.read_exact(&mut format)?;

            let format = match format {
                [0] => Format::Absolute,
                [1] => Format::Big,
                _ => return Err(io::Error::new(io::ErrorKind::InvalidData, "invalid symbol type number")),
            };

            symbols.push((format, s.into_boxed_str(), u16::from_le_bytes(pos_buf)));
        }

        Ok(Self(symbols))
    }

    fn write(&self, buf: &mut Vec<u8>) -> Result<(), Self::Error> {
        for (format, name, pos) in &self.0 {
            buf.extend_from_slice(name.as_bytes());
            buf.push(b'\n');
            buf.extend_from_slice(&pos.to_le_bytes());
            buf.push(match format {
                Format::Absolute => 0,
                Format::Big => 1,
            });
        }

        Ok(())
    }
}

trait ResultInfalliableExt<T> {
    fn cannot_fail(self) -> T;
}

#[derive(Debug, Clone, Copy)]
pub enum Infallible {}

impl<T> ResultInfalliableExt<T> for Result<T, Infallible> {
    #[inline(always)]
    fn cannot_fail(self) -> T {
        match self {
            Ok(i) => i,
            Err(i) => match i {},
        }
    }
}
