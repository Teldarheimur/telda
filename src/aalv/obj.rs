use std::{path::Path, io::{self, BufRead, Read, Write, BufReader}, collections::BTreeMap, fmt::{Display, self}};

use super::{Section, read_aalv_file, write_aalv_file_with_offset};

pub const AALV_OBJECT_EXT: &str = "to";

#[derive(Debug, Default)]
pub struct Object {
    pub file_offset: u64,
    pub entry: Option<Entry>,
    pub segs: BTreeMap<SegmentType, (u16, Vec<u8>)>,
    pub symbols: SymbolTable,
    pub relocation_table: RelocationTable,
}

impl Object {
    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let mut aalvur = read_aalv_file(path)?;

        let mut segs = BTreeMap::new();

        while let Some(seg) = aalvur.read_section() {
            let BinarySegment { offset, stype, bytes } = seg?;

            if segs.insert(stype, (offset, bytes)).is_some() {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "duplicate segment type"));
            }
        }

        let obj = Object {
            file_offset: aalvur.file_offset,
            entry: aalvur.read_section().transpose()?,
            segs,
            symbols: aalvur.read_section().transpose()?.unwrap_or_else(|| SymbolTable(Vec::new())),
            relocation_table: aalvur.read_section().transpose()?.unwrap_or_else(|| RelocationTable(Vec::new())),
        };

        if aalvur.remaing_sections().any(|s| s.starts_with('_')) {
            unimplemented!("error unexpected sections")
        } else {
            Ok(obj)
        }
    }
    pub fn zero_offset(self) -> Self {
        Self {
            file_offset: 0,
            .. self
        }
    }
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let Object {
            file_offset,
            entry,
            segs,
            symbols,
            relocation_table
        } = self;

        let mut aalvur = write_aalv_file_with_offset(path, *file_offset)?;

        if let Some(entry) = entry {
            aalvur.write_section(entry)?;
        }
        for (&stype, &(offset, ref bytes)) in segs {
            aalvur.write_section(&BinarySegment{stype, offset, bytes: bytes.clone()})?;
        }
        if !symbols.0.is_empty() {
            aalvur.write_section(symbols)?;
        }
        if !relocation_table.0.is_empty() {
            aalvur.write_section(relocation_table)?;
        }

        Ok(())
    }

    pub fn get_flattened_memory(&self) -> Vec<u8> {
        let size = self.segs.iter().map(|(_, &(o, ref v))| o as usize + v.len()).max().unwrap();
        let mut vec = vec![0; size];

        for (_, &(offset, ref bytes)) in &self.segs {
            vec[offset as usize.. offset as usize + bytes.len()].copy_from_slice(&bytes);
        }

        vec
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Entry(pub SegmentType, pub u16);

impl Section for Entry {
    const NAME: &'static str = "_entry";
    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = [0; 3];
        reader.read_exact(&mut buf)?;
        let [stype, loc @ ..] = buf;
        Ok(Entry(segment_type_from_u8(stype)?, u16::from_le_bytes(loc)))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        writer.write_all(&[self.0 as u8])?;
        writer.write_all(&self.1.to_le_bytes())
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SegmentType {
    Unknown = 0xff,
    Zero = 0,
    Data = 0x10,
    RoData = 0x18,
    Text = 0x20,
    Heap = 0x70,
}

impl Display for SegmentType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SegmentType::Unknown => write!(f, "unknown"),
            SegmentType::Zero => write!(f, "zero"),
            SegmentType::Data => write!(f, "data"),
            SegmentType::RoData => write!(f, "rodata"),
            SegmentType::Text => write!(f, "text"),
            SegmentType::Heap => write!(f, "heap"),
        }
    }
}

impl TryFrom<u8> for SegmentType {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use self::SegmentType::*;
        match value {
            0xff => Ok(Unknown),
            0x00 => Ok(Zero),
            0x10 => Ok(Data),
            0x18 => Ok(RoData),
            0x20 => Ok(Text),
            0x70 => Ok(Heap),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinarySegment{
    pub offset: u16,
    pub stype: SegmentType,
    pub bytes: Vec<u8>,
}

impl Section for BinarySegment {
    const NAME: &'static str = "_seg";

    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = [0; 3];
        reader.read_exact(&mut buf)?;
        let [ol, oh, stype] = buf;

        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes)?;

        Ok(Self{
            offset: u16::from_le_bytes([ol, oh]),
            stype: segment_type_from_u8(stype)?,
            bytes,
        })
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        writer.write_all(&self.offset.to_le_bytes())?;
        writer.write_all(&[self.stype as u8])?;
        writer.write_all(&self.bytes)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct SymbolDefinition {
    // No nulls, no initial whitespace
    pub name: Box<str>,
    pub is_global: bool,
    pub segment_type: SegmentType,
    pub location: u16,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable(pub Vec<SymbolDefinition>);

impl SymbolTable {
    pub fn from_iter(iter: impl Iterator<Item=SymbolDefinition>) -> Self {
        SymbolTable(iter.collect())
    }
    pub fn mutate<F: FnMut(&mut Box<str>, &mut bool, &mut SegmentType, &mut u16)>(&mut self, mut f: F) {
        for SymbolDefinition { name, is_global, segment_type, location } in &mut self.0 {
            f(name, is_global, segment_type, location);
        }
    }
    pub fn into_iter(self) -> impl Iterator<Item=SymbolDefinition> {
        self.0.into_iter()
    }
}

impl Section for SymbolTable {
    const NAME: &'static str = "_syms";

    fn read<R: Read>(reader: R) -> io::Result<Self> {
        let mut symbols = Vec::new();
        let mut reader = BufReader::new(reader);

        loop {
            let mut namebuf = Vec::new();
            match reader.read_until(0, &mut namebuf)? {
                0 => break,
                _ => ()
            }
            namebuf.pop();

            let mut buf = [0; 3];
            reader.read_exact(&mut buf)?;
            let [stype, ol, oh] = buf;

            let segment_type = segment_type_from_u8(stype)?;

            let is_global = namebuf[0] != b' ';
            let name;
            if is_global {
                name = String::from_utf8_lossy(&namebuf).into();
            } else {
                name = String::from_utf8_lossy(&namebuf[1..]).into();
            }

            let def = SymbolDefinition {
                name,
                is_global,
                segment_type,
                location: u16::from_le_bytes([ol, oh]),
            };
            symbols.push(def);
        }

        Ok(SymbolTable(symbols))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        for &SymbolDefinition { ref name, is_global, segment_type, location } in &self.0 {
            write!(writer, "{}{name}\0", if is_global { "" } else { " " })?;
            writer.write_all(&[segment_type as u8])?;
            writer.write_all(&location.to_le_bytes())?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RelocationEntry {
    pub reference_segment: SegmentType,
    pub reference_location: u16,
    pub symbol_index: u16,
    // Future perhaps a format field again
}
#[derive(Debug, Clone, Default)]
pub struct RelocationTable(pub Vec<RelocationEntry>);

impl Section for RelocationTable {
    const NAME: &'static str = "_reloc";

    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut entries = Vec::new();

        loop {
            let mut buf = [0; 5];
            match reader.read_exact(&mut buf) {
                Ok(()) => (),
                Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
                    break;
                }
                Err(e) => return Err(e),
            }
            let [stype, ol1, oh1, ol2, oh2] = buf;

            let reference_segment = segment_type_from_u8(stype)?;
            let reference_location = u16::from_le_bytes([ol1, oh1]);
            let symbol_index = u16::from_le_bytes([ol2, oh2]);

            let entry = RelocationEntry {
                reference_segment,
                reference_location,
                symbol_index,
            };
            entries.push(entry)
        }

        Ok(RelocationTable(entries))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        for &RelocationEntry { reference_segment, reference_location, symbol_index } in &self.0 {
            writer.write_all(&[reference_segment as u8])?;
            writer.write_all(&reference_location.to_le_bytes())?;
            writer.write_all(&symbol_index.to_le_bytes())?;
        }
        Ok(())
    }
}

fn segment_type_from_u8(n: u8) -> io::Result<SegmentType> {
    SegmentType::try_from(n).map_err(|()| io::Error::new(io::ErrorKind::InvalidData, "unrecognised segment type"))
}
