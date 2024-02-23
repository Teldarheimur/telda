use std::{
    collections::BTreeMap,
    fmt::{self, Display},
    io::{self, BufRead, Seek},
    path::Path,
};

use super::{read_aalv_file, write_aalv_file_with_offset, AalvReader, Section};

mod sec_impl;

pub const AALV_OBJECT_EXT: &str = "to";

#[derive(Debug, Default)]
pub struct Object {
    pub file_offset: u64,
    pub entry: Option<Entry>,
    pub flags: Option<Flags>,
    pub stack_size: Option<StackSize>,
    pub heap_size: Option<HeapSize>,
    pub segs: BTreeMap<SegmentType, (u16, Vec<u8>)>,
    pub symbols: SymbolTable,
    pub relocation_table: RelocationTable,
}

impl Object {
    pub fn from_aalv_reader<F: BufRead + Seek>(aalvur: &mut AalvReader<F>) -> io::Result<Self> {
        let mut segs = BTreeMap::new();

        while let Some(seg) = aalvur.read_section() {
            let BinarySegment {
                offset,
                stype,
                bytes,
            } = seg?;

            if segs.insert(stype, (offset, bytes)).is_some() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "duplicate segment type",
                ));
            }
        }

        let obj = Object {
            file_offset: aalvur.file_offset,
            entry: aalvur.read_section().transpose()?,
            flags: aalvur.read_section().transpose()?,
            stack_size: aalvur.read_section().transpose()?,
            heap_size: aalvur.read_section().transpose()?,
            segs,
            symbols: aalvur
                .read_section()
                .transpose()?
                .unwrap_or_else(|| SymbolTable(Vec::new())),
            relocation_table: aalvur
                .read_section()
                .transpose()?
                .unwrap_or_else(|| RelocationTable(Vec::new())),
        };

        if aalvur.remaing_sections().any(|s| s.starts_with('_')) {
            unimplemented!("error unexpected sections")
        } else {
            Ok(obj)
        }
    }
}

impl Object {
    #[inline]
    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        Self::from_aalv_reader(&mut read_aalv_file(path)?)
    }
    pub fn zero_offset(self) -> Self {
        Self {
            file_offset: 0,
            ..self
        }
    }
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let Object {
            file_offset,
            entry,
            flags,
            stack_size,
            heap_size,
            segs,
            symbols,
            relocation_table,
        } = self;

        let mut aalvur = write_aalv_file_with_offset(path, *file_offset)?;

        if let Some(entry) = entry {
            aalvur.write_section(entry)?;
        }
        if let Some(flags) = flags {
            aalvur.write_section(flags)?;
        }
        if let Some(stack_size) = stack_size {
            aalvur.write_section(stack_size)?;
        }
        if let Some(heap_size) = heap_size {
            aalvur.write_section(heap_size)?;
        }
        for (&stype, &(offset, ref bytes)) in segs {
            aalvur.write_section(&BinarySegment {
                stype,
                offset,
                bytes: bytes.clone(),
            })?;
        }
        if !symbols.0.is_empty() {
            aalvur.write_section(symbols)?;
        }
        if !relocation_table.0.is_empty() {
            aalvur.write_section(relocation_table)?;
        }

        Ok(())
    }

    #[deprecated = "Load segmented instead"]
    pub fn get_flattened_memory(&self) -> Vec<u8> {
        let size = self
            .segs
            .iter()
            .map(|(_, &(o, ref v))| o as usize + v.len())
            .max()
            .unwrap_or(0);
        let mut vec = vec![0; size];

        for &(offset, ref bytes) in self.segs.values() {
            vec[offset as usize..offset as usize + bytes.len()].copy_from_slice(bytes);
        }

        vec
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Entry(pub SegmentType, pub u16);

#[derive(Debug, Clone, Copy)]
pub struct Flags {
    pub readable_text: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Flags {
            readable_text: false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StackSize(pub u16);
impl Default for StackSize {
    fn default() -> Self {
        Self(0x180)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeapSize(pub u16);
impl Default for HeapSize {
    fn default() -> Self {
        Self(0x400)
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
    // new layout:
    // zero (), text (x), rodata (r), data (rw), heap (rw)
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
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinarySegment {
    pub offset: u16,
    pub stype: SegmentType,
    pub bytes: Vec<u8>,
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
    pub fn mutate<F: FnMut(&mut Box<str>, &mut bool, &mut SegmentType, &mut u16)>(
        &mut self,
        mut f: F,
    ) {
        for SymbolDefinition {
            name,
            is_global,
            segment_type,
            location,
        } in &mut self.0
        {
            f(name, is_global, segment_type, location);
        }
    }
    pub fn iter(&self) -> impl Iterator<Item=&SymbolDefinition> {
        self.0.iter()
    }
}

impl IntoIterator for SymbolTable {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = SymbolDefinition;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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

fn segment_type_from_u8(n: u8) -> io::Result<SegmentType> {
    SegmentType::try_from(n)
        .map_err(|()| io::Error::new(io::ErrorKind::InvalidData, "unrecognised segment type"))
}
