use std::{
    fmt::{self, Display},
    iter, mem,
};

use crate::aalv::obj::SegmentType;

use super::{Error, ErrorType, Result as SourceResult, SourceLocation};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(super) struct Address(pub SegmentType, pub u16);

impl Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}+0x{:02x}>", self.0, self.1)
    }
}

pub struct LabelRead {
    pub segment: SegmentType,
    pub position: u16,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(u8)]
pub enum SymbolType {
    #[default]
    Internal,
    Global,
    Reference,
}
impl SymbolType {
    fn set_global(&mut self) {
        use self::SymbolType::*;
        match *self {
            Internal => *self = Global,
            Reference => (),
            Global => (),
        }
    }
    fn set_reference(&mut self) {
        use self::SymbolType::*;
        match *self {
            Internal => *self = Reference,
            Global => *self = Reference,
            Reference => (),
        }
    }
}

pub(super) struct Symbols {
    labels: Vec<Box<str>>,
    id_to_pos: Vec<Result<Address, Vec<SourceLocation>>>,
    symbol_types: Vec<SymbolType>,
}

impl Symbols {
    pub fn new() -> Self {
        Self {
            labels: Vec::new(),
            symbol_types: Vec::new(),
            id_to_pos: Vec::new(),
        }
    }
    fn find_id(&mut self, lbl: &str) -> usize {
        if let Some(i) = self.labels.iter().position(|l| &**l == lbl) {
            i
        } else {
            let i = self.labels.len();
            self.labels.push(lbl.to_owned().into_boxed_str());
            self.id_to_pos.push(Err(Vec::new()));
            i
        }
    }
    pub fn set_label(&mut self, lbl: &str, addr: Address, loc: SourceLocation) -> SourceResult<()> {
        let id = self.find_id(lbl);

        match mem::replace(&mut self.id_to_pos[id], Ok(addr)) {
            Ok(cur_addr) => Err(Error::new(
                loc.source,
                loc.line_number,
                ErrorType::Other(
                    format!("Label {lbl} already had {cur_addr} but is now being set to {addr}")
                        .into_boxed_str(),
                ),
            )),
            Err(_) => Ok(()),
        }
    }
    pub fn get_label(&mut self, lbl: &str, loc: SourceLocation) -> usize {
        let id = self.find_id(lbl);
        match &mut self.id_to_pos[id] {
            Ok(_) => (),
            Err(v) => v.push(loc),
        }

        id
    }
    pub fn set_global(&mut self, id: usize) {
        if id >= self.symbol_types.len() {
            self.symbol_types.resize(id + 1, SymbolType::default());
        }
        self.symbol_types[id].set_global();
    }
    pub fn set_reference(&mut self, id: usize) {
        if id >= self.symbol_types.len() {
            self.symbol_types.resize(id + 1, SymbolType::default());
        }
        self.symbol_types[id].set_reference();
    }
    pub fn size(&self) -> usize {
        self.labels.len()
    }
    pub fn into_iter(
        self,
    ) -> impl Iterator<Item = (Box<str>, SymbolType, Result<Address, Vec<SourceLocation>>)> {
        self.labels
            .into_iter()
            .zip(
                self.symbol_types
                    .into_iter()
                    .chain(iter::repeat(SymbolType::default()))
                    .zip(self.id_to_pos),
            )
            .map(|(a, (b, c))| (a, b, c))
    }
}
