use std::{fmt::Display, iter, mem};

use super::{SourceLocation, Result as SourceResult, Error, ErrorType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Absolute,
    Big,
}

pub struct LabelRead {
    pub position: u16,
    pub format: Format,
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

pub struct Symbols {
    labels: Vec<Box<str>>,
    id_to_pos: Vec<Result<u16, Vec<SourceLocation>>>,
    symbol_types: Vec<SymbolType>,
}

impl Symbols {
    pub fn new() -> Self {
        Self { labels: Vec::new(), symbol_types: Vec::new(), id_to_pos: Vec::new() }
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
    pub fn set_label(&mut self, lbl: &str, pos: u16, loc: SourceLocation) -> SourceResult<()> {
        let id = self.find_id(lbl);
        match mem::replace(&mut self.id_to_pos[id], Ok(pos)) {
            Ok(p) => {
                Err(Error::new(loc.source, loc.line_number, ErrorType::Other(
                    format!("Label {lbl} already had pos {p:03x} but is now being set to {pos:03x}").into_boxed_str()
                )))
            }
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
            self.symbol_types.resize(id+1, SymbolType::default());
        }
        self.symbol_types[id].set_global();
    }
    pub fn set_reference(&mut self, id: usize) {
        if id >= self.symbol_types.len() {
            self.symbol_types.resize(id+1, SymbolType::default());
        }
        self.symbol_types[id].set_reference();
    }
    pub fn size(&self) -> usize {
        self.labels.len()
    }
    pub fn into_iter(self) -> impl Iterator<Item=(Box<str>, SymbolType, Result<u16, Vec<SourceLocation>>)> {
        self.labels.into_iter().zip(self.symbol_types.into_iter().chain(iter::repeat(SymbolType::default())).zip(self.id_to_pos))
            .map(|(a, (b, c))| (a, b, c))
    }

    /// Marker that indicates the state of `self` used for `mangle_interal` to mangle
    /// non-global symbols that have been since this marker
    pub fn marker(&self) -> Marker {
        Marker { size: self.size() }
    }

    pub fn mangle_interal(&mut self, src: impl Display, old_label_marker: Marker) {
        for (id, lbl) in self.labels.iter_mut().enumerate().skip(old_label_marker.size) {
            // mangle interal symbols
            if let SymbolType::Internal = self.symbol_types.get(id).unwrap_or(&SymbolType::default()) {
                *lbl = format!("{src}  {lbl}").into_boxed_str();
            };
        }
    }
    // fn is_global(&self, id: usize) -> bool {
    //     self.globals.get(id).copied().unwrap_or(false)
    // }
}

pub struct Marker {
    size: usize,
}
